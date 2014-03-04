/* qparser.cc - Notmuch of a query parser
 *
 * Copyright © 2014 Austin Clements
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Austin Clements <aclements@csail.mit.edu>
 */

#include "qparser.h"
#include "notmuch-private.h"

#include <vector>

// XXX Exact term wildcards
// XXX label -> field (more sense for user documentation)

using Xapian::Unicode::is_whitespace;
using Xapian::Unicode::is_wordchar;

/*
 * Query AST nodes
 */

static const char *qnode_type_names[] = {
    "AND", "OR", "NOT", "LABEL", "GROUP", "TERMS", "QUERY",
};

static int
qnode_destructor (_notmuch_qnode_t *tok)
{
    tok->query.~Query ();
    return 0;
}

_notmuch_qnode_t *
_notmuch_qnode_create (const void *ctx, enum _notmuch_qnode_type type,
		       const char **error_out)
{
    _notmuch_qnode_t *node = talloc_zero (ctx, _notmuch_qnode_t);
    if (! node) {
	if (! *error_out)
	    *error_out = "Out of memory allocating qnode";
	return NULL;
    }
    node->type = type;
    new (&node->query) Xapian::Query();
    talloc_set_destructor (node, qnode_destructor);
    return node;
}

void
_notmuch_qnode_add_child (_notmuch_qnode_t *parent,
			  _notmuch_qnode_t *child,
			  const char **error_out)
{
    if (parent->nchild == talloc_array_length (parent->child)) {
	size_t new_count = parent->nchild * 2;
	if (new_count == 0)
	    new_count = 2;
	_notmuch_qnode_t **new_child =
	    talloc_realloc (parent, parent->child, _notmuch_qnode_t *,
			    new_count);
	if (! new_child) {
	    if (! *error_out)
		*error_out = "Out of memory allocating child qnode array";
	    return;
	}
	parent->child = new_child;
    }
    parent->child[parent->nchild++] = child;
}

const char *
_notmuch_qnode_to_string (const void *ctx, _notmuch_qnode_t *node)
{
    if (! node) {
	return talloc_strdup (ctx, "<nil>");
    } else if (node->type == NODE_TERMS) {
	return talloc_asprintf (ctx, "\"%s\"", node->text);
    } else if (node->type == NODE_QUERY) {
	return talloc_asprintf (ctx, "QUERY/%s",
				node->query.get_description ().c_str () +
				strlen ("Xapian::Query"));
    } else {
	void *local = talloc_new (ctx);
	char *out = talloc_asprintf (ctx, "(%s", qnode_type_names[node->type]);
	if (node->text)
	    out = talloc_asprintf_append_buffer (out, "/%s", node->text);
	for (size_t i = 0; i < node->nchild; ++i)
	    out = talloc_asprintf_append_buffer (
		out, " %s",
		_notmuch_qnode_to_string (local, node->child[i]));
	out = talloc_strdup_append_buffer (out, ")");
	talloc_free (local);
	return out;
    }
}

/*
 * Query node construction
 */

static Xapian::Query
expand_wildcard (Xapian::Database db, const char *term, size_t limit,
		 const char **error_out)
{
    std::vector<Xapian::Query> qs;
    Xapian::TermIterator i = db.allterms_begin (term),
	end = db.allterms_end (term);

    for (; i != end; i++) {
	if (limit && qs.size () == limit) {
	    if (! *error_out)
		*error_out = "Wildcard expands to too many terms; please be more specific";
	    return Xapian::Query ();
	}

	qs.push_back (Xapian::Query (*i));
    }
    if (qs.empty ())
	return Xapian::Query::MatchNothing;
    return Xapian::Query (Xapian::Query::OP_SYNONYM, qs.begin (), qs.end ());
}

_notmuch_qnode_t *
_notmuch_qparser_make_literal_query (
    const void *ctx, const char *text, const char *db_prefix,
    const char **error_out)
{
    _notmuch_qnode_t *node = _notmuch_qnode_create (ctx, NODE_QUERY, error_out);
    if (! node)
	return NULL;
    std::string db_term (db_prefix ? db_prefix : "");
    /* This test is compatible with Xapian's prefix_needs_colon.  It's
     * important that we don't use isupper here, since that's
     * locale-specific. */
    if (db_prefix && db_prefix[0] && db_prefix[1] &&
	text[0] >= 'A' && text[0] <= 'Z')
	db_term += ':';
    db_term += text;
    node->query = Xapian::Query (db_term);
    return node;
}

_notmuch_qnode_t *
_notmuch_qparser_make_text_query (
    const void *ctx, const char *text, bool quoted,
    _notmuch_qparser_text_options_t *options, const char **error_out)
{
    _notmuch_qnode_t *node = _notmuch_qnode_create (ctx, NODE_QUERY, error_out);
    if (! node)
	return NULL;

    /* As a special case of wildcard expansion, a bare * expands to
     * everything. */
    if (options->wildcard && ! quoted && strcmp (text, "*") == 0) {
	if (options->db_prefix) {
	    node->query = expand_wildcard (*options->db, options->db_prefix,
					   options->wildcard_limit, error_out);
	    if (node->query.empty ())
		return NULL;
	} else {
	    node->query = Xapian::Query::MatchAll;
	}
	return node;
    }

    /* Use the term generator to split text.  (We use the Utf8Iterator
     * version of index_text to avoid copying through std::string.) */
    Xapian::TermGenerator tgen (*options->tgen);
    Xapian::Document doc;
    tgen.set_document (doc);
    tgen.set_termpos (0);
    if (options->db_prefix)
	tgen.index_text (Xapian::Utf8Iterator (text), 1, options->db_prefix);
    else
	tgen.index_text (Xapian::Utf8Iterator (text));

    /* Unfortunately, there's no direct way to ask TermGenerator for
     * the list of terms it split text into, so we have to walk over
     * the terms in the Document that index_text populated. */
    size_t nterms = tgen.get_termpos();
    std::vector<Xapian::Query> qs (nterms);
    Xapian::TermIterator it, end;
    Xapian::PositionIterator pit, pend;
    std::string single, stemmed;
    for (it = doc.termlist_begin (), end = doc.termlist_end ();
	 it != end; ++it) {
	/* Skip unstemmed terms */
	if ((*it).length () && (*it)[0] == 'Z') {
	    if (nterms == 1 && ! quoted)
		stemmed = *it;
	    continue;
	}
	/* If there's just one term, we may need it later */
	if (nterms == 1 && single.empty ())
	    single = *it;
	/* Put terms in their positions in the query */
	for (pit = it.positionlist_begin (), pend = it.positionlist_end ();
	     pit != pend; ++pit)
	    qs[*pit - 1] = Xapian::Query (*it);
    }

    /* Perform wildcard expansion */
    if (options->wildcard && ! quoted && nterms == 1 &&
	text[strlen (text) - 1] == '*') {
	qs[0] = expand_wildcard (*options->db, single.c_str (),
				 options->wildcard_limit, error_out);
	if (qs[0].empty ())
	    return NULL;
	/* Inhibit stemming */
	stemmed = std::string ();
    }

    /* If the term generator was configured to stem, this was not
     * quoted or a phrase, and we found a stemmed term, use it.
     * Xapian further restricts this to terms that don't start with a
     * lower case letter (or a few other Unicode classes) and that
     * aren't followed by various "stem preventers".
     */
    if (! stemmed.empty ())
	qs[0] = Xapian::Query (stemmed);

    /* Build query */
    if (nterms == 0)
	node->query = Xapian::Query ();
    else if (nterms == 1)
	node->query = qs[0];
    else
	node->query = Xapian::Query (Xapian::Query::OP_PHRASE,
				     qs.begin (), qs.end (), qs.size ());
    return node;
}

/*
 * Transformation
 */

struct _label_transform_state
{
    const char *label;
    _notmuch_qparser_label_transformer *cb;
    void *opaque;
    const char **error_out;
};

static _notmuch_qnode_t *
label_transform_rec (struct _label_transform_state *s, _notmuch_qnode_t *node,
		     bool active)
{
    if (*s->error_out)
	return node;
    /* XXX Should it be an error to have clashing labels?  E.g.,
     * foo:(bar:x)? */
    if (node->type == NODE_LABEL)
	active = s->label && (strcmp (node->text, s->label) == 0);
    else if (active && node->type == NODE_TERMS)
	return s->cb (node, s->opaque, s->error_out);
    for (size_t i = 0; i < node->nchild; i++)
	node->child[i] = label_transform_rec (s, node->child[i], active);
    if (active && node->type == NODE_LABEL)
	return node->child[0];
    return node;
}

_notmuch_qnode_t *
_notmuch_qparser_label_transform (_notmuch_qnode_t *node, const char *label,
				  _notmuch_qparser_label_transformer *cb,
				  void *opaque, const char **error_out)
{
    struct _label_transform_state state = {label, cb, opaque, error_out};
    return label_transform_rec (&state, node, label == NULL);
}

struct _literal_prefix_state
{
    const char *label, *db_prefix;
    bool exclusive;
};

static _notmuch_qnode_t *
literal_prefix_cb (_notmuch_qnode_t *terms, void *opaque, const char **error_out)
{
    struct _literal_prefix_state *state = (struct _literal_prefix_state*)opaque;
    _notmuch_qnode_t *q = _notmuch_qparser_make_literal_query (
	terms, terms->text, state->db_prefix, error_out);
    if (state->exclusive && q)
	q->conj_class = state->label;
    return q;
}

_notmuch_qnode_t *
_notmuch_qparser_literal_prefix (_notmuch_qnode_t *node, const char *label,
				 const char *db_prefix, bool exclusive,
				 const char **error_out)
{
    struct _literal_prefix_state state = {label, db_prefix, exclusive};
    return _notmuch_qparser_label_transform (node, label, literal_prefix_cb,
					     &state, error_out);
}

static _notmuch_qnode_t *
text_prefix_cb (_notmuch_qnode_t *terms, void *opaque, const char **error_out)
{
    return _notmuch_qparser_make_text_query (
	terms, terms->text, terms->quoted,
	(_notmuch_qparser_text_options_t *)opaque, error_out);
}

_notmuch_qnode_t *
_notmuch_qparser_text_prefix (_notmuch_qnode_t *node, const char *label,
			      _notmuch_qparser_text_options_t *options,
			      const char **error_out)
{
    return _notmuch_qparser_label_transform (node, label, text_prefix_cb,
					     options, error_out);
}

/*
 * Generator
 */

struct _generate_state
{
    const void *ctx, *local;
    const char *error;
};

static _notmuch_qnode_t *
generate_group (struct _generate_state *s, _notmuch_qnode_t *node)
{
    /* Turn the group into a NODE_AND, but with children in the same
     * conjunction class OR'd. */
    _notmuch_qnode_t *sub =
	_notmuch_qnode_create (s->local, NODE_AND, &s->error);
    if (! sub)
	return NULL;
    for (size_t i = 0; i < node->nchild; ++i) {
	_notmuch_qnode_t *child = node->child[i];
	if (! child->conj_class) {
	    _notmuch_qnode_add_child (sub, child, &s->error);
	} else {
	    size_t j;
	    for (j = 0; j < sub->nchild; ++j) {
		if (sub->child[j]->conj_class &&
		    strcmp (child->conj_class, sub->child[j]->conj_class) == 0) {
		    _notmuch_qnode_add_child (sub->child[j], child, &s->error);
		    break;
		}
	    }
	    if (j == sub->nchild) {
		_notmuch_qnode_t *conj =
		    _notmuch_qnode_create (s->local, NODE_OR, &s->error);
		if (! conj)
		    return NULL;
		conj->conj_class = child->conj_class;
		_notmuch_qnode_add_child (sub, conj, &s->error);
		_notmuch_qnode_add_child (conj, child, &s->error);
	    }
	}
    }
    return sub;
}

static Xapian::Query
generate (struct _generate_state *s, _notmuch_qnode_t *node)
{
    using Xapian::Query;
    Query l, r;

    if (s->error)
	return Query ();
    if (! node)
	INTERNAL_ERROR ("NULL node in qparser AST");

    /* Translate this node to a query.  Be careful because any
     * recursive generate call can return an empty query. */
    switch (node->type) {
    case NODE_AND:
	for (size_t i = 0; i < node->nchild; ++i) {
	    Query::op op = Query::OP_AND;
	    if (l.empty ()) {
		l = generate (s, node->child[i]);
	    } else if (node->child[i]->type == NODE_NOT) {
		r = generate (s, node->child[i]->child[0]);
		op = Query::OP_AND_NOT;
	    } else {
		r = generate (s, node->child[i]);
	    }
	    if (! r.empty())
		l = Query (op, l, r);
	}
	return l;

    case NODE_OR:
	for (size_t i = 0; i < node->nchild; ++i) {
	    r = generate (s, node->child[i]);
	    if (l.empty ())
		l = r;
	    else if (! r.empty())
		l = Query (Query::OP_OR, l, r);
	}
	return l;

    case NODE_NOT:
	if (node->child[0]->type == NODE_NOT)
	    return generate (s, node->child[0]->child[0]);
	l = generate (s, node->child[0]);
	if (l.empty ())
	    return l;
	return Query (Query::OP_AND_NOT, Query::MatchAll, l);

    case NODE_LABEL:
	/* Transformers have stripped out all known labels. */
	if (! s->error)
	    s->error = talloc_asprintf (
		s->ctx, "Unknown label '%s' in query", node->text);
	return Query ();

    case NODE_GROUP:
	/* XXX Currently we don't distinguish weighted and
	 * non-weighted parts of the query.  Xapian uses FILTER or
	 * SCALE_WEIGHT*0 for any boolean-prefixed terms in a prob.
	 * We should do this, too, but should probably handle it
	 * generically in NODE_AND. */
	return generate (s, generate_group (s, node));

    case NODE_TERMS:
	if (! s->error)
	    s->error = "Internal error: NODE_TERMS in qparser AST";
	return Query ();

    case NODE_QUERY:
	return node->query;
    }
    INTERNAL_ERROR ("Illegal qnode %s in IR",
		    _notmuch_qnode_to_string (s->local, node));
}

Xapian::Query
_notmuch_qparser_generate (const void *ctx, _notmuch_qnode_t *root,
			   const char **error_out)
{
    // XXX I should either clear *error_out, or I should bail
    // immediately if *error_out.
    void *local = talloc_new (ctx);
    struct _generate_state state = {ctx, local, NULL};
    Xapian::Query query = generate (&state, root);
    talloc_free (local);
    if (state.error) {
	if (! *error_out)
	    *error_out = state.error;
	return Xapian::Query ();
    }
    return query;
}
