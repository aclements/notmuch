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

using Xapian::Unicode::is_whitespace;
using Xapian::Unicode::is_wordchar;

static const char *qnode_type_names[] = {
    "LOVE", "HATE", "BRA", "KET",
    "AND", "OR",
    "NOT", "PREFIX",
    "GROUP",
    "TERMS", "QUERY", "END"
};

static int
qnode_destructor (_notmuch_qnode_t *tok)
{
    tok->query.~Query ();
    return 0;
}

_notmuch_qnode_t *
_notmuch_qnode_create (const void *ctx, enum _notmuch_qnode_type type,
		       const char *text, const char **error_out)
{
    _notmuch_qnode_t *node = talloc (ctx, _notmuch_qnode_t);
    if (! node) {
	if (! *error_out)
	    *error_out = "Out of memory allocating qnode";
	return NULL;
    }
    node->type = type;
    node->text = text;
    new (&node->query) Xapian::Query();
    node->next = NULL;
    node->nchild = 0;
    node->child = NULL;
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

static const char *
qnode_to_string (const void *ctx, _notmuch_qnode_t *node)
{
    if ((unsigned)node->type > TOK_END)
	return talloc_asprintf (ctx, "<bad type %d>", node->type);

    if (node->type == NODE_TERMS)
	return talloc_asprintf (ctx, "TERMS/\"%s\"", node->text);
    else if (node->type == NODE_PREFIX)
	return talloc_asprintf (ctx, "PREFIX/%s", node->text);
    else if (node->type == NODE_QUERY)
	return talloc_asprintf (ctx, "QUERY/%s",
				node->query.get_description ().c_str ());

    return qnode_type_names[node->type];
}

__attribute__((unused))
static char *
qnode_list_to_string (const void *ctx, _notmuch_qnode_t *node)
{
    void *local = talloc_new (ctx);
    char *out = talloc_strdup (ctx, "");

    for (; node->type != TOK_END; node = node->next) {
	const char *t = qnode_to_string (local, node);
	out = talloc_asprintf_append_buffer (
	    out, "%s%s", *out == 0 ? "" : " ", t);
    }

    talloc_free (local);
    return out;
}

const char *
_notmuch_qnode_tree_to_string (const void *ctx, _notmuch_qnode_t *node)
{
    if (!node) {
	return talloc_strdup (ctx, "<nil>");
    } else if (node->type == NODE_TERMS || node->type == NODE_QUERY) {
	return qnode_to_string (ctx, node);
    } else {
	void *local = talloc_new (ctx);
	char *out = talloc_asprintf (ctx, "(%s", qnode_to_string (local, node));
	for (size_t i = 0; i < node->nchild; ++i)
	    out = talloc_asprintf_append_buffer (
		out, " %s",
		_notmuch_qnode_tree_to_string (local, node->child[i]));
	out = talloc_strdup_append_buffer (out, ")");
	talloc_free (local);
	return out;
    }
}

/*
 * Transformation
 */

/**
 * Return a query that matches a single, literal term with the given
 * text and database prefix (a "boolean prefix" in Xapian lingo).
 */
_notmuch_qnode_t *
_notmuch_qparser_make_literal_query (
    const void *ctx, const char *text, const char *db_prefix,
    const char **error_out)
{
    _notmuch_qnode_t *node =
	_notmuch_qnode_create (ctx, NODE_QUERY, NULL, error_out);
    if (! node)
	return NULL;
    std::string db_term (db_prefix);
    if (*db_prefix && *(db_prefix + 1) && isupper ((unsigned char) text[0]))
	db_term += ':';
    db_term += text;
    node->query = Xapian::Query (db_term);
    return node;
}

/**
 * Return a query that matches the given free-text, prefixed with the
 * given database prefix (a "probabilistic prefix" in Xapian lingo).
 * The text will be split into individual terms using the provided
 * TermGenerator, which can be configured for stemming and stopping.
 * If there are no terms in text, returns Query ().
 */
_notmuch_qnode_t *
_notmuch_qparser_make_text_query (
    const void *ctx, const char *text, const char *db_prefix,
    Xapian::TermGenerator tgen, const char **error_out)
{
    _notmuch_qnode_t *node =
	_notmuch_qnode_create (ctx, NODE_QUERY, NULL, error_out);
    if (! node)
	return NULL;

    /* Use the term generator to split text.  (We use the Utf8Iterator
     * version of index_text to avoid copying through std::string.) */
    Xapian::Document doc;
    tgen.set_document (doc);
    tgen.set_termpos (0);
    if (db_prefix)
	tgen.index_text (Xapian::Utf8Iterator (text), 1, db_prefix);
    else
	tgen.index_text (Xapian::Utf8Iterator (text));

    /* Unfortunately, there's no direct way to ask TermGenerator for
     * the list of terms it split text into, so we have to walk over
     * the terms in the Document that index_text populated. */
    size_t nterms = tgen.get_termpos();
    Xapian::Query *qs = new Xapian::Query[nterms];
    Xapian::TermIterator it, end;
    Xapian::PositionIterator pit, pend;
    for (it = doc.termlist_begin (), end = doc.termlist_end ();
	 it != end; ++it) {
	for (pit = it.positionlist_begin (), pend = it.positionlist_end ();
	     pit != pend; ++pit)
	    qs[*pit - 1] = Xapian::Query (*it);
    }

    /* Build query */
    if (nterms == 0)
	node->query = Xapian::Query ();
    else if (nterms == 1)
	node->query = qs[0];
    else
	node->query = Xapian::Query (Xapian::Query::OP_PHRASE,
				     qs, qs + nterms, nterms);
    delete[] qs;
    return node;
}

/**
 * A prefix transformer callback, which will be passed a NODE_TERMS
 * token to which the desired prefix applies.  This must either return
 * a transformed token, or return NULL and set *error_out to an error
 * message.
 */
typedef _notmuch_qnode_t *_notmuch_qparser_prefix_transformer (
    _notmuch_qnode_t *terms, void *opaque, const char **error_out);

struct _prefix_transform_state
{
    const char *prefix;
    _notmuch_qparser_prefix_transformer *cb;
    void *opaque;
    const char **error_out;
};

static _notmuch_qnode_t *
prefix_transform_rec (struct _prefix_transform_state *s, _notmuch_qnode_t *node,
		      bool active)
{
    if (*s->error_out)
	return node;
    /* XXX Should it be an error to have clashing prefixes?  E.g.,
     * foo:(bar:x)? */
    if (node->type == NODE_PREFIX)
	active = (strcmp (node->text, s->prefix) == 0);
    else if (active && node->type == NODE_TERMS)
	return s->cb (node, s->opaque, s->error_out);
    for (size_t i = 0; i < node->nchild; i++)
	node->child[i] = prefix_transform_rec (s, node->child[i], active);
    if (active && node->type == NODE_PREFIX)
	return node->child[0];
    return node;
}

/**
 * Transform all terms that have the given prefix in the query rooted
 * at node using the provided transformer.  In effect, this finds all
 * NODE_TERMS tokens in sub-queries under the given prefix, invokes the
 * callback for all these NODE_TERMS tokens, and strips the prefix
 * token itself from the query.
 */
_notmuch_qnode_t *
_notmuch_qparser_prefix_transform (_notmuch_qnode_t *node, const char *prefix,
				   _notmuch_qparser_prefix_transformer *cb,
				   void *opaque, const char **error_out)
{
    struct _prefix_transform_state state = {prefix, cb, opaque, error_out};
    return prefix_transform_rec (&state, node, false);
}

struct _literal_prefix_state
{
    const char *prefix, *db_prefix;
    bool exclusive;
};

static _notmuch_qnode_t *
literal_prefix_cb (_notmuch_qnode_t *terms, void *opaque, const char **error_out)
{
    struct _literal_prefix_state *state = (struct _literal_prefix_state*)opaque;
    _notmuch_qnode_t *q = _notmuch_qparser_make_literal_query (
	terms, terms->text, state->db_prefix, error_out);
    if (state->exclusive && q)
	q->conj_class = state->prefix;
    return q;
}

_notmuch_qnode_t *
_notmuch_qparser_literal_prefix (_notmuch_qnode_t *node, const char *prefix,
				 const char *db_prefix, bool exclusive,
				 const char **error_out)
{
    struct _literal_prefix_state state = {prefix, db_prefix, exclusive};
    return _notmuch_qparser_prefix_transform (node, prefix, literal_prefix_cb,
					      &state, error_out);
}

struct _text_prefix_state
{
    const char *prefix, *db_prefix;
    Xapian::TermGenerator tgen;
};

static _notmuch_qnode_t *
text_prefix_cb (_notmuch_qnode_t *terms, void *opaque, const char **error_out)
{
    struct _text_prefix_state *state = (struct _text_prefix_state*)opaque;
    return _notmuch_qparser_make_text_query (
	terms, terms->text, state->db_prefix, state->tgen, error_out);
}

_notmuch_qnode_t *
_notmuch_qparser_text_prefix (_notmuch_qnode_t *node, const char *prefix,
			      const char *db_prefix, Xapian::TermGenerator tgen,
			      const char **error_out)
{
    struct _text_prefix_state state = {prefix, db_prefix, tgen};
    return _notmuch_qparser_prefix_transform (node, prefix, text_prefix_cb,
					      &state, error_out);
}



// struct _notmuch_qparser
// {
//     GPtrArray *transforms;
// };

// // XXX If the caller just applies transformations, then I don't need a
// // first-class representation.  For general transformations, it can
// // just apply the function to the root.  For prefix transformations, I
// // can have a simple "apply this prefix transform function" function
// // (with a closure, bleh).

// typedef struct _notmuch_qparser_transform
// {
//     /**
//      * Callback to transform a parse tree.  Must return the new root
//      * of the tree.
//      */
//     _notmuch_qnode_t *(*transform) (struct _notmuch_qparser_transform *self,
// 				  _notmuch_qnode_t *root);
// } _notmuch_qparser_transform_t;

// typedef struct _notmuch_prefix_transform
// {
//     /**
//      * Callback to transform a NODE_TERMS token that this prefix
//      * applies to.
//      */
//     _notmuch_qnode_t *(*transform) (struct _notmuch_prefix_transform *self,
// 				  _notmuch_qnode_t *node);
// } _notmuch_prefix_transform_t;

// struct _notmuch_prefix_adapter
// {
//     _notmuch_qparser_transform_t transform;
//     const char *prefix;
//     _notmuch_prefix_transform *ptransform;
// };

// static _notmuch_qnode_t *
// prefix_transform_rec (struct _notmuch_prefix_adapter *self,
// 		      _notmuch_qnode_t *node, bool active)
// {
//     if (node->type == NODE_PREFIX)
// 	active = (strcmp (node->text, self->prefix) == 0);
//     else if (active && node->type == NODE_TERMS)
// 	return self->ptransform->transform (self->ptransform, node);
//     for (size_t i = 0; i < node->nchild; i++)
// 	node->child[i] = prefix_transform_rec (self, node->child[i], active);
//     if (active && node->type == NODE_PREFIX)
// 	return node->child[0];
//     return node;
// }

// static _notmuch_qnode_t *
// prefix_transform (_notmuch_qparser_transform_t *self, _notmuch_qnode_t *root)
// {
//     return prefix_transform_rec ((struct _notmuch_prefix_adapter*)self,
// 				 root, false);
// }

// _notmuch_qparser_transform_t *
// _notmuch_qparser_prefix_transform_create (
//     const void *ctx, const char *prefix, _notmuch_prefix_transform_t *transform)
// {
//     _notmuch_prefix_adapter *t = talloc (ctx, _notmuch_prefix_adapter);
//     if (! t)
// 	return NULL;
//     t->transform.transform = prefix_transform;
//     t->prefix = prefix;
//     t->ptransform = transform;
//     return &t->transform;
// }

// struct _notmuch_boolean_transform
// {
//     _notmuch_prefix_transform_t prefix_transform;
//     const char *db_prefix;
//     bool exclusive;
// };

// static _notmuch_qnode_t *
// boolean_transform (_notmuch_prefix_transform_t *xself, _notmuch_qnode_t *node)
// {
//     struct _notmuch_boolean_transform *self =
// 	(struct _notmuch_boolean_transform*)xself;
//     return _notmuch_qparser_make_boolean_query (
// 	node, node->text, self->db_prefix,
// 	self->exclusive ? self->db_prefix : NULL,
// 	XXX);
// }

// _notmuch_qparser_transform_t *
// _notmuch_qparser_boolean_transform_create (const void *ctx, const char *prefix,
// 					   const char *db_prefix, bool exclusive)
// {
//     struct _notmuch_boolean_transform *t =
// 	talloc (ctx, struct _notmuch_boolean_transform);
//     if (! t)
// 	return NULL;
//     t->prefix_transform.transform = boolean_transform;
//     t->db_prefix = db_prefix;
//     t->exclusive = exclusive;
//     return _notmuch_qparser_prefix_transform_create (
// 	ctx, prefix, &t->prefix_transform);
// }


// class _notmuch_qparser_transform
// {
// public:
//     virtual ~_notmuch_qparser_transform() { }
//     /**
//      * Transform a parse tree.  Must return the new root of the tree.
//      */
//     virtual _notmuch_qnode_t *transform (_notmuch_qnode_t *root) = 0;
// };

// class _notmuch_qparser_prefix_transform
// {
// public:
//     virtual ~_notmuch_qparser_prefix_transform() { }
//     /**
//      * Transform a NODE_TERMS token that this prefix applies to.
//      */
//     virtual _notmuch_qnode_t *transform (_notmuch_qnode_t *node) = 0;
// };

// class _notmuch_qparser_prefix_adpater : public _notmuch_qparser_transform
// {
//     const char *prefix_;
//     _notmuch_qnode_t *transform (_notmuch_qnode_t *root);
//     _notmuch_qnode_t *rec (_notmuch_qnode_t *node, bool active);
// public:
//     _notmuch_qparser_prefix_adpater (const char *prefix) : prefix_(prefix) { }
// };

// _notmuch_qnode_t *
// _notmuch_qparser_prefix_transform::transform (_notmuch_qnode_t *root)
// {
//     return rec (root, false);
// }

// _notmuch_qnode_t *
// _notmuch_qparser_prefix_transform::rec (_notmuch_qnode_t *node, bool active)
// {
//     if (active && node->type == NODE_TERMS)
// 	return transform_prefix (node);
//     else if (node->type == NODE_PREFIX)
// 	active = (strcmp (node->text, prefix_) == 0);
//     for (size_t i = 0; i < node->nchild; i++)
// 	node->child[i] = rec (node->child[i], active);
//     if (active && node->type == NODE_PREFIX)
// 	return node->child[0];
// }

// class _notmuch_qparser_boolean_transform
//     : public _notmuch_qparser_prefix_transform
// {
//     const char *db_prefix_;
//     bool exclusive_;
//     _notmuch_qnode_t *transform_prefix (_notmuch_qnode_t *node);
// public:
//     _notmuch_qparser_boolean_transform (
// 	const char *prefix, const char *db_prefix, bool exclusive)
// 	: _notmuch_qparser_prefix_transform (prefix), db_prefix_ (db_prefix),
// 	  exclusive_ (exclusive) { }
// };

// _notmuch_qnode_t *
// _notmuch_qparser_boolean_transform::transform_prefix (_notmuch_qnode_t *node)
// {
//     return _notmuch_qparser_make_boolean_query (
// 	node, db_prefix_, exclusive_ ? db_prefix_ : NULL);
// }

// // XXX Is this even necessary?  If the caller can separately parse and
// // generate, then they can also transform all they want however they
// // want.
// //
// // XXX Errors from transforms.
// static _notmuch_qnode_t *
// transform (_notmuch_qnode_t *root, _notmuch_qparser_transform **transforms,
// 	   size_t ntransforms)
// {
//     for (size_t i = 0; i < ntransforms; i++)
// 	root = transforms[i]->transform (transforms[i], root);
//     return root;
// }


/*
 * Generator
 */

struct _generate_state
{
    const void *ctx, *local;
    Xapian::TermGenerator tgen;
    const char *error;
};

static _notmuch_qnode_t *
generate_group (struct _generate_state *s, _notmuch_qnode_t *node)
{
    /* Turn the group into a NODE_AND, but with children in the same
     * conjunction class OR'd. */
    _notmuch_qnode_t *sub =
	_notmuch_qnode_create (s->local, NODE_AND, NULL, &s->error);
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
		    _notmuch_qnode_create (s->local, NODE_OR, NULL, &s->error);
		if (! conj)
		    return NULL;
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

    case NODE_PREFIX:
	/* Transformers have stripped out all known prefixes. */
	if (! s->error)
	    s->error = talloc_asprintf (
		s->ctx, "Unknown prefix '%s' in query", node->text);
	return Query ();

    case NODE_GROUP:
	/* XXX Currently we don't distinguish weighted and
	 * non-weighted parts of the query.  Xapian uses FILTER or
	 * SCALE_WEIGHT*0 for any boolean-prefixed terms in a prob.
	 * We should do this, too, but should probably handle it
	 * generically in NODE_AND. */
	return generate (s, generate_group (s, node));

    case NODE_TERMS:
	/* Terms that weren't prefixed are treated as regular text
	 * queries. */
	return _notmuch_qparser_make_text_query (
	    s->local, node->text, NULL, s->tgen, &s->error)->query;

    case NODE_QUERY:
	return node->query;

    case TOK_LOVE: case TOK_HATE: case TOK_BRA: case TOK_KET: case TOK_END:
	/* Fall through to the error after the switch */
	break;
    }
    INTERNAL_ERROR ("Illegal token %s in IR", qnode_to_string (s->local, node));
}

Xapian::Query
_notmuch_qparser_generate (const void *ctx, _notmuch_qnode_t *root,
			   Xapian::TermGenerator tgen,
			   const char **error_out)
{
    void *local = talloc_new (ctx);
    struct _generate_state state = {ctx, local, tgen, NULL};
    Xapian::Query query = generate (&state, root);
    talloc_free (local);
    if (state.error) {
	if (! *error_out)
	    *error_out = state.error;
	return Xapian::Query ();
    }
    return query;
}
