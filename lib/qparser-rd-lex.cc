#include "qparser.h"
#include "notmuch-private.h"

using Xapian::Unicode::is_whitespace;
using Xapian::Unicode::is_wordchar;

static const char *qnode_type_names[] = {
    "LOVE", "HATE", "BRA", "KET",
    "AND", "OR",
    "NOT", "LABEL",
    "GROUP",
    "TERMS", "QUERY", "END"
};

static const char *
token_to_string (const void *ctx, _notmuch_qnode_t *node)
{
    if ((unsigned)node->type > TOK_END)
	return talloc_asprintf (ctx, "<bad type %d>", node->type);

    if (node->type == NODE_TERMS)
	return talloc_asprintf (ctx, "\"%s\"", node->text);
    else if (node->type == NODE_LABEL)
	return talloc_asprintf (ctx, "LABEL/%s", node->text);
    else if (node->type == NODE_QUERY)
	return talloc_asprintf (ctx, "QUERY/%s",
				node->query.get_description ().c_str () +
				strlen ("Xapian::Query"));

    return qnode_type_names[node->type];
}

__attribute__((unused))
static char *
token_list_to_string (const void *ctx, _notmuch_qnode_t *node)
{
    void *local = talloc_new (ctx);
    char *out = talloc_strdup (ctx, "");

    for (; node->type != TOK_END; node = node->next) {
	const char *t = token_to_string (local, node);
	out = talloc_asprintf_append_buffer (
	    out, "%s%s", *out == 0 ? "" : " ", t);
    }

    talloc_free (local);
    return out;
}

/*
 * Lexer
 */

// XXX Is the lexing of boolean terms compatible with the existing
// quoting that we do for boolean terms?

struct _lex_state
{
    const void *ctx;
    _notmuch_qnode_t *head, **ptail;
    const char *error;
};

static Xapian::Utf8Iterator
lex_skip_ws (Xapian::Utf8Iterator it)
{
    while (is_whitespace (*it))
	it++;
    return it;
}

static void
lex_emit (struct _lex_state *s, enum _notmuch_qnode_type type, char *text)
{
    _notmuch_qnode_t *node =
	_notmuch_qnode_create (s->ctx, type, text, &s->error);
    if (! node)
	return;
    node->next = *(s->ptail);
    *(s->ptail) = node;
    s->ptail = &node->next;
}

/* Lex a quoted phrase, emitting a NODE_TERMS token and returning an
 * iterator pointing to the character following the closing quote.
 * Doubled quotes in the phrase are un-escaped to a single quote.
 */
static Xapian::Utf8Iterator
lex_quoted_phrase (struct _lex_state *s, Xapian::Utf8Iterator it)
{
    Xapian::Utf8Iterator next, tmp, end;
    char *term, *src, *dst;

    /* Find the end of the phrase.  Xapian distinguishes between
     * regular phrases that have no way to escape quotes and boolean
     * terms, where quotes are escaped, but we simplify this to a
     * single lexical grammar with escaped quotes. */
    if (*(it++) != '"')
	INTERNAL_ERROR ("lex_quoted_phrase must be called on a '\"'");
    for (next = it; ; ++next) {
	if (next == end) {
	    s->error = "Unbalanced quotation marks";
	    return end;
	} else if (*next == '"') {
	    tmp = next;
	    if (++tmp == end || *tmp != '"')
		break;
	    ++next;
	}
    }

    /* Xapian lexes +/-/( in quotes mode and simply doesn't generate
     * tokens for them.  For us, the term generator will discard
     * them. */
    term = talloc_strndup (s->ctx, it.raw (), next.raw () - it.raw ());
    /* Un-escape doubled quotes. */
    for (src = dst = term; *src; ++src, ++dst) {
	*dst = *src;
	if (*src == '"')
	    ++src;
    }
    *dst = '\0';
    lex_emit (s, NODE_TERMS, term);

    return ++next;
}

/* Lex a query, returning the head of a list of tokens.
 *
 * The result will be a sequence of the following form:
 *   ((LOVE|HATE)? LABEL? (TERMS|BRA|KET|AND|OR|NOT))*
 *
 * If there is a lexical error in the input, *error_out will be set to
 * an error string (either static or allocated under ctx) and this
 * will return NULL.
 */
static _notmuch_qnode_t *
lex (const void *ctx, const char *query, const char **error_out)
{
    Xapian::Utf8Iterator it (query), next, end;
    struct _lex_state state = {ctx, NULL, &state.head, NULL};
    struct _lex_state *s = &state;

    /* Create the distinguished self-looping end token. */
    s->head = _notmuch_qnode_create (ctx, TOK_END, NULL, error_out);
    if (! s->head)
	return NULL;
    s->head->next = s->head;

    while (it != end && !s->error) {
	if ((it = lex_skip_ws (it)) == end)
	    break;

	unsigned ch = *it;

	/* Try to consume a love/hate token. */
	if (ch == '+' || ch == '-') {
	    ++it;
	    /* Xapian ignores these unless preceded by whitespace or
	     * an open paren, which has the effect of ignoring all
	     * +'s in "x +++y", "x#+y", and "(x)+y".  We don't
	     * discriminate. */

	    /* Ignore if followed by a space or another + or - */
	    if (is_whitespace (*it) || *it == '+' || *it == '-')
		continue;
	    lex_emit (s, ch == '+' ? TOK_LOVE : TOK_HATE, NULL);
	    continue;
	}

	/* Try to consume a label, which is a sequence of word
	 * characters followed by a colon.  Xapian allows anything
	 * except colon and whitespace, but restricts to registered
	 * labeles.  Our syntax is not sensitive to the set of
	 * registered labeles, so we're more restrictive in the
	 * accepted characters. */
	next = it;
	while (next != end && is_wordchar(*next))
	    ++next;
	if (next != it && next != end && *next == ':') {
	    /* Found a label.  In Xapian, labels corresponding to
	     * boolean prefixes dramatically affect the accepted
	     * lexical grammar of the following term.  We make no
	     * distinction; a label is a label.  This is okay because
	     * we lex the term following the label similar to how
	     * Xapian lexes boolean terms anyway (and term splitting
	     * happens late, unlike in Xapian). */
	    char *label = talloc_strndup (ctx, it.raw (),
					  next.raw () - it.raw ());
	    lex_emit (s, NODE_LABEL, label);
	    it = ++next;
	}

	/* Consume a term or a sub-query. */
	ch = *it;
	switch (ch) {
	case '(':
	    ++it;
	    /* Xapian ignores this unless preceded by whitespace,
	     * parens, +, or -.  We don't discriminate. */
	    lex_emit (s, TOK_BRA, NULL);
	    continue;

	case ')':
	    ++it;
	    lex_emit (s, TOK_KET, NULL);
	    continue;

	case '"':
	    it = lex_quoted_phrase (s, it);
	    continue;
	}

	/* Consume a (possibly empty) term up to the next (, ), ", or
	 * whitespace.  We'll word-split this much later.  (As an
	 * optimization, we start at 'next', which may already have
	 * scanned over some word characters above.)
	 *
	 * Xapian permits other characters to separate term phrases.
	 * For example, "x#y" is parsed as two separate (non-phrase)
	 * terms.  However, because the characters allowed in a term
	 * are context-sensitive, this is quite unpredictable and
	 * replicating this is very hard.  Here we take a simpler
	 * approach where only whitespace and a few operator
	 * characters that are never term characters separate
	 * terms. */
	while (next != end && !strchr ("()\"", *next) && !is_whitespace (*next))
	    ++next;
	char *term = talloc_strndup (s->ctx, it.raw (), next.raw () - it.raw ());
	it = next;
	if (strcasecmp (term, "and") == 0)
	    lex_emit (s, NODE_AND, term);
	else if (strcasecmp (term, "or") == 0)
	    lex_emit (s, NODE_OR, term);
	else if (strcasecmp (term, "not") == 0)
	    lex_emit (s, NODE_NOT, term);
	else
	    lex_emit (s, NODE_TERMS, term);
    }

    if (s->error) {
	*error_out = s->error;
	return NULL;
    }
    return state.head;
}

/*
 * Parser
 */

struct _parse_state
{
    const void *ctx;
    _notmuch_qnode_t *tok;	/* Next input token */
    const char *error;
};

static _notmuch_qnode_t *
parse_binary_op (struct _parse_state *s, int prec);

static _notmuch_qnode_t *
parse_consume (struct _parse_state *s)
{
    _notmuch_qnode_t *next = s->tok;
    s->tok = s->tok->next;
    return next;
}

static _notmuch_qnode_t *
parse_term (struct _parse_state *s)
{
    _notmuch_qnode_t *sub;
    switch (s->tok->type) {
    case TOK_BRA:
	/* Sub-query */
	parse_consume (s);
	sub = parse_binary_op (s, 0);
	if (s->tok->type != TOK_KET) {
	    if (! s->error)
		s->error = "Missing close parenthesis.  Did you mean to quote the open parenthesis?";
	    return NULL;
	}
	return sub;

    case TOK_KET:
	/* This can arise from, e.g. "+)" or "foo:)".  Insert an
	 * empty term and let the caller handle the ")". */
	return _notmuch_qnode_create (s->ctx, NODE_TERMS, "", &s->error);

    case NODE_LABEL:
	sub = parse_consume (s);
	_notmuch_qnode_add_child (sub, parse_term (s), &s->error);
	return sub;

    case NODE_TERMS:
	/* If we encounter an operator, treat it like a term.  This
	 * can arise from, e.g. "+and" or "foo:and". */
    case NODE_AND: case NODE_OR: case NODE_NOT:
	sub = parse_consume (s);
	sub->type = NODE_TERMS;
	return sub;

    case NODE_GROUP: case NODE_QUERY:
	/* The following tokens can't happen here because of the
	 * restrictions on the lexer's output sequence. */
    case TOK_LOVE: case TOK_HATE: case TOK_END:
	/* Fall through to the error after the switch */
	break;
    }
    INTERNAL_ERROR ("Unexpected token %s", token_to_string (s->ctx, s->tok));
}

static _notmuch_qnode_t *
parse_group (struct _parse_state *s)
{
    /* A group is a sequence of three types of subqueries.  Because
     * the default query operator is AND, loved terms are not treated
     * specially.
     * 1) Probabilistic terms (labeled or not).  These are combined
     *    with the default query operator, AND.
     * 2) Terms with a boolean label.  All of the terms with the same
     *    label are combined with OR.  Different labels are combined
     *    with AND.
     * 3) Hate terms.  These are combined with OR.
     * The final IR looks like
     *   (probs AND (FILTER bools)) AND (NOT hates)
     * XXX Fix up
     */

    bool done = false;
    _notmuch_qnode_t *group, *sub;
    group = _notmuch_qnode_create (s->ctx, NODE_GROUP, NULL, &s->error);
    if (! group)
	return NULL;
    while (! done && ! s->error) {
	switch (s->tok->type) {
	case TOK_KET: case NODE_AND: case NODE_OR: case NODE_NOT: case TOK_END:
	    /* End of the group. */
	    /* XXX Allows empty groups in the query "and" */
	    done = true;
	    break;

	case TOK_HATE:
	    /* Hates are like NOTs, just with lower precedence.
	     * Xapian treats a hate following an AND like "x AND -y z"
	     * as "x AND NOT y z", which causes *both* y and z to be
	     * negated, rather than just y.  We don't special case
	     * this. */
	    sub = parse_consume (s);
	    sub->type = NODE_NOT;
	    _notmuch_qnode_add_child (sub, parse_term (s), &s->error);
	    _notmuch_qnode_add_child (group, sub, &s->error);
	    break;

	case TOK_LOVE:
	    /* Join into the query like any other term, since the
	     * default operator is AND anyway. */
	    parse_consume (s);
	    /* Fall through */
	case TOK_BRA:
	    /* In Xapian, exclusive boolean prefixes and sub-queries
	     * interact strangely.  "boolex:x boolex:y" will produce a
	     * query for x OR y, while "boolex:x (boolex:y)" will
	     * produce a query for x AND y.
	     * XXX Maybe we do the same.
	     */

	    /* XXX How are sub-queries incorporated into the group?
	     * Maybe we *shouldn't* reduce single-child groups; then
	     * we'd do what Xapian does. */
	case NODE_LABEL:
	case NODE_TERMS:
	    sub = parse_term (s);
	    _notmuch_qnode_add_child (group, sub, &s->error);
	    break;

	case NODE_GROUP: case NODE_QUERY:
	    INTERNAL_ERROR ("Unexpected token %s",
			    token_to_string (s->ctx, s->tok));
	}
    }

    /* XXX What if it has 0 children? */
    if (group->nchild == 1 && group->child[0]->type == NODE_NOT)
	/* Peephole optimization: promote individual hate terms so the
	 * generator can find (AND x (NOT y)) patterns. */
	return group->child[0];
    return group;
}

static _notmuch_qnode_t *
parse_unary_op (struct _parse_state *s)
{
    if (s->tok->type == NODE_NOT) {
	_notmuch_qnode_t *sub = parse_consume (s);
	_notmuch_qnode_add_child (sub, parse_unary_op (s), &s->error);
	return sub;
    }
    return parse_group (s);
}

static _notmuch_qnode_t *
parse_binary_op (struct _parse_state *s, int prec)
{
    if (prec == 2)
	return parse_unary_op (s);

    _notmuch_qnode_t *left = parse_binary_op (s, prec + 1);
    _notmuch_qnode_t *op = NULL;
    while (((prec == 0 && s->tok->type == NODE_OR) ||
	    (prec == 1 && s->tok->type == NODE_AND)) && ! s->error) {
	if (! op) {
	    op = s->tok;
	    _notmuch_qnode_add_child (op, left, &s->error);
	}
	parse_consume (s);
	_notmuch_qnode_add_child (op, parse_binary_op (s, prec + 1), &s->error);
    }
    return op ? op : left;
}

static _notmuch_qnode_t *
parse (const void *ctx, _notmuch_qnode_t *toks, const char **error_out)
{
    struct _parse_state state = {ctx, toks, NULL};
    struct _parse_state *s = &state;
    _notmuch_qnode_t *root = parse_binary_op (s, 0);
    if (s->tok->type == TOK_KET) {
	if (! s->error)
	    s->error = "Unexpected close parenthesis.  Did you mean to quote the parenthesis?";
    } else if (s->tok->type != TOK_END)
	INTERNAL_ERROR ("Token stream not fully consumed: %s",
			token_list_to_string (s->ctx, s->tok));
    if (s->error) {
	*error_out = s->error;
	return NULL;
    }
    return root;
}

_notmuch_qnode_t *
_notmuch_qparser_parse (const void *ctx, const char *query,
			const char **error_out)
{
    _notmuch_qnode_t *toks = lex (ctx, query, error_out);
    if (! toks)
	return NULL;
    return parse (ctx, toks, error_out);
}
