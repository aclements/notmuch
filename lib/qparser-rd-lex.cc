#include "qparser.h"
#include "notmuch-private.h"

/* This implements a top-down parser for a simple natural query
 * grammar inspired by the Xapian QueryParser grammar.  Each
 * production function gives its grammar as a PEG [1] fragment with
 * explicit cuts [2] (*).  The implementation follows these fragments
 * closely, but unlike a backtracking PEG parser, this parser is
 * predictive at all but the lowest levels.  Given the small lookahead
 * of the grammar, this simplifies error reporting and overall
 * implementation.
 *
 * Like most PEG parsers, this parser is scannerless.  The grammar has
 * fairly simple lexical rules, but the rules are stateful, making it
 * more natural to integrate lexical analysis into parsing.
 *
 * [1] Ford, B.  Parsing Expression Grammars: A recognition-based
 * syntactic foundation.
 *
 * [2] Mizushima, K., Maeda, A., Yamaguchi, Y.  Packrat parsers can
 * handle practical grammars in mostly constant space.
 *
 * (*) There are lots of good descriptions of PEG.  Cuts are more
 * esoteric, but not complex: a cut limits backtracking by committing
 * to the branch of the enclosing choice or to another repetition in a
 * repeat.
 */

using Xapian::Unicode::is_whitespace;
using Xapian::Unicode::is_wordchar;
using Xapian::Utf8Iterator;

struct _parse_state
{
    Utf8Iterator pos, start, end;
    const void *ctx;
    const char *error;
};

static _notmuch_qnode_t *
parse_binary_op (struct _parse_state *s, int prec);

static _notmuch_qnode_t *
parse_fail (struct _parse_state *s, const char *error)
{
    enum { LIM = 65 };
    if (! s->error) {
	/* Point out where in the query the problem is.  Getting the
	 * actual visual width of a Unicode string is HARD, so we just
	 * assume each code point is a space wide.  FIXME: position
	 * information should be propagated up to the caller so it can
	 * display it more appropriately. */
	Utf8Iterator estart (s->start), eend (s->end);
	/* If the query is long, truncate it to around pos. */
	if (estart.left () > LIM) {
	    while (s->pos.raw () - estart.raw () > LIM / 2)
		++estart;
	    eend = estart;
	    for (int i = 0; i < LIM && eend != s->end; ++i)
		++eend;
	}
	unsigned spaces = estart == s->start ? 0 : 3;
	for (Utf8Iterator it (estart); it != s->pos; ++it)
	    ++spaces;

	s->error = talloc_asprintf (
	    s->ctx, "%s\n  %s%.*s%s\n  %*s^",
	    error,
	    estart == s->start ? "" : "...",
	    (int)(eend == s->end ? estart.left () : eend.raw () - estart.raw ()),
	    estart.raw (), eend == s->end ? "" : "...",
	    spaces, "");
	if (! s->error)
	    s->error = error;
    }
    return NULL;
}

/**
 * KW <- "<kw>" (whitespace+ / &("(") / &(")") / !(.))
 */
static bool
parse_kw (struct _parse_state *s, const char *kw)
{
    size_t len = strlen (kw);
    if (s->pos.left () < len || strncasecmp (s->pos.raw (), kw, len) != 0)
	return false;
    Utf8Iterator pos (s->pos);
    for (size_t i = 0; i < len; ++i)
	++pos;
    /* A keyword must be followed by whitespace, (, ), or the end. */
    bool ws = false;
    while (pos != s->end && is_whitespace (*pos)) {
	++pos;
	ws = true;
    }
    if (ws || pos == s->end || *pos == '(' || *pos == ')') {
	s->pos = pos;
	return true;
    }
    return false;
}

static unsigned
parse_punctuation (struct _parse_state *s, const char *accept)
{
    unsigned c;
    if (s->pos == s->end || ! strchr (accept, (c = *s->pos)))
	return 0;
    ++s->pos;
    return c;
}

/**
 * whitespace <- [[:whitespace:]]*
 */
static void
parse_whitespace (struct _parse_state *s)
{
    while (s->pos != s->end && is_whitespace (*s->pos))
	++s->pos;
}

/**
 * quoted <- ([^"] / "\"\"")*
 */
static _notmuch_qnode_t *
parse_quoted (struct _parse_state *s)
{
    /* Find the end of the phrase.  Xapian distinguishes between
     * regular phrases that have no way to escape quotes and boolean
     * terms, where quotes are escaped, but we simplify this to a
     * single lexical grammar with escaped quotes.  Xapian also lexes
     * +/-/( in quotes mode and simply doesn't generate tokens for
     * them.  For us, the term generator will discard them. */
    Utf8Iterator pos (s->pos);
    while (pos != s->end) {
	if (*pos == '"') {
	    if (pos.left() < 2 || strncmp (pos.raw (), "\"\"", 2) != 0)
		break;
	    ++pos;
	}
	++pos;
    }
    if (pos == s->end)
	return parse_fail (s, "Unbalanced quotation marks");

    /* Create node */
    _notmuch_qnode_t *node =
	_notmuch_qnode_create (s->ctx, QNODE_TERMS, &s->error);
    if (! node)
	return NULL;
    char *dst = talloc_array (node, char, pos.raw () - s->pos.raw () + 1);
    if (! dst)
	return parse_fail (s, "Out of memory allocating term");
    node->text = dst;
    node->quoted = true;

    /* Copy and de-escape text */
    for (const char *src = s->pos.raw (); src != pos.raw (); ++src, ++dst) {
	*dst = *src;
	if (*src == '"')
	    ++src;
    }
    *dst = 0;
    s->pos = pos;
    return node;
}

/**
 * term <- "(" ^ whitespace* and_expr ")" whitespace*
 *       / "\"" ^ quoted "\"" whitespace*
 *       / [^()"[:whitespace:]]+ whitespace*
 */
static _notmuch_qnode_t *
parse_term (struct _parse_state *s)
{
    _notmuch_qnode_t *node;

    switch (parse_punctuation (s, "(\"")) {
    case '(':
	/* Xapian ignores '(' unless preceded by whitespace, parens,
	 * +, or -.  We don't discriminate. */
	parse_whitespace (s);
	node = parse_binary_op (s, 0);
	if (! parse_punctuation (s, ")"))
	    return parse_fail (s, "Missing close parenthesis.  "
			       "Did you mean to quote the open parenthesis?");
	parse_whitespace (s);
	return node;

    case '"':
	node = parse_quoted (s);
	if (! node)
	    return NULL;
	++s->pos;
	parse_whitespace (s);
	return node;
    }

    /* Consume a (possibly empty) term up to the next (, ), ", or
     * whitespace.  We'll word-split this after parsing.
     *
     * Xapian permits other characters to separate term phrases.  For
     * example, "x#y" is parsed as two separate (non-phrase) terms.
     * However, because the characters allowed in a term are
     * context-sensitive, this is quite unpredictable and replicating
     * this is very hard.  Here we take a simpler approach where only
     * whitespace and a few operator characters that are never term
     * characters separate terms. */
    Utf8Iterator pos (s->pos);
    while (pos != s->end && !(*pos == '(' || *pos == ')' || *pos == '"' ||
			      is_whitespace (*pos)))
	++pos;
    if (pos == s->pos)
	return parse_fail (s, "Search term expected");
    node = _notmuch_qnode_create (s->ctx, QNODE_TERMS, &s->error);
    if (! node)
	return NULL;
    node->text =
	talloc_strndup (node, s->pos.raw (), pos.raw () - s->pos.raw ());
    if (! node->text)
	return parse_fail (s, "Out of memory allocating term");
    s->pos = pos;
    parse_whitespace (s);
    return node;
}

/**
 * field <- [[:wordchar:]]+ ":"
 */
static _notmuch_qnode_t *
parse_field (struct _parse_state *s)
{
    /* A field is a sequence of word characters followed by a colon.
     * Xapian allows anything except colon and whitespace, but
     * restricts to registered fields.  Our syntax is not sensitive to
     * the set of registered fields, so we're more restrictive in the
     * accepted characters.
     *
     * In Xapian, fields for boolean prefixes dramatically affect the
     * accepted lexical grammar of the following term.  We make no
     * distinction; a field is a field.  This is okay because we parse
     * the term itself much like how Xapian lexes boolean terms anyway
     * (and term splitting happens later, unlike in Xapian where it
     * happens during parsing).
     */
    Utf8Iterator pos (s->pos);
    while (pos != s->end && is_wordchar (*pos))
	++pos;
    if (pos == s->pos || pos == s->end || *pos != ':')
	return NULL;
    _notmuch_qnode_t *field =
	_notmuch_qnode_create (s->ctx, QNODE_FIELD, &s->error);
    if (! field)
	return NULL;
    field->text =
	talloc_strndup (s->ctx, s->pos.raw (), pos.raw () - s->pos.raw ());
    if (! field->text)
	return parse_fail (s, "Out of memory allocating field");
    s->pos = ++pos;
    return field;
}

/**
 * group <- (!(AND / OR / NOT) ("+"/"-")? field? term)+
 */
static _notmuch_qnode_t *
parse_group (struct _parse_state *s)
{
    bool done = false;
    _notmuch_qnode_t *group, *sub, *node;
    group = _notmuch_qnode_create (s->ctx, QNODE_GROUP, &s->error);
    if (! group)
	return NULL;
    while (! done && ! s->error) {
	Utf8Iterator start (s->pos);
	if (s->pos == s->end || *s->pos == ')' ||
	    parse_kw (s, "and") || parse_kw (s, "or") || parse_kw (s, "not")) {
	    s->pos = start;
	    break;
	}

	sub = group;

	/* Optional love/hate marker.  Just skip +, since the default
	 * group operator is AND anyway.
	 *
	 * Xapian ignores these unless preceded by whitespace or an
	 * open paren, which has the effect of ignoring all +'s and
	 * -'s in "x +++y", "x#+y", and "(x)+y" and "-x".  We don't
	 * discriminate. */
	if (parse_punctuation (s, "-+") == '-') {
	    /* Hates are like NOTs, just with lower precedence.
	     * Xapian treats a hate following an AND like "x AND -y z"
	     * as "x AND NOT y z", which causes *both* y and z to be
	     * negated, rather than just y.  We don't special case
	     * this. */
	    node = _notmuch_qnode_create (s->ctx, QNODE_NOT, &s->error);
	    if (! node)
		return NULL;
	    _notmuch_qnode_add_child (sub, node, &s->error);
	    sub = node;
	}

	/* Optional field */
	node = parse_field (s);
	if (node) {
	    _notmuch_qnode_add_child (sub, node, &s->error);
	    sub = node;
	}

	/* Term */
	node = parse_term (s);
	if (node)
	    _notmuch_qnode_add_child (sub, node, &s->error);
    }

    if (group->nchild == 0)
	return parse_fail (s, "Expected term or group of terms");
    if (group->nchild == 1 && group->child[0]->type == QNODE_NOT)
	/* Peephole optimization: promote individual hate terms so the
	 * generator can find (AND x (NOT y)) patterns. */
	return group->child[0];
    return group;
}

/**
 * unary_expr <- NOT ^ unary_expr
 *             / group
 */
static _notmuch_qnode_t *
parse_unary_op (struct _parse_state *s)
{
    if (parse_kw (s, "not")) {
	_notmuch_qnode_t *sub =
	    _notmuch_qnode_create (s->ctx, QNODE_NOT, &s->error);
	if (sub)
	    _notmuch_qnode_add_child (sub, parse_unary_op (s), &s->error);
	return sub;
    }
    return parse_group (s);
}

/**
 * or_expr  <- and_expr (OR ^ and_expr)*
 * and_expr <- unary_expr ((AND / NOT) ^ unary_expr)*
 */
static _notmuch_qnode_t *
parse_binary_op (struct _parse_state *s, int prec)
{
    if (prec == 2)
	return parse_unary_op (s);

    _notmuch_qnode_t *left = parse_binary_op (s, prec + 1);
    _notmuch_qnode_t *op = NULL;
    Utf8Iterator prev_pos;
    bool and_not;
    /* We allow things like "a NOT b" to mean "a AND NOT b" for Xapian
     * compatibility. */
    while (prev_pos = s->pos, and_not = false,
	   ! s->error && ((prec == 0 && parse_kw (s, "or")) ||
			  (prec == 1 && (parse_kw (s, "and") ||
					 (and_not = parse_kw (s, "not")))))) {
	if (! op) {
	    op = _notmuch_qnode_create (s->ctx, prec == 0 ? QNODE_OR : QNODE_AND,
					&s->error);
	    if (! op)
		return NULL;
	    _notmuch_qnode_add_child (op, left, &s->error);
	}
	if (and_not)
	    /* Back up and let parse_unary_op consume the NOT and
	     * build the NOT node. */
	    s->pos = prev_pos;
	_notmuch_qnode_add_child (op, parse_binary_op (s, prec + 1), &s->error);
    }
    return op ? op : left;
}

_notmuch_qnode_t *
_notmuch_qparser_parse (const void *ctx, const char *query,
			const char **error_out)
{
    Utf8Iterator start (query);
    struct _parse_state state = {start, start, Utf8Iterator (), ctx, NULL};
    struct _parse_state *s = &state;
    parse_whitespace (s);
    _notmuch_qnode_t *root = parse_binary_op (s, 0);

    if (s->error) {
	*error_out = s->error;
	return NULL;
    } else if (parse_punctuation (s, ")")) {
	parse_fail (s, "Unexpected close parenthesis.  "
		    "Did you mean to quote the parenthesis?");
    } else if (s->pos != s->end) {
	INTERNAL_ERROR ("Query not fully consumed: %s", s->pos.raw ());
    }

    return root;
}
