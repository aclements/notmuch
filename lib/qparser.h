/* qparser.h - Notmuch of a query parser
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

#ifndef NOTMUCH_QPARSER_H
#define NOTMUCH_QPARSER_H

#include <xapian.h>

// XXX PEG parser doesn't need TOK_ types or text argument to
// qnode_create.

/**
 * Query parser AST node type.
 *
 * This MUST be kept in sync with qnode_type_names in qparser.cc.
 */
enum _notmuch_qnode_type
{
    /* The first four token types appear only in the lexer output and
     * never in the parse tree. */
    TOK_LOVE, TOK_HATE, TOK_BRA, TOK_KET,
    /* Binary operators.  These have two children. */
    NODE_AND, NODE_OR,
    /* Unary operators.  These have one child.
     *
     * Xapian::Query has no pure NOT operator, so the generator
     * converts (AND x (NOT y)) into (x AND_NOT y) and other
     * occurrences of (NOT x) into (<all> AND_NOT x).
     *
     * For NODE_PREFIX, the text field specifies the prefix (sans
     * colon).  The child of NODE_PREFIX may be a term or a sub-query.
     */
    NODE_NOT, NODE_PREFIX,
    /* A group of space-separated queries.  These appear only in the
     * parser output and never in the lexer output.  At a syntactic
     * level, a group is the stuff between boolean operators.  Its
     * children are terms, prefixed terms, and sub-queries.  (Xapian
     * calls this a "prob". */
    NODE_GROUP,
    /* Search terms.  This can represent a single term, a quoted
     * phrase, or an implicit phrase.  An implicit phrase is something
     * like "foo/bar", for which the database contains two separate
     * terms, but you want to treat them as a phrase, even though it's
     * not quoted.  Xapian calls characters that implicitly connect
     * terms into phrases "phrase generators."  We take a simpler
     * approach: the lexer consumes almost anything except whitespace
     * as a potential phrase and the generator splits it into
     * individual search terms. */
    NODE_TERMS,
    /* A "compiled" Xapian query.  These are produced by
     * transformations when they compile parts of a query AST. */
    NODE_QUERY,
    /* TOK_END indicates the end of the token list.  Such tokens loop
     * back on themselves so it's always safe to follow "next".
     * These appear only in the lexer output. */
    TOK_END,
};

/**
 * A node in the query parser's abstract syntax tree.
 */
typedef struct _notmuch_qnode
{
    enum _notmuch_qnode_type type;

    /* For NODE_PREFIX, the query prefix of this token.  For
     * NODE_TERMS, the literal text from the query. */
    const char *text;

    /* For NODE_QUERY, the Xapian query for this node.  (For other
     * types, this will be the default-constructed Query, which is
     * just a NULL pointer internally.)
     */
    Xapian::Query query;

    /* For children of NODE_GROUP, the conjunction class of this node.
     * Children with identical non-NULL conjunction classes will be
     * OR'd together (and these, in turn, AND'd). */
    const char *conj_class;

    /* Link in the lexer token list. */
    struct _notmuch_qnode *next;

    /* Links to children in the intermediate AST. */
    size_t nchild;
    struct _notmuch_qnode **child;
} _notmuch_qnode_t;

#pragma GCC visibility push(hidden)

_notmuch_qnode_t *
_notmuch_qnode_create (const void *ctx, enum _notmuch_qnode_type type,
		       const char *text, const char **error_out);

void
_notmuch_qnode_add_child (_notmuch_qnode_t *parent, _notmuch_qnode_t *child,
			  const char **error_out);

const char *
_notmuch_qnode_tree_to_string (const void *ctx, _notmuch_qnode_t *node);

_notmuch_qnode_t *
_notmuch_qparser_parse (const void *ctx, const char *query);

#pragma GCC visibility pop

#endif
