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
// qnode_create or next link.

/**
 * Query parser AST node type.
 *
 * This MUST be kept in sync with qnode_type_names in qparser.cc.
 */
enum _notmuch_qnode_type
{
    /* Binary operators.  These have two or more children. */
    NODE_AND, NODE_OR,
    /* Unary operators.  These have one child.
     *
     * Xapian::Query has no pure NOT operator, so the generator
     * converts (AND x (NOT y)) into (x AND_NOT y) and other
     * occurrences of (NOT x) into (<all> AND_NOT x).
     *
     * For NODE_LABEL, the text field specifies the label (sans
     * colon).  The child of NODE_LABEL may be a term or a sub-query.
     */
    NODE_NOT, NODE_LABEL,
    /* A group of space-separated queries.  Syntactically, a group is
     * the stuff between boolean operators.  It's equivalent to
     * NODE_AND except that children with the same conjunction class
     * are OR'd.  (Xapian calls this a "prob".) */
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
};

/**
 * A node in the query parser's abstract syntax tree.
 */
typedef struct _notmuch_qnode
{
    enum _notmuch_qnode_type type;

    /* For NODE_LABEL, the query label of this token.  For NODE_TERMS,
     * the literal text from the query. */
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
_notmuch_qnode_to_string (const void *ctx, _notmuch_qnode_t *node);

_notmuch_qnode_t *
_notmuch_qparser_parse (const void *ctx, const char *query,
			const char **error_out);

/**
 * Transform all terms that have the given label into literal queries.
 * If exclusive is true, then all terms with this label in the same
 * group will be OR'd (rather than the default AND).
 */
_notmuch_qnode_t *
_notmuch_qparser_literal_prefix (_notmuch_qnode_t *node, const char *label,
				 const char *db_prefix, bool exclusive,
				 const char **error_out);

/**
 * Transform all terms that have the given label into text queries.
 */
_notmuch_qnode_t *
_notmuch_qparser_text_prefix (_notmuch_qnode_t *node, const char *label,
			      const char *db_prefix, Xapian::TermGenerator tgen,
			      const char **error_out);

/**
 * Generate a Xapian Query from a qparser AST.
 *
 * Any NODE_TERMS remaining in the AST will be parsed into a text
 * query using the provided term generator.  If the AST contains any
 * NODE_LABEL nodes, an "unknown label" error will be reported;
 * callers should eliminate all known labels during transformation.
 *
 * If there's an error, this returns an empty Query and sets
 * *error_out to the error if *error_out is NULL.  The error will
 * either be a static string or allocated in ctx.
 */
Xapian::Query
_notmuch_qparser_generate (const void *ctx, _notmuch_qnode_t *root,
			   Xapian::TermGenerator tgen,
			   const char **error_out);

#pragma GCC visibility pop

#endif
