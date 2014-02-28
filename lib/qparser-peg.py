"""Simple PEG parser generator.

This implements a C parser generator for Parsing Expression Grammars
[1] with support for explicit cut operators [2].

[1] Ford, B.  Parsing Expression Grammars: A recognition-based
syntactic foundation.

[2] Mizushima, K., Maeda, A., Yamaguchi, Y.  Packrat parsers can
handle practical grammars in mostly constant space.
"""

import sys
import re
import contextlib

# XXX Handle system errors (e.g., allocation failure)

# XXX Generate useful messages on parse errors

# XXX Clean up allocations better on backtrack

class PExpr:
    """Base class for PEG parsing expressions."""
    def __init__(self, *productions):
        self.productions = productions
        self._has_cut \
            = any(p._has_cut for p in productions if isinstance(p, PExpr))

    def gen_c(self, g):
        raise NotImplementedError('gen_c is abstract')

class AutoSeq(PExpr):
    """Parsing expressions that implicitly Seq their arguments."""
    def __init__(self, *productions):
        if len(productions) == 1:
            super().__init__(*productions)
        else:
            super().__init__(Seq(*productions))
        self._production = self.productions[0]

class Grammar:
    def __init__(self, user_state_type):
        self.__user_state_type = user_state_type
        self.__decls = []
        self.__code = []
        self.__indent = 0
        self.__errors = {}

    def rules(self, **rules):
        # XXX All of the C generation should be done in write_to
        def rec(pexpr, counter, fname=None):
            if pexpr in self.__fnames: return
            for sub in pexpr.productions:
                rec(sub, counter)
            # We could make these methods of the parser state, which
            # would be nice and OO, but by declaring all parser
            # functions as static, the compiler is able to inline the
            # vast majority of these functions and doesn't have to
            # emit non-inlined versions at all.
            if not fname:
                fname = '_parse_%s_%d' % (name, counter[0])
                counter[0] += 1
            self.__fnames[pexpr] = fname
            decl = ('static bool\n'
                    '%s (struct _parse_state *s, Utf8Iterator &pos)' %
                    fname)
            with self.func(decl):
                pexpr.gen_c(self)
        # Establish function names of named definitions
        self.__fnames = {name: '_parse_' + name for name in rules}
        # Generate code for all parsing expressions via post-order DFS
        for name, pexpr in sorted(rules.items()):
            if pexpr._has_cut:
                raise ValueError('Top-level cut in rule %r' % name)
            rec(pexpr, [0], self.__fnames[name])

    def write_to(self, fp):
        fp.write('''\
#include <xapian.h>
#include <strings.h>
#include <stdint.h>

using Xapian::Utf8Iterator;

struct _parser_error_info {
    const char *error_pos;   /* Error position */
    uint64_t error_set;      /* Errors at error_pos */
    unsigned disable;        /* Error tracking disable depth */
};

static bool
_parser_error (struct _parser_error_info *e, const char *pos, uint64_t set) {
    if (e->disable) {
        return false;
    } else if (pos == e->error_pos) {
        e->error_set |= set;
    } else if (pos > e->error_pos) {
        e->error_pos = pos;
        e->error_set = set;
    }
    return false;
}

struct _parse_state {
    %s *user;                /* User parse state */
    bool *backtrack;         /* Backtracking allowed */
    struct _parser_error_info error;
};
''' % (self.__user_state_type,))

        fp.write('\n'.join(self.__decls) + '\n\n')
        fp.write('\n'.join(self.__code).replace(' '*8, '\t') + '\n')

    @contextlib.contextmanager
    def func(self, decl):
        # XXX Now private?
        self.__decls.append(decl + ';')
        self(decl, '{')
        self.__indent += 1
        yield
        self.__indent -= 1
        self('}', '')

    def __call__(self, *lines):
        self.__code.extend('    ' * self.__indent + line
                           for line in '\n'.join(lines).split('\n'))

    def call(self, pexpr):
        return '%s (s, pos)' % self.__fnames[pexpr]

    def declare_start(self, pexpr, func_name, static=True):
        decl = '%sbool\n%s (const char *str, %s *user)' % \
               ('static ' if static else '', func_name, self.__user_state_type)
        with self.func(decl):
            self('struct _parse_state state = {user, NULL, {NULL, 0, 0}};',
                 'Utf8Iterator pos (str);',
                 'return %s (&state, pos);' % self.__fnames[pexpr])

    def fail_if_end(self, msg):
        self('if (pos == Utf8Iterator ()) return %s;' % self.expected(msg))

    def save(self, errors=False):
        self('Utf8Iterator saved_pos = pos;',
             '%s::save saved_user (s->user);' % self.__user_state_type)
        if errors:
            self('++s->error.disable;')

    def restore(self, errors=False):
        self('pos = saved_pos;',
             'saved_user.restore (s->user);')
        if errors:
            self('--s->error.disable;')

    def expected(self, msg):
        return '_parser_error (&s->error, pos.raw (), %#x)' % \
            self.__errors.setdefault(msg, len(self.__errors))

def cstring(text):
    text = re.sub(b'[\x00-\x1f"\\\\\x7f-\xff]',
                  lambda m: ('\\%03o' % ord(m.group(0))).encode('utf8'),
                  text.encode('utf8'))
    return '"' + text.decode('ascii') + '"'

class Lit(PExpr):
    """Parse 'text' case-insensitively."""
    def __init__(self, text):
        super().__init__()
        self.__text = text
    def gen_c(self, g):
        g.fail_if_end(self.__text)
        if len(self.__text) == 1:
            g('if (*pos != %d)' % ord(self.__text))
        else:
            g('if (strncasecmp (pos.raw(), %s, %d) != 0)' %
              (cstring(self.__text), len(self.__text.encode('utf8'))))
        g('    return %s;' % g.expected(self.__text))
        g('%sreturn true;' % ('++pos; ' * len(self.__text)))

class CharClass(PExpr):
    """Parse a Unicode code point satisfying the C expression expr.

    In expr, the variable 'c' identifies the code point.
    """
    def __init__(self, expr):
        super().__init__()
        self.__expr = expr
    def gen_c(self, g):
        g.fail_if_end(self.__expr)
        g('unsigned c = *pos;',
          'if (%s) { ++pos; return true; } else return %s;' %
          (self.__expr, g.expected(self.__expr)))

class Seq(PExpr):
    """Parse a sequence of productions."""
    def gen_c(self, g):
        g.save()
        g('if (%s) return true;' % ' && '.join(map(g.call, self.productions)))
        g.restore()
        g('return false;')

class Alt(PExpr):
    """Parse one of productions, trying each in order."""
    def __init__(self, *productions):
        super().__init__(*productions)
        self._has_cut = False
        if isinstance(productions[-1], PExpr) and productions[-1]._has_cut:
            raise ValueError('Last alternate must not have a cut')
    def gen_c(self, g):
        if not any(p._has_cut for p in self.productions if isinstance(p, PExpr)):
            g('return %s;' % ' || '.join(map(g.call, self.productions)))
            return
        g('bool *saved_backtrack = s->backtrack;',
          'bool backtrack = true;',
          's->backtrack = &backtrack;',
          'bool r;')
        for p in self.productions[:-1]:
            g('if ((r = %s) || ! backtrack) goto DONE;' % g.call(p))
        g('r = %s;' % g.call(self.productions[-1]),
          'DONE:',
          's->backtrack = saved_backtrack;',
          'return r;')

class Cut(PExpr):
    """Commit to this branch of the containing Alt.  Consumes nothing."""
    def __init__(self):
        super().__init__()
        self._has_cut = True
    def gen_c(self, g):
        g('(void)pos;', '*s->backtrack = false;', 'return true;')

class Many(AutoSeq):
    """Parse zero or more occurrences of productions."""
    def gen_c(self, g):
        g('while (%s);' % g.call(self._production), 'return true;')

def Many1(*productions):
    """Parse one or more occurrences of productions."""
    return Seq(*(productions + (Many(*productions),)))

class Optional(AutoSeq):
    """Parse zero or one occurrences of productions."""
    def gen_c(self, g):
        g('%s;' % g.call(self._production), 'return true;')

class Lookahead(AutoSeq):
    """Succeeds if productions succeed, but consumes nothing."""
    def gen_c(self, g):
        g.save(errors=True)
        g('if (! %s) return false;' % g.call(self._production))
        g.restore(errors=True)
        g('return true;')

class NotLookahead(AutoSeq):
    """Succeeds if productions fail, but consumes nothing."""
    def gen_c(self, g):
        g.save(errors=True)
        g('if (! %s) return true;' % g.call(self._production))
        g.restore(errors=True)
        g('return false;')

class End(PExpr):
    """Succeeds if there is no more input.  Consumes nothing."""
    def __init__(self):
        super().__init__()
    def gen_c(self, g):
        g('return (pos == Utf8Iterator()) || %s;' % g.expected('end'))

class Node(AutoSeq):
    """Like Seq, but creates a new Node object.

    The Node object will have the type given by 'typ'.  Descendant
    Nodes in the abstract parse tree will be added as children.

    If promote_unit is True, then if the node has only a single child,
    return that child instead of the new node.
    """
    def __init__(self, typ, *productions, promote_unit=False):
        super().__init__(*productions)
        self.__typ, self.__promote_unit = typ, promote_unit
    def gen_c(self, g):
        g('_notmuch_qnode_t *parent = s->user->node,',
          '    *node = _notmuch_qnode_create (parent, %s, NULL, NULL);' % self.__typ,
          # XXX Handle allocation failure
          's->user->node = node;',
          'bool result = %s;' % g.call(self._production),
          's->user->node = parent;',
          'if (! result) {',
          '    talloc_free (node);')
        if self.__promote_unit:
            g('} else if (node->nchild == 1) {',
              '    talloc_steal (parent, node->child[0]);',
              '    _notmuch_qnode_add_child (parent, node->child[0], NULL);',
              '    talloc_free (node);')
        g('} else {',
          '    _notmuch_qnode_add_child (parent, node, NULL);',
          # XXX Handle add failure
          '}',
          'return result;')

class Text(AutoSeq):
    """Parse production and set the text of the current Node to its match."""
    def gen_c(self, g):
        g('const char *start = pos.raw();',
          'if (! %s) return false;' % g.call(self._production),
          's->user->node->text = talloc_strndup (',
          '    s->user->node, start, pos.raw () - start);',
          'return true;')

def KW(text):
    return Seq(Lit(text), '__')

g = Grammar('_user_state')
g.rules(
    root      = Seq('_', 'andExpr', End()),

    # XXX Do we need to have (and support) a cut after 'and'?
    andExpr   = Node('NODE_AND', 'orExpr', Many(KW('and'), 'orExpr'),
                     promote_unit=True),
    orExpr    = Node('NODE_OR', 'unaryExpr', Many(KW('or'), 'unaryExpr'),
                     promote_unit=True),
    # XXX Will often allocate node and then backtrack
    unaryExpr = Alt(Node('NODE_NOT', KW('not'), Cut(), 'unaryExpr'),
                    'group'),

    # A group is a sequence of possibly-loved/hated possibly-labeled
    # terms and subqueries.  We stop consuming if we hit a bare
    # operator, but note that we intentionally accept text that looks
    # like an operator further down in the grammar if it's loved/hated
    # or labeled.
    #
    # XXX Peephole optimization of removing GROUP for HATE-only terms
    group  = Node('NODE_GROUP',
                  Many1(NotLookahead(Alt(KW('and'), KW('or'), KW('not'))),
                        'loveHate')),
    # A love prefix has no effect, since the default group operator is
    # AND anyway.  Hates are like NOTs, just with lower precedence.
    #
    # Xapian treats a hate following an AND like "x AND -y z" as "x
    # AND NOT y z", which causes *both* y and z to be negated, rather
    # than just y.  We don't special case this.
    #
    # Xapian also ignores +/- unless preceded by whitespace or an open
    # paren, which has the effect of ignoring all +'s and -'s in "x
    # +++y", "x#+y", "(x)+y", and "-x".  We don't discriminate.
    #
    # XXX In the hand-parser, we ignore if followed by a space or
    # another + or -, but I'm not sure why.
    loveHate  = Alt(Node('NODE_NOT', Lit('-'), Cut(), 'frag'),
                    Seq(Optional(Lit('+')), 'frag')),
    frag      = Alt(Node('NODE_LABEL', 'label', 'term'),
                    'term'),
    # A label is a sequence of word characters followed by a colon.
    # Xapian allows anything except colon and whitespace, but
    # restricts to registered labels.  Our syntax is not sensitive to
    # the set of registered labels, so we're more restrictive in the
    # accepted characters.
    #
    # In Xapian, labels for boolean prefixes dramatically affect the
    # accepted lexical grammar of the following term.  We make no
    # distinction; a label is a label.  This is okay because we parse
    # the term itself much like how Xapian lexes boolean terms anyway
    # (and term splitting happens later, unlike in Xapian where it
    # happens during parsing).
    label     = Seq(Text(Many1(CharClass('is_wordchar (c)'))), Lit(':')),

    # XXX Is the lexing of boolean terms compatible with the existing
    # quoting that we do for boolean terms?

    # Xapian ignores '(' unless preceded by whitespace, parens, +, or
    # -.  We don't discriminate.
    term      = Alt(Seq(Lit('('), Cut(), '_', 'andExpr', Lit(')'), '_'),
                    # XXX Will often allocate node and then backtrack
                    Node('NODE_TERMS',
                         Lit('"'), Cut(), Text('quoted'), Lit('"'), '_'),
                    Seq(Node('NODE_TERMS', 'termText', '__'))),
    # Quotes in a quoted phrase can be escaped by doubling them.
    # Xapian distinguishes between regular phrases that have no way to
    # escape quotes and boolean terms, where quotes are escaped, but
    # we simplify this to a single lexical grammar with escaped
    # quotes.  Xapian also lexes +/-/( in quotes mode in a regular
    # phrase and simply doesn't generate tokens for them.  For us, the
    # term generator will discard them.
    # XXX Unescape
    quoted    = Many(Alt(CharClass('c != \'"\''), Lit('""'))),
    # Consume a term up to the next (, ), ", or whitespace.  We'll
    # word-split this much later, during generation.
    #
    # Xapian permits other characters to separate term phrases.  For
    # example, "x#y" is parsed as two separate (non-phrase) terms.
    # However, because the characters allowed in a term are
    # context-sensitive, this is quite unpredictable and replicating
    # this is very hard.  Here we take a simpler approach where only
    # whitespace and a few operator characters that are never term
    # characters separate terms.
    termText  = Text(Many1(CharClass(
        "!(c == '(' || c == ')' || c == '\"' || is_whitespace (c))"))),

    _ = Many(CharClass('is_whitespace (c)')),
    __ = Alt(Many1(CharClass('is_whitespace (c)')),
             Lookahead(Lit('(')),
             Lookahead(Lit(')')),
             End()))
g.declare_start('root', 'parse')

print('''\
/* Automatically generated.  DO NOT EDIT */

#include "qparser.h"
#include "notmuch-private.h"

#include <xapian.h>

using namespace Xapian::Unicode;

// XXX Pretty lame use of C++
struct _user_state {
    _notmuch_qnode_t *node;
    struct save {
	size_t nchild;
	save (const _user_state *u) : nchild (u->node ? u->node->nchild : 0) {}
	void restore (_user_state *u)
	{
	    if (u->node)
		u->node->nchild = nchild;
	    // XXX Free dangling children
	}
    };
};
''')
g.write_to(sys.stdout)
print('''\
_notmuch_qnode_t *
_notmuch_qparser_parse (const void *ctx, const char *query,
			const char **error_out)
{
    struct _user_state user;
    _notmuch_qnode_t *result = NULL;
    /* Create a dummy root node */
    /* XXX error_out */
    user.node = _notmuch_qnode_create (ctx, NODE_AND, NULL, NULL);
    if (! user.node)
	return NULL;
    /* XXX Will need error message stuff */
    if (! parse (query, &user))
	goto DONE;
    if (user.node->nchild != 1)
	INTERNAL_ERROR ("Wrong number of root children: %s",
			_notmuch_qnode_to_string (ctx, user.node));
    result = talloc_steal (ctx, user.node->child[0]);

  DONE:
    talloc_free (user.node);
    return result;
}''')
