import sys
import re
import contextlib

# XXX Handle system errors (e.g., allocation failure)

# XXX Generate useful messages on parse errors

# XXX Clean up allocations better on backtrack?

# XXX "/" for alt, methods for repeat, optional.  Would require
# wrapping non-terminal references and some introspection to flatten.

# XXX If actions were a separate, non-backtracking pass, would I need
# any return values other than bool in the recognizer rules?

# XXX Alternatively, I could just recursively free the node children I
# unwind.

# Simpler style: g.rule('andExpr', ...).  Don't even have to resolve
# non-terminal names (assuming we don't need type information).  Could
# depend on C compiler to do that.

# Rather than passing around the parse state, the parse actions could
# all be methods of a parse class that could be subclassed.  No, that
# doesn't really help because user-provided code has to operate in
# that subclass.

# Ugh, action tree maintenance is actually really hairy.  Maybe user
# code could just push undo actions onto an undo stack?  Seems like
# overkill, since we really need to save only one number at the unwind
# point and everything else can be derived.

# Even if I have on-the-fly actions, I could move to a boolean-only
# return model by putting a little more in the user state (e.g., the
# last created Node, which would go up the tree until a Child consumed
# it).

class PExpr:
    NONCE = 0

    def __init__(self, gen, typ):
        self.gen, self.typ = gen, typ
        self.fname = '_parse_n%d' % PExpr.NONCE
        PExpr.NONCE += 1

class Writer:
    def __init__(self, grammar, initial):
        self.__grammar = grammar
        self.__decls = []
        self.__code = []
        self.__indent = 0
        self.__queue = []

        self.resolve(initial)
        self.__qpos = 0
        while self.__qpos < len(self.__queue):
            self.__queue[self.__qpos].gen(self)
            self.__qpos += 1

    def write_to(self, fp):
        fp.write('#include <xapian.h>\n')
        fp.write('#include <talloc.h>\n')
        fp.write('#include <strings.h>\n')
        fp.write('using namespace Xapian::Unicode;\n')
        fp.write('using Xapian::Utf8Iterator;\n')
        # XXX
        fp.write('''typedef struct _notmuch_node {
\tconst char *text;
\tsize_t nchild;
\tstruct _notmuch_node **child;
} _notmuch_node_t;
        enum _notmuch_node_type {NODE_AND, NODE_OR, NODE_NOT, NODE_COMPOUND, NODE_PREFIX, NODE_TERMS};
_notmuch_node_t* _notmuch_qparser_node_create (const void *ctx, _notmuch_node_type type);
void _notmuch_qparser_node_add_child (_notmuch_node_t*, _notmuch_node_t*);
#define unused(x) x __attribute__ ((unused))

struct _parse_action {
\tstruct _parse_action *next;
\tsize_t nchild;
\tXXX (*action)(XXX);
};

struct _parse_state {
	/* Pre-order traversal of the action tree. */
	struct _parse_action *action_head, **action_ptail;
	/* The action currently being built. */
	struct _parse_action *action_cur;
	/* Action allocation pool. */
	struct *action_pool;
};
''')

        fp.write('\n'.join(self.__decls) + '\n\n')
        fp.write('\n'.join(self.__code) + '\n')

    @contextlib.contextmanager
    def func(self, decl):
        self.__decls.append(decl + ';')
        self(decl + ' {')
        self.__indent += 1
        yield
        self.__indent -= 1
        self('}')
        self('')

    @contextlib.contextmanager
    def parser_func(self, save=False, end_fail=False):
        pexpr = self.__queue[self.__qpos]
        decl = 'static %s\n%s (struct _parse_state *s, Utf8Iterator &pos)' % \
               (self.result_type(pexpr), pexpr.fname)
        with self.func(decl):
            if end_fail:
                self('if (pos == Utf8Iterator()) return 0;')
            if save:
                self('Utf8Iterator saved_pos = pos;')
                self('struct _parse_action **saved_tail = s->action_ptail;')
            yield
        # XXX Could put result in a variable and write generic restore
        # code here (if save).  But that does the wrong thing for
        # Lookahead.

    def resolve(self, pexpr):
        if not isinstance(pexpr, PExpr):
            pexpr = getattr(self.__grammar, pexpr)
        if pexpr not in self.__queue:
            self.__queue.append(pexpr)
        return pexpr

    def result_type(self, pexpr):
        typ = self.resolve(pexpr).typ
        if callable(typ): typ = typ(self)
        return typ

    def __call__(self, text):
        self.__code.append('\t' * self.__indent + text)

    def call(self, pexpr):
        return '%s (ctx, pos, node)' % self.resolve(pexpr).fname

    def restore(self):
        self('pos = saved_pos;')
        # Move any actions that were added to the action tree to the
        # action pool to be reused later.
        self('*s->action_ptail = s->action_pool;')
        self('s->action_pool = *saved_ptail;')
        self('*saved_ptail = NULL;')
        self('s->action_ptail = saved_ptail;')

def cstring(text):
    text = re.sub(b'[\x00-\x1f"\\\\\x7f-\xff]',
                  lambda m: ('\\%03o' % ord(m.group(0))).encode('utf8'),
                  text.encode('utf8'))
    return '"' + text.decode('ascii') + '"'

def Lit(text):
    """Parse 'text' case-insensitively.  Evaluates to nothing."""
    def gen(w):
        with w.parser_func(end_fail=True):
            if len(text) == 1:
                w('if (*pos != %d) return false;' % ord(text))
            else:
                w('if (strncasecmp (pos.raw(), %s, %d) != 0) return false;' %
                  (cstring(text), len(text.encode('utf8'))))
            w('%s return true;' % ('++pos; ' * len(text)))
    return PExpr(gen, 'bool')

def CharClass(expr):
    """Parse a Unicode code point satisfying the C expression expr.

    In expr, the variable 'c' identifies the code point.
    """
    def gen(w):
        with w.parser_func(end_fail=True):
            w('unsigned c = *pos;')
            w('if (%s) {++pos; return true;} else return false;' % expr)
    return PExpr(gen, 'bool')

def Seq(*productions, n=-1):
    """Parse a sequence of productions.  Evaluates to the Nth production."""
    if len(productions) == 1:
        return productions[n]
    def gen(w):
        with w.parser_func(save=True):
            w('%s result;' % w.result_type(productions[n]))
            exprs = list(map(w.call, productions))
            exprs = '(result = %s)' % exprs[n]
            w('if (%s) return result;' % ' && '.join(map(w.call, productions)))
            w.restore()
            w('return 0;')
    return PExpr(gen, lambda w: w.result_type(productions[n]))

def Alt(*productions):
    """Parse one of productions, trying each in order.

    Evaluates to the value of the successful production.
    """
    def gen(w):
        with w.parser_func():
            w('%s result;' % w.result_type(productions[0]))
            for p in productions[:-1]:
                w('if ((result = %s)) return result;' % w.call(p))
            w('return %s;' % w.call(productions[-1]))
    return PExpr(gen, lambda w: w.result_type(productions[0]))

def ZeroPlus(*productions):
    """Parse zero or more occurrences of productions.  Evaluates to nothing."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func():
            w('while (%s); return true;' % w.call(production))
    return PExpr(gen, 'bool')

def OnePlus(*productions):
    """Equivalent to Seq(*productions, ZeroPlus(*productions))."""
    return Seq(*(productions + (ZeroPlus(*productions),)))

def Optional(*productions):
    """Parse zero or one occurrences of productions.  Evaluates to nothing."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func():
            w('%s; return true;' % w.call(production))
    return PExpr(gen, 'bool')

def Lookahead(*productions):
    """Succeeds if Seq(*productions) succeeds, but consume nothing."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func(save=True):
            w('if (!%s) return false;' % w.call(production))
            w.restore()
            w('return true;')
    return PExpr(gen, 'bool')

def NotLookahead(*productions):
    """Succeeds if Seq(*productions) fails, but consume nothing."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func(save=True):
            w('if (!%s) return true;' % w.call(production))
            w.restore()
            w('return false;')
    return PExpr(gen, 'bool')

def End():
    """Succeed if there is no more input.  Consumes nothing."""
    def gen(w):
        with w.parser_func():
            w('return (pos == Utf8Iterator());')
    return PExpr(gen, 'bool')

def Action(*productions, before=None, after=None):
    production = Seq(*productions)
    def gen(w):
        # Declare action function
        action_fname = production.fname + '_action'
        with w.func('static XXX\n%s (XXX)' % action_fname):
            if before:
                w(before)
            w('struct _parse_action *my_action = s->action_head;')
            w('for (size_t i = 0; i < my_action->nchild; ++i) {')
            w('	s->action_head = s->action_head->next;')
            w(' s->action_head->action(XXX);')
            w('}')
            if after:
                w(after)
        with w.parser_func(save=True):
            # Allocate and link in the action
            w('struct _parse_action *action = s->action_pool;')
            w('if (action) {')
            w('	s->action_pool = action->next;')
            w('} else {')
            w('	action = talloc (s, struct _parse_action);')
            # XXX Handle allocation failure
            w('}')
            w('*s->action_ptail = action;')
            w('s->action_ptail = &action->next;')
            w('action->next = NULL;')
            w('action->nchild = 0;')
            w('action->action = %s;' % action_fname)

            # Update parser state
            # XXX Need a root action
            w('struct _parse_action *parent = s->action_cur;')
            w('++parent->nchild;')
            w('s->action_cur = action;')

            # Parse the child
            w('bool result = %s;' % w.call(production))

            # Unwind
            w('s->action_cur = parent;')
            w('if (result) return true;')
            w.restore()
            w('return false;')
    return PExpr(gen, 'bool')

def Node(typ, *productions, promote_unit=False):
    """Like Seq, but evaluates to a new Node object.

    The Node object will have the type given by 'typ' and its children
    will be the productions marked with Child under the Node production.

    If promote_unit is True, then if the node has only a single child,
    return that child instead of the new node.
    """
    # XXX It would be nice if the state required for this weren't so
    # baked in to the core parser generator.  The state could be
    # generalized by subclassing the general parser state and here we
    # could save and restore the current node (keeping it in the state
    # may be more efficient than passing it to every function, though
    # if the compiler's smart, it'll just keep it in the right
    # register.)  Alternatively, the actions could be a second pass,
    # which would also have the advantage of never backtracking.
    production = Seq(*productions)
    def gen(w):
        with w.parser_func():
            w('node = _notmuch_qparser_node_create (ctx, %s);' % typ)
            w('if (%s) {' % w.call(production))
            if promote_unit:
                w('\tif (node->nchild == 1) return node->child[0];')
            w('\treturn node;')
            w('}')
            w('talloc_free (node);')
            w('return 0;')
    return PExpr(gen, '_notmuch_node_t *')

def Child(*productions):
    """Parse production and add its result to the current Node."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func():
            w('_notmuch_node_t *val = %s;' % w.call(production))
            w('if (val) _notmuch_qparser_node_add_child (node, val);')
            w('return !! val;')
    return PExpr(gen, 'bool')

def Text(*productions):
    """Parse production and set the text of the current Node to its match."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func():
            w('const char *start = pos.raw();')
            w('if (! %s) return false;' % w.call(production))
            w('node->text = talloc_strndup (node, start, pos.raw () - start);')
            w('return true;')
    return PExpr(gen, 'bool')

def KW(text):
    return Seq(Lit(text), '__')

class Grammar:
    root      = Seq('_', 'andExpr')

    andExpr   = Node('NODE_AND',
                     Child('orExpr'), ZeroPlus(KW('and'), Child('orExpr')),
                     promote_unit=True)
    orExpr    = Node('NODE_OR',
                     Child('unaryExpr'), ZeroPlus(KW('or'), Child('unaryExpr')),
                     promote_unit=True)
    unaryExpr = Alt(Node('NODE_NOT', KW('not'), '__', 'unaryExpr'),
                    'compound')

    # A compound is a sequence of possibly-loved/hated
    # possibly-prefixed terms and subqueries.  We stop consuming if we
    # hit a bare operator, but note that we intentionally accept text
    # that looks like an operator further down in the grammar if it's
    # loved/hated or prefixed.
    #
    # XXX Peephole optimization of removing COMPOUND for HATE-only terms
    compound  = Node('NODE_COMPOUND',
                     ZeroPlus(NotLookahead(Alt(KW('and'), KW('or'), KW('not'))),
                              Child('loveHate')))
    # A love prefix has no effect, since the default compound operator
    # is AND anyway.  Hates are like NOTs, just with lower precedence.
    #
    # Xapian treats a hate following an AND like "x AND -y z" as "x
    # AND NOT y z", which causes *both* y and z to be negated, rather
    # than just y.  We don't special case this.
    #
    # Xapian also ignores +/- unless preceded by whitespace or an open
    # paren, which has the effect of ignoring all +'s in "x +++y",
    # "x#+y", and "(x)+y".  We don't discriminate.
    #
    # XXX In the hand-parser, we ignore if followed by a space or
    # another + or -, but I'm not sure why.
    loveHate  = Alt(Node('NODE_NOT', Lit('-'), Child('frag')),
                    Seq(Optional(Lit('+')), 'frag'))
    frag      = Alt(Node('NODE_PREFIX', 'prefix', Child('term')),
                    'term')
    # A prefix is a sequence of word characters followed by a colon.
    # Xapian allows anything except colon and whitespace, but
    # restricts to registered prefixes.  Our syntax is not sensitive
    # to the set of registered prefixes, so we're more restrictive in
    # the accepted characters.
    #
    # In Xapian, boolean prefixes dramatically affect the accepted
    # lexical grammar of the following term.  We make no distinction;
    # a prefix is a prefix.  This is okay because we parse the term
    # itself much like how Xapian lexes boolean terms anyway (and term
    # splitting happens later, unlike in Xapian where it happens
    # during parsing).
    prefix    = Seq(Text(OnePlus(CharClass('is_wordchar(c)'))),
                    Lit(':'))

    # XXX Is the lexing of boolean terms compatible with the existing
    # quoting that we do for boolean terms?

    # Xapian ignores '(' unless preceded by whitespace, parens, +, or
    # -.  We don't discriminate.
    term      = Alt(Seq(Lit('('), '_', 'andExpr', Lit(')'), '_', n=2),
                    Node('NODE_TERMS', Lit('"'), Text('quoted'), Lit('"'), '_'),
                    Node('NODE_TERMS', termText, '__'))
    # Quotes in a quoted phrase can be escaped by doubling them.
    # Xapian distinguishes between regular phrases that have no way to
    # escape quotes and boolean terms, where quotes are escaped, but
    # we simplify this to a single lexical grammar with escaped
    # quotes.  Xapian also lexes +/-/( in quotes mode in a regular
    # phrase and simply doesn't generate tokens for them.  For us, the
    # term generator will discard them.
    # XXX Unescape
    quoted    = ZeroPlus(Alt(CharClass('c != \'"\''), Lit('""')))
    # Consume a (possibly empty) term up to the next (, ) or ".  We'll
    # word-split this much later, during generation.
    #
    # Xapian permits other characters to separate term phrases.  For
    # example, "x#y" is parsed as two separate (non-phrase) terms.
    # However, because the characters allowed in a term are
    # context-sensitive, this is quite unpredictable and replicating
    # this is very hard.  Here we take a simpler approach where only
    # whitespace and a few operator characters that are never term
    # characters separate terms.
    termText  = Text(ZeroPlus(CharClass("!(c == '(' || c == ')' || c == '\"')")))

    _ = ZeroPlus(CharClass('is_whitespace(c)'))
    __ = Alt(OnePlus(CharClass('is_whitespace(c)')),
             Lookahead(Lit('(')),
             Lookahead(Lit(')')),
             End())

Writer(Grammar, Grammar.root).write_to(sys.stdout)
