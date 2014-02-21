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

    def __init__(self, gen):
        self.gen = gen
        self.fname = '_parse_n%d' % PExpr.NONCE
        PExpr.NONCE += 1

class Grammar:
    def __init__(self, user_state_type):
        self.__user_state_type = user_state_type
        self.__rules = {}
        self.__decls = []
        self.__code = []
        self.__indent = 0
        self.__queue = []
        self.__qpos = 0

    def rules(self, **rules):
        self.__rules.update(rules)
        self.__queue.extend(rules.values())
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
    const char *text;
    size_t nchild;
    struct _notmuch_node **child;
} _notmuch_node_t;
enum _notmuch_node_type {NODE_AND, NODE_OR, NODE_NOT, NODE_COMPOUND, NODE_PREFIX, NODE_TERMS};
_notmuch_node_t* _notmuch_qparser_node_create (const void *ctx, _notmuch_node_type type);
void _notmuch_qparser_node_add_child (_notmuch_node_t*, _notmuch_node_t*);
#define unused(x) x __attribute__ ((unused))

struct _user_state {
    _notmuch_node_t *node;
    struct save {
        size_t nchild;
        save (const _user_state *u) : nchild (u->node ? u->node->nchild : 0) {}
        void restore (_user_state *u) {
            if (u->node)
                u->node->nchild = nchild;
            // XXX Free dangling children
        }
    };
};

struct _parse_state {
    %s user;                /* User parse state */
};
''' % self.__user_state_type)

        fp.write('\n'.join(self.__decls) + '\n\n')
        fp.write('\n'.join(self.__code).replace(' '*8, '\t') + '\n')

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
        # We could make these methods of the parser state, which would
        # be nice and OO, but by declaring all parser functions as
        # static, the compiler is able to inline the vast majority of
        # these functions and doesn't have to emit non-inlined
        # versions at all.
        decl = ('static bool\n'
                '%s (unused (struct _parse_state *s), Utf8Iterator &pos)' % \
                (pexpr.fname))
        with self.func(decl):
            if end_fail:
                self('if (pos == Utf8Iterator ()) return false;')
            if save:
                self('Utf8Iterator saved_pos = pos;')
                self('%s::save saved_user (&s->user);' % self.__user_state_type)
            yield
        # XXX Could put result in a variable and write generic restore
        # code here (if save).  But that does the wrong thing for
        # Lookahead.

    def resolve(self, pexpr):
        if not isinstance(pexpr, PExpr):
            pexpr = self.__rules[pexpr]
        if pexpr not in self.__queue:
            self.__queue.append(pexpr)
        return pexpr

    def __call__(self, text):
        self.__code.append('    ' * self.__indent + text)

    def call(self, pexpr):
        return '%s (s, pos)' % self.resolve(pexpr).fname

    def restore(self):
        self('pos = saved_pos;')
        self('saved_user.restore (&s->user);')

def cstring(text):
    text = re.sub(b'[\x00-\x1f"\\\\\x7f-\xff]',
                  lambda m: ('\\%03o' % ord(m.group(0))).encode('utf8'),
                  text.encode('utf8'))
    return '"' + text.decode('ascii') + '"'

def Lit(text):
    """Parse 'text' case-insensitively."""
    def gen(w):
        with w.parser_func(end_fail=True):
            if len(text) == 1:
                w('if (*pos != %d) return false;' % ord(text))
            else:
                w('if (strncasecmp (pos.raw(), %s, %d) != 0) return false;' %
                  (cstring(text), len(text.encode('utf8'))))
            w('%s return true;' % ('++pos; ' * len(text)))
    return PExpr(gen)

def CharClass(expr):
    """Parse a Unicode code point satisfying the C expression expr.

    In expr, the variable 'c' identifies the code point.
    """
    def gen(w):
        with w.parser_func(end_fail=True):
            w('unsigned c = *pos;')
            w('if (%s) {++pos; return true;} else return false;' % expr)
    return PExpr(gen)

def Seq(*productions):
    """Parse a sequence of productions."""
    if len(productions) == 1:
        return productions[0]
    def gen(w):
        with w.parser_func(save=True):
            w('if (%s) return true;' % ' && '.join(map(w.call, productions)))
            w.restore()
            w('return false;')
    return PExpr(gen)

def Alt(*productions):
    """Parse one of productions, trying each in order."""
    def gen(w):
        with w.parser_func():
            w('return %s;' % ' || '.join(map(w.call, productions)))
    return PExpr(gen)

def ZeroPlus(*productions):
    """Parse zero or more occurrences of productions."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func():
            w('while (%s); return true;' % w.call(production))
    return PExpr(gen)

def OnePlus(*productions):
    """Equivalent to Seq(*productions, ZeroPlus(*productions))."""
    return Seq(*(productions + (ZeroPlus(*productions),)))

def Optional(*productions):
    """Parse zero or one occurrences of productions."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func():
            w('%s; return true;' % w.call(production))
    return PExpr(gen)

def Lookahead(*productions):
    """Succeeds if Seq(*productions) succeeds, but consume nothing."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func(save=True):
            w('if (!%s) return false;' % w.call(production))
            w.restore()
            w('return true;')
    return PExpr(gen)

def NotLookahead(*productions):
    """Succeeds if Seq(*productions) fails, but consume nothing."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func(save=True):
            w('if (!%s) return true;' % w.call(production))
            w.restore()
            w('return false;')
    return PExpr(gen)

def End():
    """Succeeds if there is no more input.  Consumes nothing."""
    def gen(w):
        with w.parser_func():
            w('return (pos == Utf8Iterator());')
    return PExpr(gen)

def Node(typ, *productions, promote_unit=False):
    """Like Seq, but creates a new Node object.

    The Node object will have the type given by 'typ'.  Descendant
    Nodes in the abstract parse tree will be added as children.

    If promote_unit is True, then if the node has only a single child,
    return that child instead of the new node.
    """
    production = Seq(*productions)
    def gen(w):
        with w.parser_func():
            w('_notmuch_node_t *parent = s->user.node,')
            w('    *node = _notmuch_qparser_node_create (parent, %s);' % typ)
            # XXX Handle allocation failure
            w('s->user.node = node;')
            w('bool result = %s;' % w.call(production))
            w('s->user.node = parent;')
            w('if (! result) {')
            w('    talloc_free (node);')
            if promote_unit:
                w('} else if (node->nchild == 1) {')
                w('    _notmuch_qparser_node_add_child (parent, node->child[0]);')
                w('    talloc_free (node);')
            w('} else {')
            w('    _notmuch_qparser_node_add_child (parent, node);')
            # XXX Handle add failure
            # XXX Need root node or something
            w('}')
            w('return result;')
    return PExpr(gen)

def Text(*productions):
    """Parse production and set the text of the current Node to its match."""
    production = Seq(*productions)
    def gen(w):
        with w.parser_func():
            w('const char *start = pos.raw();')
            w('if (! %s) return false;' % w.call(production))
            w('s->user.node->text = talloc_strndup (s->user.node, start, pos.raw () - start);')
            w('return true;')
    return PExpr(gen)

def KW(text):
    return Seq(Lit(text), '__')

g = Grammar('_user_state')
g.rules(
    root      = Seq('_', 'andExpr'),

    andExpr   = Node('NODE_AND', 'orExpr', ZeroPlus(KW('and'), 'orExpr'),
                     promote_unit=True),
    orExpr    = Node('NODE_OR', 'unaryExpr', ZeroPlus(KW('or'), 'unaryExpr'),
                     promote_unit=True),
    unaryExpr = Alt(Node('NODE_NOT', KW('not'), '__', 'unaryExpr'),
                    'compound'),

    # A compound is a sequence of possibly-loved/hated
    # possibly-prefixed terms and subqueries.  We stop consuming if we
    # hit a bare operator, but note that we intentionally accept text
    # that looks like an operator further down in the grammar if it's
    # loved/hated or prefixed.
    #
    # XXX Peephole optimization of removing COMPOUND for HATE-only terms
    compound  = Node('NODE_COMPOUND',
                     ZeroPlus(NotLookahead(Alt(KW('and'), KW('or'), KW('not'))),
                              'loveHate')),
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
    loveHate  = Alt(Node('NODE_NOT', Lit('-'), 'frag'),
                    Seq(Optional(Lit('+')), 'frag')),
    frag      = Alt(Node('NODE_PREFIX', 'prefix', 'term'),
                    'term'),
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
                    Lit(':')),

    # XXX Is the lexing of boolean terms compatible with the existing
    # quoting that we do for boolean terms?

    # Xapian ignores '(' unless preceded by whitespace, parens, +, or
    # -.  We don't discriminate.
    term      = Alt(Seq(Lit('('), '_', 'andExpr', Lit(')'), '_'),
                    Node('NODE_TERMS', Lit('"'), Text('quoted'), Lit('"'), '_'),
                    Node('NODE_TERMS', 'termText', '__')),
    # Quotes in a quoted phrase can be escaped by doubling them.
    # Xapian distinguishes between regular phrases that have no way to
    # escape quotes and boolean terms, where quotes are escaped, but
    # we simplify this to a single lexical grammar with escaped
    # quotes.  Xapian also lexes +/-/( in quotes mode in a regular
    # phrase and simply doesn't generate tokens for them.  For us, the
    # term generator will discard them.
    # XXX Unescape
    quoted    = ZeroPlus(Alt(CharClass('c != \'"\''), Lit('""'))),
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
    termText  = Text(ZeroPlus(
        CharClass("!(c == '(' || c == ')' || c == '\"')"))),

    _ = ZeroPlus(CharClass('is_whitespace(c)')),
    __ = Alt(OnePlus(CharClass('is_whitespace(c)')),
             Lookahead(Lit('(')),
             Lookahead(Lit(')')),
             End()))

g.write_to(sys.stdout)
