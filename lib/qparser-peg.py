import sys
import re

# XXX Handle system errors (e.g., allocation failure)

# XXX Generate useful messages on parse errors

# XXX Clean up allocations better on backtrack?

# XXX "/" for alt, methods for repeat, optional.  Would require
# wrapping non-terminal references.

# XXX If actions were a separate, non-backtracking pass, would I need
# any return values other than bool in the recognizer rules?

# Simpler style: g.rule('andExpr', ...).  Don't even have to resolve
# non-terminal names (assuming we don't need type information).  Could
# depend on C compiler to do that.

class PExpr:
    NONCE = 0

    def __init__(self, gen, typ, save=False, end_fail=False):
        self.gen, self.typ, self.save, self.end_fail = gen, typ, save, end_fail
        self.fname = '_parse_n%d' % PExpr.NONCE
        PExpr.NONCE += 1

    def gen_code(self, w):
        decl = 'static %s\n%s (unused (const void *ctx), Utf8Iterator &pos, unused (_notmuch_node_t *node))' % \
               (w.result_type(self), self.fname)

        w.declare(decl + ';')
        w(decl + ' {')
        w.indent(1)
        if self.end_fail:
            w('if (pos == Utf8Iterator()) return 0;')
        if self.save:
            w('Utf8Iterator saved_pos = pos;')
            w('size_t saved_nchild = node ? node->nchild : 0;')
        self.gen(w)
        w.indent(-1)
        w('}')
        w('')

class Writer:
    def __init__(self, grammar, initial):
        self.__grammar = grammar
        self.__decls = []
        self.__code = []
        self.__indent = 0
        self.__queue = []

        self.resolve(initial)
        qpos = 0
        while qpos < len(self.__queue):
            self.__queue[qpos].gen_code(self)
            qpos += 1

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
''')

        fp.write('\n'.join(self.__decls) + '\n\n')
        fp.write('\n'.join(self.__code) + '\n')

    def indent(self, by=1):
        self.__indent += by

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

    def declare(self, text):
        self.__decls.append(text)

    def __call__(self, text):
        self.__code.append('\t' * self.__indent + text)

    def call(self, pexpr):
        return '%s (ctx, pos, node)' % self.resolve(pexpr).fname

    def restore(self):
        self('pos = saved_pos;')
        self('if (node) node->nchild = saved_nchild;')

def cstring(text):
    text = re.sub(b'[\x00-\x1f"\\\\\x7f-\xff]',
                  lambda m: ('\\%03o' % ord(m.group(0))).encode('utf8'),
                  text.encode('utf8'))
    return '"' + text.decode('ascii') + '"'

def Lit(text):
    """Parse 'text' case-insensitively.  Evaluates to nothing."""
    def gen(w):
        if len(text) == 1:
            w('if (*pos != %d) return false;' % ord(text))
        else:
            w('if (strncasecmp (pos.raw(), %s, %d) != 0) return false;' %
              (cstring(text), len(text.encode('utf8'))))
        w('%s return true;' % ('++pos; ' * len(text)))
    return PExpr(gen, 'bool', end_fail=True)

def CharClass(expr):
    """Parse a Unicode code point satisfying the C expression expr.

    In expr, the variable 'c' identifies the code point.
    """
    def gen(w):
        w('unsigned c = *pos;')
        w('if (%s) {++pos; return true;} else return false;' % expr)
    return PExpr(gen, 'bool', end_fail=True)

def Seq(*productions, n=-1):
    """Parse a sequence of productions.  Evaluates to the Nth production."""
    if len(productions) == 1:
        return productions[n]
    def gen(w):
        w('%s result;' % w.result_type(productions[n]))
        exprs = list(map(w.call, productions))
        exprs = '(result = %s)' % exprs[n]
        w('if (%s) return result;' % ' && '.join(map(w.call, productions)))
        w.restore()
        w('return 0;')
    return PExpr(gen, lambda w: w.result_type(productions[n]), save=True)

def Alt(*productions):
    """Parse one of productions, trying each in order.

    Evaluates to the value of the successful production.
    """
    def gen(w):
        w('%s result;' % w.result_type(productions[0]))
        for p in productions[:-1]:
            w('if ((result = %s)) return result;' % w.call(p))
        w('return %s;' % w.call(productions[-1]))
    return PExpr(gen, lambda w: w.result_type(productions[0]))

def ZeroPlus(*productions):
    """Parse zero or more occurrences of productions.  Evaluates to nothing."""
    production = Seq(*productions)
    def gen(w):
        w('while (%s); return true;' % w.call(production))
    return PExpr(gen, 'bool')

def OnePlus(*productions):
    """Equivalent to Seq(*productions, ZeroPlus(*productions))."""
    return Seq(*(productions + (ZeroPlus(*productions),)))

def Optional(*productions):
    """Parse zero or one occurrences of productions.  Evaluates to nothing."""
    production = Seq(*productions)
    def gen(w):
        w('%s; return true;' % w.call(production))
    return PExpr(gen, 'bool')

def Lookahead(*productions):
    """Succeeds if Seq(*productions) succeeds, but consume nothing."""
    production = Seq(*productions)
    def gen(w):
        w('if (!%s) return false;' % w.call(production))
        w.restore()
        w('return true;')
    return PExpr(gen, 'bool', save=True)

def NotLookahead(*productions):
    """Succeeds if Seq(*productions) fails, but consume nothing."""
    production = Seq(*productions)
    def gen(w):
        w('if (!%s) return true;' % w.call(production))
        w.restore()
        w('return false;')
    return PExpr(gen, 'bool', save=True)

def End():
    """Succeed if there is no more input.  Consumes nothing."""
    def gen(w):
        w('return (pos == Utf8Iterator());')
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
        w('node = _notmuch_qparser_node_create (ctx, %s);' % typ)
        w('if (%s) {' % w.call(production))
        if promote_unit:
            w('\tif (node->nchild == 1) return node->child[0];')
        w('\treturn node;')
        w('}')
        w('talloc_free (node); node = NULL;')
        w.restore()
        w('return 0;')
    return PExpr(gen, '_notmuch_node_t *', save=True)

def Child(*productions):
    """Parse production and add its result to the current Node."""
    production = Seq(*productions)
    def gen(w):
        w('_notmuch_node_t *val = %s;' % w.call(production))
        w('if (val) _notmuch_qparser_node_add_child (node, val);')
        w('return !! val;')
    return PExpr(gen, 'bool')

def Text(*productions):
    """Parse production and set the text of the current Node to its match."""
    production = Seq(*productions)
    def gen(w):
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

    # XXX Peephole optimization of removing COMPOUND for HATE-only terms
    compound  = Node('NODE_COMPOUND',
                     ZeroPlus(NotLookahead(Alt(KW('and'), KW('or'), KW('not'))),
                              Child('loveHate')))
    loveHate  = Alt(Node('NODE_NOT', Lit('-'), Child('frag')),
                    Seq(Optional(Lit('+')), 'frag'))
    frag      = Alt(Node('NODE_PREFIX', 'prefix', Child('term')),
                    'term')
    prefix    = Seq(Text(OnePlus(CharClass('is_wordchar(c)'))),
                    Lit(':'))

    term      = Alt(Seq(Lit('('), '_', 'andExpr', Lit(')'), '_', n=2),
                    Node('NODE_TERMS', Lit('"'), Text('quoted'), Lit('"'), '_'),
                    Node('NODE_TERMS',
                         Text(ZeroPlus(CharClass('is_termchar(c)'))), '__'))
    quoted    = ZeroPlus(Alt(CharClass('c != \'"\''), Lit('""')))

    _ = ZeroPlus(CharClass('is_whitespace(c)'))
    __ = Alt(OnePlus(CharClass('is_whitespace(c)')),
             Lookahead(Lit('(')),
             Lookahead(Lit(')')),
             End())

Writer(Grammar, Grammar.root).write_to(sys.stdout)
