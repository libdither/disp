"""The reading programs (kindOf, close, open, size, interp) written against each case mechanism.

Each design defines dkindOf, dclose, dopen, dsize, dinterp, plus a second variant of size and interp:
  view, hand, once:  dsize2 / dinterp2 have closed arms, self handed in after the choice
  keep, chain, ref, build:  dsizeK / dinterpK thread the table through every read (it is never copied or dropped
                            until the end), the arms taking a continuation
The check.py definitions (zero, suc, add, Z, no, err, size, interp, …) come first, so `size` is always the
λ-view size: the fixed piece of code every design's size reads in the measurements.
"""
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))
from check import PROGRAMS

def arms_of(design):
    """the binders an arm for kind i is handed before its parts"""
    if design in ('view', 'hand'): return lambda i, others: others
    if design == 'once': return lambda i, others: ''
    return lambda i, others: 't '

def table(design, arms, scrut=None):
    """the source of a table (applied to scrut, if given)"""
    if design == 'view':
        return f'ρ {scrut} ' + ' '.join(f'({a})' for a in arms) if scrut else None
    if design == 'build':
        t = '(RHO ' + ' '.join(f'({a})' for a in arms) + ')'
    else:
        t = design + '[' + ' | '.join(arms) + ']'
    return f'{t} {scrut}' if scrut else t

OTHERS = ['o a v p', 'f a v p', 'f o v p', 'f o a p', 'f o a v']   # the λ-view's names for the arms handed over

def defs(design):
    pre = arms_of(design)
    A = lambda i, params, body: f'\\{pre(i, OTHERS[i] + " ")}{params}. {body}' if (pre(i, OTHERS[i] + " ") + params).strip() else body
    prim_q = 'q' if design != 'view' else ''          # prim: the value read is handed back (not in the λ-view)
    def T(arms, scrut):
        return table(design, arms, scrut)
    def V(i, params, body):                            # an arm; the prim arm gets its part q
        if i == 4 and prim_q: params = (params + ' ' + prim_q).strip()
        return A(i, params, body)
    lines = []
    lines.append(f"dkindOf := \\c. {T([V(0, 'F', 'true'), V(1, 'L', 'true'), V(2, 'g b', 'false'), V(3, 'w', 'false'), V(4, '', 'false')], 'c')}")
    lines.append(f"dclose := \\F. {T(['no', V(1, 'L', 'L'), 'no', 'no', 'no'], 'F')}")
    lines.append(f"dopen := \\L. {T([V(0, 'F', 'F'), 'no', 'no', 'no', 'no'], 'L')}")
    prim_val = 'q' if prim_q else 'ρ'
    prim_app = 'q y' if prim_q else 'ρ y'
    if design in ('view', 'hand', 'once'):
        lines.append(f"dsize := Z (\\self c. {T([V(0, 'F', 'suc (self (F ρ))'), V(1, 'L', 'self L'), V(2, 'g b', 'add (self g) (self b)'), V(3, 'w', 'zero'), V(4, '', 'zero')], 'c')})")
        lines.append(f"dsize2 := Z (\\self c. {T([V(0, 'F self', 'suc (self (F ρ))'), V(1, 'L self', 'self L'), V(2, 'g b self', 'add (self g) (self b)'), V(3, 'w self', 'zero'), V(4, 'self', 'zero') if not prim_q else A(4, 'q self', 'zero')], 'c')} self)")
        inner = T([V(0, 'F', 'self (F y)'), V(1, 'L', 'dopen L y'), V(2, 'g b', 'err'), V(3, 'w', 'err'), V(4, '', prim_app)], 'x')
        lines.append(f"dinterp := Z (\\self c. {T([V(0, 'F', 'dclose F'), V(1, 'L', 'dopen L'), V(2, 'g b', f'(\\x y. {inner}) (self g) (self b)'), V(3, 'w', 'w'), V(4, '', prim_val)], 'c')})")
        inner2 = T([V(0, 'F self y', 'self (F y)'), V(1, 'L self y', 'dopen L y'), V(2, 'g b self y', 'err'), V(3, 'w self y', 'err'),
                    A(4, (prim_q + ' self y').strip(), prim_app)], 'x') + ' self y'
        lines.append(f"dinterp2 := Z (\\self c. {T([V(0, 'F self', 'dclose F'), V(1, 'L self', 'dopen L'), V(2, 'g b self', f'(\\x y self. {inner2}) (self g) (self b) self'), V(3, 'w self', 'w'), A(4, (prim_q + ' self').strip(), prim_val)], 'c')} self)")
    else:
        lines.append(f"dsize := {T(['\\t F. suc (t (F ρ))', '\\t L. t L', '\\t g b. add (t g) (t b)', '\\t w. zero', '\\t q. zero'], None)}")
        lines.append(f"dsizeK := \\c. {T(['\\t F k. t (F ρ) (\\t n. k t (suc n))', '\\t L k. t L k', '\\t g b k. t g (\\t n. t b (\\t m. k t (add n m)))', '\\t w k. k t zero', '\\t q k. k t zero'], 'c')} (\\t n. n)")
        inner = T(['\\u F t y. t (F y)', '\\u L t y. dopen L y', '\\u g b t y. err', '\\u w t y. err', '\\u q t y. q y'], 'x') + ' t y'
        lines.append(f"dinterp := {T(['\\t F. dclose F', '\\t L. dopen L', f'\\t g b. (\\x y. {inner}) (t g) (t b)', '\\t w. w', '\\t q. q'], None)}")
        innerK = T(['\\u F t y k. t (F y) k', '\\u L t y k. k t (dopen L y)', '\\u g b t y k. err', '\\u w t y k. err', '\\u q t y k. k t (q y)'], 'x') + ' t y k'
        lines.append(f"dinterpK := \\c. {T(['\\t F k. k t (dclose F)', '\\t L k. k t (dopen L)', f'\\t g b k. t g (\\t x. t b (\\t y. {innerK}))', '\\t w k. k t w', '\\t q k. k t q'], 'c')} (\\t v. v)")
    return '\n'.join(lines)

DESIGNS = ['view', 'hand', 'once', 'keep', 'build', 'chain', 'ref']

def source(design, body):
    return PROGRAMS + '\n' + defs(design) + '\n' + body

def body_read(design, x):
    """open the function x on ρ and hand back its body as code (check.py's BODY)"""
    return table(design, [arms_of(design)(0, OTHERS[0] + ' ') and f"\\{arms_of(design)(0, OTHERS[0] + ' ')}F. F ρ" or '\\F. F ρ', 'no', 'no', 'no', 'no'], x)

def opened_read(design, x):
    """hand back x opened (check.py's OPENED)"""
    return table(design, [arms_of(design)(0, OTHERS[0] + ' ') and f"\\{arms_of(design)(0, OTHERS[0] + ' ')}F. F" or '\\F. F', 'no', 'no', 'no', 'no'], x)

def quote(src):
    return f'((^d. {src}) ρ)'

def tests(design):
    """check.py's TESTS that read, translated: (name, the original test's source, the translated source)"""
    B = lambda x: f'({body_read(design, x)})'
    Op = lambda x: f'({opened_read(design, x)})'
    from check import BODY, OPENED, quote as oquote
    has2 = design in ('view', 'hand', 'once')
    size2 = 'dsize2' if has2 else 'dsizeK'
    interp2 = 'dinterp2' if has2 else 'dinterpK'
    out = [
        ('kind of a function', 'kindOf (\\x. x)', 'dkindOf (\\x. x)'),
        ('kind of ρ', 'kindOf ρ', 'dkindOf ρ'),
        ('binders in λx. x', 'size (\\x. x)', 'dsize (\\x. x)'),
        ('binders in λx y. y x', 'size (\\x y. y x)', 'dsize (\\x y. y x)'),
        ('binders in a closure', 'size ((\\x y. x) (\\a b c. c))', 'dsize ((\\x y. x) (\\a b c. c))'),
        ('binders in suc', 'size suc', 'dsize suc'),
        ('binders in add 2 2', 'size (add (suc (suc zero)) (suc (suc zero)))', 'dsize (add (suc (suc zero)) (suc (suc zero)))'),
        ('binders in size', 'size size', 'dsize size'),
        ('binders in size, second size', 'size size', f'{size2} size'),
        ('binders in suc, second size', 'size suc', f'{size2} suc'),
        ('open then read the body', f'ρ (\\x. x x) {BODY}', B('(\\x. x x)')),
        ('read an application part', f'ρ ((\\x. \\y. x y) zero) {BODY}', B('((\\x. \\y. x y) zero)')),
        ('share a piece of code', f'(\\c k. k c c) (ρ (\\x. x x) {BODY})', f'(\\c k. k c c) {B("(\\x. x x)")}'),
        ('share a spliced value', f'(\\c k. k c c) (ρ (\\x. x) {BODY})', f'(\\c k. k c c) {B("(\\x. x)")}'),
        ('share an opened binder', f'(\\g k. k g g) (ρ (\\x y. y x) {OPENED})', f'(\\g k. k g g) {Op("(\\x y. y x)")}'),
        ('reflect an opened binder', f'kindOf (ρ (\\x. x) {OPENED})', f'dkindOf {Op("(\\x. x)")}'),
        ('open a shared copy twice', f'(\\g. g ρ (g zero)) (ρ (\\x y. x) {OPENED})', f'(\\g. g ρ (g zero)) {Op("(\\x y. x)")}'),
        ('read, close, then run', 'close (open suc) zero', 'dclose (dopen suc) zero'),
        ('binders in an opened function', f'size (ρ (\\x y. y x) {OPENED})', f'dsize {Op("(\\x y. y x)")}'),
        ('interpret a quote', 'interp ((^d. add (suc zero) (suc zero)) ρ)', 'dinterp ((^d. add (suc zero) (suc zero)) ρ)'),
        ('interpret a quote, second interp', 'interp ((^d. add (suc zero) (suc zero)) ρ)', f'{interp2} ((^d. add (suc zero) (suc zero)) ρ)'),
        ('interpret: identity applied', 'interp ' + oquote('(\\x. x) (\\y. y)'), 'dinterp ' + quote('(\\x. x) (\\y. y)')),
        ('interpret: two plus two', 'interp ' + oquote('add (suc (suc zero)) (suc (suc zero))'), 'dinterp ' + quote('add (suc (suc zero)) (suc (suc zero))')),
        ('interpret: binders in suc (λ-view size)', 'interp ' + oquote('size suc'), 'dinterp ' + quote('size suc')),
        ('interpret: binders in suc (own size)', 'interp ' + oquote('size suc'), 'dinterp ' + quote('dsize suc')),
        ('interpret: open then read the body', 'interp ' + oquote(f'ρ (\\x. x x) {BODY}'), 'dinterp ' + quote(B('(\\x. x x)'))),
        ('interpret: open a shared copy twice', 'interp ' + oquote(f'(\\g. g ρ (g zero)) (ρ (\\x y. x) {OPENED})'), 'dinterp ' + quote(f'(\\g. g ρ (g zero)) {Op("(\\x y. x)")}')),
        ('interpret the interpreter', 'interp ' + oquote('interp ' + oquote('(\\x. x) (\\y. y)')), 'dinterp ' + quote('dinterp ' + quote('(\\x. x) (\\y. y)'))),
        ('interpret the interpreter, reading', 'interp ' + oquote('interp ' + oquote(f'ρ (\\x. x x) {BODY}')), 'dinterp ' + quote('dinterp ' + quote(B('(\\x. x x)')))),
        ('interpret, second interp: two plus two', 'interp ' + oquote('add (suc (suc zero)) (suc (suc zero))'), f'{interp2} ' + quote('add (suc (suc zero)) (suc (suc zero))')),
        ('interpret, second interp: binders in suc', 'interp ' + oquote('size suc'), f'{interp2} ' + quote(f'{size2} suc')),
    ]
    return out
