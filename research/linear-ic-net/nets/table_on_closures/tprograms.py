"""The reading programs written against the kept table on closures. Every arm takes the table first (t), then
the parts: fun F (the function opened), opened L (closed again), app g b, var w, prim q (ρ), table a1..a6.
../check.py's PROGRAMS come first, so `size` and `interp` there are the λ-view ones: the fixed code read in the
measurements, and the source of the answers the existing net gives."""
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))
from check import PROGRAMS, BODY, OPENED, quote as oquote

A6 = 'a1 a2 a3 a4 a5 a6'
DEFS = r'''
tkindOf := \c. keep[\t F. true | \t L. true | \t g b. false | \t w. false | \t q. false | \t a1 a2 a3 a4 a5 a6. false] c
tclose := \F. keep[no | \t L. L | no | no | no | no] F
topen := \L. keep[\t F. F | no | no | no | no | no] L
-- naive: the table is copied in the app and table arms, dropped in the leaf arms
tsize := keep[\t F. suc (t (F ρ)) | \t L. t L | \t g b. add (t g) (t b) | \t w. zero | \t q. zero
   | \t a1 a2 a3 a4 a5 a6. add (t a1) (add (t a2) (add (t a3) (add (t a4) (add (t a5) (t a6)))))]
-- threaded: the table goes through every read and comes back with the count; dropped once at the end
tsizeK := \c. keep[\t F k. t (F ρ) (\t n. k t (suc n)) | \t L k. t L k
   | \t g b k. t g (\t n. t b (\t m. k t (add n m))) | \t w k. k t zero | \t q k. k t zero
   | \t a1 a2 a3 a4 a5 a6 k. t a1 (\t n1. t a2 (\t n2. t a3 (\t n3. t a4 (\t n4. t a5 (\t n5. t a6 (\t n6.
        k t (add n1 (add n2 (add n3 (add n4 (add n5 n6)))))))))))] c (\t n. n)
-- an interpreter: a table in code is interpreted slot by slot and rebuilt; applying a table value reads natively
tinterp := keep[\t F. tclose F | \t L. topen L
   | \t g b. (\x y. keep[\u F t y. t (F y) | \u L t y. topen L y | \u g b t y. err | \u w t y. err | \u q t y. q y
                    | \u a1 a2 a3 a4 a5 a6 t y. keep[a1 | a2 | a3 | a4 | a5 | a6] y] x t y) (t g) (t b)
   | \t w. w | \t q. q | \t a1 a2 a3 a4 a5 a6. keep[t a1 | t a2 | t a3 | t a4 | t a5 | t a6]]
'''

def source(body):
    return PROGRAMS + DEFS + '\n' + body

def quote(src):
    return f'((^d. {src}) ρ)'

def B(x): return f'(keep[\\t F. F ρ | no | no | no | no | no] {x})'
def Op(x): return f'(keep[\\t F. F | no | no | no | no | no] {x})'

# (name, check.py's original test (the λ-view, run on the existing net), the same read written with a table)
TESTS = [
    ('kind of a function', 'kindOf (\\x. x)', 'tkindOf (\\x. x)'),
    ('kind of ρ', 'kindOf ρ', 'tkindOf ρ'),
    ('binders in λx. x', 'size (\\x. x)', 'tsize (\\x. x)'),
    ('binders in λx y. y x', 'size (\\x y. y x)', 'tsize (\\x y. y x)'),
    ('binders in a closure', 'size ((\\x y. x) (\\a b c. c))', 'tsize ((\\x y. x) (\\a b c. c))'),
    ('binders in suc', 'size suc', 'tsize suc'),
    ('binders in add 2 2', 'size (add (suc (suc zero)) (suc (suc zero)))', 'tsize (add (suc (suc zero)) (suc (suc zero)))'),
    ('binders in the λ-view size', 'size size', 'tsize size'),
    ('binders in the λ-view size, threaded', 'size size', 'tsizeK size'),
    ('binders in suc, threaded', 'size suc', 'tsizeK suc'),
    ('open then read the body', f'ρ (\\x. x x) {BODY}', B('(\\x. x x)')),
    ('read an application part', f'ρ ((\\x. \\y. x y) zero) {BODY}', B('((\\x. \\y. x y) zero)')),
    ('share a piece of code', f'(\\c k. k c c) (ρ (\\x. x x) {BODY})', f'(\\c k. k c c) {B("(\\x. x x)")}'),
    ('share a spliced value', f'(\\c k. k c c) (ρ (\\x. x) {BODY})', f'(\\c k. k c c) {B("(\\x. x)")}'),
    ('share an opened binder', f'(\\g k. k g g) (ρ (\\x y. y x) {OPENED})', f'(\\g k. k g g) {Op("(\\x y. y x)")}'),
    ('reflect an opened binder', f'kindOf (ρ (\\x. x) {OPENED})', f'tkindOf {Op("(\\x. x)")}'),
    ('open a shared copy twice', f'(\\g. g ρ (g zero)) (ρ (\\x y. x) {OPENED})', f'(\\g. g ρ (g zero)) {Op("(\\x y. x)")}'),
    ('read, close, then run', 'close (open suc) zero', 'tclose (topen suc) zero'),
    ('binders in an opened function', f'size (ρ (\\x y. y x) {OPENED})', f'tsize {Op("(\\x y. y x)")}'),
    ('interpret a quote', 'interp ((^d. add (suc zero) (suc zero)) ρ)', 'tinterp ((^d. add (suc zero) (suc zero)) ρ)'),
    ('interpret: identity applied', 'interp ' + oquote('(\\x. x) (\\y. y)'), 'tinterp ' + quote('(\\x. x) (\\y. y)')),
    ('interpret: two plus two', 'interp ' + oquote('add (suc (suc zero)) (suc (suc zero))'), 'tinterp ' + quote('add (suc (suc zero)) (suc (suc zero))')),
    ('interpret: binders in suc (λ-view size)', 'interp ' + oquote('size suc'), 'tinterp ' + quote('size suc')),
    ('interpret: open then read the body', 'interp ' + oquote(f'ρ (\\x. x x) {BODY}'), 'tinterp ' + quote(B('(\\x. x x)'))),
    ('interpret: open a shared copy twice', 'interp ' + oquote(f'(\\g. g ρ (g zero)) (ρ (\\x y. x) {OPENED})'), 'tinterp ' + quote(f'(\\g. g ρ (g zero)) {Op("(\\x y. x)")}')),
    ('interpret the interpreter', 'interp ' + oquote('interp ' + oquote('(\\x. x) (\\y. y)')), 'tinterp ' + quote('tinterp ' + quote('(\\x. x) (\\y. y)'))),
    ('interpret the interpreter, reading', 'interp ' + oquote('interp ' + oquote(f'ρ (\\x. x x) {BODY}')), 'tinterp ' + quote('tinterp ' + quote(B('(\\x. x x)')))),
]

# reads that only a table design can do: checked against the reference (and the plain run, for interp)
TABLE_TESTS = [
    ('binders in tsize (a table value)', 'tsize tsize'),
    ('binders in tsize, threaded', 'tsizeK tsize'),
    ('binders in tsize\'s code', 'tsize ' + quote('tsize')),
    ('binders in tsizeK\'s code, threaded', 'tsizeK ' + quote('tsizeK')),
    ('kind of a table', 'tkindOf tsize'),
    ('interpret: binders in suc (table size)', 'tinterp ' + quote('tsize suc')),
    ('run: binders in suc (table size)', 'tsize suc'),
    ('interpret: kind of a function (table)', 'tinterp ' + quote('tkindOf (\\x. x)')),
    ('interpret: a table built from interpreted slots', 'tinterp ' + quote('keep[\\t F. F | no | no | no | no | no]')),
    ('interpret: open then read the body (table)', 'tinterp ' + quote(B('(\\x. x x)'))),
    ('interpret the interpreter on a table read', 'tinterp ' + quote('tinterp ' + quote('tkindOf ρ'))),
]
