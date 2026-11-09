"""Unary multiply (a, b, p) -> (a, b, p + a*b), numbers as stem chains △^n △.
Reversible: nested loops, no K, program handed back. Inner loop moves b onto bacc while adding a stem to p
(unary, so the reversed order does not matter); outer loop walks a, rebuilding it, and runs the inner loop once
per stem. Inner frame FI = △ (△ IEND ISTEP) ISPARE, outer frame FO = △ (△ OEND OSTEP) OSPARE; the step arms'
spares sit in the frames' unused fork slots; the end arms' spares ride along (IES, OES).
Ordinary: mul a b p = a == 0 ? p : mul (a-1) b (add b p), K allowed, self-application."""
import sys
from common import *
from addx4 import best_found, subst_K
def frame(c, d, b): return A(L, A(L, c, d), b)

def build(optimized=True):
    # inner step: ISTEP IEND ISPARE brest bacc p [FO arest aacc IES OES]
    if optimized and best_found('FRK') is not None:
        push2 = best_found('PUSH2') or comp(lam('fr', 'brest', 'bacc', 'p', A('fr', 'brest', A(L, 'bacc'), A(L, 'p'))))
        ISTEP = subst_K(best_found('FRK'), push2)
    else:
        ISTEP = comp(lam('iend', 'isp', 'brest', 'bacc', 'p',
                         A(frame('iend', 'isp', 'isp'), 'brest', A(L, 'bacc'), A(L, 'p'))))
    # inner end: IEND ISTEP ISPARE bacc p FO arest aacc IES OES
    IEND = comp(lam('ist', 'isp', 'bacc', 'p', 'FO', 'arest', 'aacc', 'IES', 'OES',
                    A('FO', 'arest', 'bacc', 'p', 'aacc', frame('IES', 'ist', 'isp'), 'IES', 'OES')))
    # outer step: OSTEP OEND OSPARE arest b p aacc FI IES OES
    OSTEP = comp(lam('oend', 'osp', 'arest', 'b', 'p', 'aacc', 'FI', 'IES', 'OES',
                     A('FI', 'b', L, 'p', frame('oend', 'osp', 'osp'), 'arest', A(L, 'aacc'), 'IES', 'OES')))
    # outer end: OEND OSTEP OSPARE b p aacc FI IES OES
    OEND = comp(lam('ost', 'osp', 'b', 'p', 'aacc', 'FI', 'IES', 'OES',
                    A(L, A(L, A(L, frame('OES', 'ost', 'osp'), 'FI'), A(L, 'IES', 'OES')),
                      A(L, 'aacc', A(L, 'b', 'p')))))
    FI = frame(IEND, ISTEP, ISTEP); FO = frame(OEND, OSTEP, OSTEP)
    prog = A(L, A(L, FO, FI), A(L, IEND, OEND))
    start = lambda a, b, p: A(FO, nat(a), nat(b), nat(p), L, FI, IEND, OEND)
    expect = lambda a, b, p: A(L, prog, A(L, nat(a), A(L, nat(b), nat(p + a * b))))
    return start, expect, {'ISTEP': size(ISTEP), 'IEND': size(IEND), 'OSTEP': size(OSTEP), 'OEND': size(OEND)}

def ordinary(w8):
    S = 'self'
    add = lam(S, 'b', 'p', A(tri(lam('p', 'r', 'p'), lam('u', 'p', 'r', A(L, A('r', 'u', 'p'))),
                                 lam('u', 'v', 'p', 'r', 'p'), 'b'), 'p', w8(S, S)))
    mul = lam(S, 'ad', 'a', 'b', 'p',
              A(tri(lam('ad', 'b', 'p', 'r', 'p'),
                    lam('u', 'ad', 'b', 'p', 'r', A('r', 'ad', 'u', 'b', A('ad', 'b', 'p'))),
                    lam('u', 'v', 'ad', 'b', 'p', 'r', 'p'), 'a'), 'ad', 'b', 'p', w8(S, S)))
    return add, mul

def main():
    print(HDR)
    for opt in (False, True):
        start, expect, sz = build(opt)
        for a, b in ((0, 0), (1, 5), (5, 1), (3, 4), (10, 10), (20, 20)):
            v, st = run(start(a, b, 0)); assert v == V(expect(a, b, 0)) and st.K == 0, (a, b)
            if (a, b) in ((10, 10), (20, 20)):
                print(row(f'rev unary mul {a}x{b} {"superopt inner arm" if opt else "bracket arms"}', st,
                          f'{st.steps / (a * b):.1f} steps per unit; sizes {sz}'))
    for calc, w8, conv in (('v5', W8_LTC, ord_ltc), ('tc', W8_TC, ord_tc)):
        add, mul = ordinary(w8)
        ADD = conv(add); MUL = conv(mul)
        for a, b in ((3, 4), (10, 10), (20, 20)):
            v, st = run(A(MUL, MUL, A(ADD, ADD), nat(a), nat(b), nat(0)), calc)
            assert v == V(nat(a * b)), (calc, a, b)
            if (a, b) != (3, 4): print(row(f'ordinary unary mul {a}x{b} {calc}', st, f'{st.steps / (a * b):.1f} steps per unit'))
big_stack(main)
