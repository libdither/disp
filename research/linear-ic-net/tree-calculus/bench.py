"""Eager (call-by-value, as disp runs) evaluation of the same recursive λ-programs compiled to TC
and to LTC: same answers, and what each run does (rewrites, nodes copied, nodes dropped)."""
import sys, threading
from compile import *
from test_compile import nf as ref_nf   # λ reference (normal order)
from collections import Counter

def run(t, calc):
    st = Counter()
    def apply(f, x):
        h, args = spine(f)
        if len(args) < 2: return A(f, x)
        a, b = args
        ha, aa = spine(a)
        if a == L:
            st['K'] += 1; st['dropped'] += size(x); return b
        if len(aa) == 1:
            p = aa[0]
            if calc == 'tc':
                st['S'] += 1; st['copied'] += size(x); st['copies'] += 1
                return apply(apply(p, x), apply(b, x))
            hp, pa = spine(p)
            if p == L:
                st['W'] += 1; st['copied'] += size(x); st['copies'] += 1
                return apply(apply(b, x), x)
            if len(pa) == 1:
                st['B'] += 1; return apply(pa[0], apply(b, x))
            st['C'] += 1; return apply(apply(pa[0], apply(pa[1], x)), b)
        c, d = aa
        st['T'] += 1
        hx, xa = spine(x)
        if not xa: st['dropped'] += size(d) + size(b); return c
        if len(xa) == 1: st['dropped'] += size(c) + size(b); return apply(d, xa[0])
        st['dropped'] += size(c) + size(d); return apply(apply(b, xa[0]), xa[1])
    def ev(t):
        if t == L: return L
        return apply(ev(t[0]), ev(t[1]))
    return ev(t), st

def nat(n):
    t = L
    for _ in range(n): t = A(L, t)
    return t
def tree(d):
    return L if d == 0 else A(L, tree(d - 1), A(L, tree(d - 2)) if d > 1 else L)
def lst(n):
    t = L
    for i in range(n): t = A(L, nat(i % 3), t)
    return t

def programs(calc):
    w = WAIT[calc]
    S_ = 'self'
    mirror = lam(S_, 't', tri(lam('r', L), lam('u', 'r', A(L, A('r', 'u'))),
                            lam('u', 'v', 'r', A(L, A('r', 'v'), A('r', 'u'))), 't') )
    mirror = ('λ', S_, ('λ', 't', A(mirror[2][2], w(S_, S_))))
    add = lam(S_, 'm', 'n', A(tri(lam('n', 'r', 'n'), lam('p', 'n', 'r', A(L, A('r', 'p', 'n'))),
                                  lam('p', 'q', 'n', 'r', 'n'), 'm'), 'n', w(S_, S_)))
    leaves = lam(S_, 'a', 't', A(tri(lam('a', 'r', A(L, L)), lam('u', 'a', 'r', A('r', 'a', 'u')),
                                     lam('u', 'v', 'a', 'r', A('a', A('r', 'a', 'u'), A('r', 'a', 'v'))), 't'),
                                 'a', w(S_, S_)))
    rev = lam(S_, 'l', 'acc', A(tri(lam('acc', 'r', 'acc'), lam('u', 'acc', 'r', 'acc'),
                                    lam('x', 'rest', 'acc', 'r', A('r', 'rest', A(L, 'x', 'acc'))), 'l'),
                                'acc', w(S_, S_)))
    inner = tri(lam('p', 'a', 'r', A(L, L)),
                lam('q', 'p', 'a', 'r', A('a', A('r', 'a', 'p'), A('r', 'a', 'q'))),
                lam('x', 'y', 'p', 'a', 'r', L), 'p')
    fib = lam(S_, 'a', 'n', A(tri(lam('a', 'r', L), lam('p', 'a', 'r', A(inner, 'p', 'a', 'r')),
                                  lam('x', 'y', 'a', 'r', L), 'n'), 'a', w(S_, S_)))
    return mirror, add, leaves, rev, fib

def main():
    rows = []
    for name in ('mirror', 'add', 'leaves', 'rev', 'fib'):
        res = {}
        for calc, br in (('tc', br_tc), ('ltc', br_ltc)):
            mirror, add, leaves, rev, fib = [compile_(p, br) for p in programs(calc)]
            addf = A(add, add)
            call = {'mirror': A(mirror, mirror, tree(7)), 'add': A(addf, nat(30), nat(25)),
                    'leaves': A(leaves, leaves, addf, tree(7)), 'rev': A(rev, rev, lst(25), L),
                    'fib': A(fib, fib, addf, nat(11))}[name]
            res[calc] = run(call, calc)
        (vt, st), (vl, sl) = res['tc'], res['ltc']
        assert vt == vl, name
        rows.append((name, size(vt), sum(st[k] for k in 'KST'), sum(sl[k] for k in ('K','W','B','C','T')),
                     st['copies'], sl['copies'], st['copied'], sl['copied'], st['dropped'], sl['dropped'],
                     dict((k, sl[k]) for k in ('B','C','W','K','T'))))
    print(f"{'call':8} {'answer':>6} {'rewrites TC→LTC':>17} {'copies':>13} {'nodes copied':>17} {'nodes dropped':>17}  LTC rule mix")
    for r in rows:
        print(f'{r[0]:8} {r[1]:6} {r[2]:8}→{r[3]:<8} {r[4]:6}→{r[5]:<6} {r[6]:8}→{r[7]:<8} {r[8]:8}→{r[9]:<8}  {r[10]}')
if __name__ == '__main__':
    threading.stack_size(512 * 1024 * 1024)
    th = threading.Thread(target=main); th.start(); th.join()
