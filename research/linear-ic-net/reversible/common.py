"""Shared helpers: compilers (V5 LTC via family.py, tree calculus via compile.py), encodings, runs with checks."""
import sys, random, threading
import ltcfast as F
from rules import L, A, size, show
from family import DESIGNS, make_compiler, wait_of
from family_test import lower_k
from compile import compile_, br_tc, lam, tri, WAIT
NAME = 'V5 I K C | W B B2'
comp = make_compiler(DESIGNS[NAME])
Kq = lambda q: A(L, L, A(L, q))
W8_LTC = wait_of(NAME)          # wait f x: a value that runs f x v only when v arrives
W8_TC = WAIT['tc']

def ord_ltc(t):
    """ordinary λ-program (tree-calculus triage) -> LTC, arms wrapped to drop what they beat"""
    return comp(lower_k(t, Kq))
def ord_tc(t):
    return compile_(t, br_tc)

# encodings
B0, B1 = L, A(L, L)                                  # bit 0 = △, bit 1 = △△
def bit(b): return B1 if b else B0
def lst(xs):
    t = L
    for x in reversed(xs): t = A(L, x, t)
    return t
def word(n, w):
    """LSB-first list of bits"""
    return lst([bit((n >> i) & 1) for i in range(w)])
def nat(n):
    t = L
    for _ in range(n): t = A(L, t)
    return t

def run(t, calc='v5', limit=None):
    """evaluate, return (value id, Stats); checks the exact law on every V5 run"""
    v, st = F.run(t, calc, limit)
    if calc == 'v5':
        lhs = st.copied - st.dropped
        rhs = 3 * st.steps + F.SIZE[v] - size_of(t)
        assert lhs == rhs, ('law', lhs, rhs)
    return v, st

def size_of(t):
    return F.size_term(t)

def V(t):
    """prototype value -> id"""
    return F.val(t)

def big_stack(main):
    threading.stack_size(512 * 1024 * 1024)
    th = threading.Thread(target=main); th.start(); th.join()

def row(name, st, extra=''):
    return (f"{name:42} {st.steps:>8} {st.K:>6} {st.W:>6} {st.copied:>9} {st.dropped:>9} {extra}")
HDR = f"{'program':42} {'steps':>8} {'K':>6} {'W':>6} {'copied':>9} {'dropped':>9}"
