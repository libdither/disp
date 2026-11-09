"""Fast eager (call-by-value) evaluator for LTC V5 and tree calculus, with hash-consed values.
Counts match family_test.eager (V5) and bench.run(.., 'tc') exactly; values are ints (0 = leaf).
Symbolic atoms (opaque values) are supported: a rule that must inspect an atom raises Stuck."""
import os, sys
PROTO = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'tree-calculus')
if PROTO not in sys.path: sys.path.insert(0, PROTO)
sys.setrecursionlimit(1000000)
from collections import Counter

# node tables: kind 0 leaf, 1 stem, 2 fork, 3 atom, 4 neutral application (atom-headed)
KIND = [0]; LEFT = [0]; RIGHT = [0]; SIZE = [1]; NAME = [None]
TAB = {}
def reset():
    global KIND, LEFT, RIGHT, SIZE, NAME, TAB
    del KIND[1:]; del LEFT[1:]; del RIGHT[1:]; del SIZE[1:]; del NAME[1:]; TAB.clear()

def _new(k, a, b, s, name=None):
    KIND.append(k); LEFT.append(a); RIGHT.append(b); SIZE.append(s); NAME.append(name)
    return len(KIND) - 1
def stem(u):
    key = (u,)
    i = TAB.get(key)
    if i is None: i = TAB[key] = _new(1, u, 0, 1 + SIZE[u])
    return i
def fork(u, v):
    key = (u, v)
    i = TAB.get(key)
    if i is None: i = TAB[key] = _new(2, u, v, 1 + SIZE[u] + SIZE[v])
    return i
def atom(name):
    key = ('atom', name)
    i = TAB.get(key)
    if i is None: i = TAB[key] = _new(3, 0, 0, 0, name)
    return i
def napp(f, x):
    key = ('app', f, x)
    i = TAB.get(key)
    if i is None: i = TAB[key] = _new(4, f, x, SIZE[f] + SIZE[x])
    return i
LEAF = 0

class Stuck(Exception): pass
class Limit(Exception): pass

class Stats:
    __slots__ = ('steps', 'I', 'K', 'C', 'W', 'B', 'B2', 'T0', 'T1', 'T2', 'S', 'copied', 'dropped', 'copies')
    def __init__(self):
        for k in self.__slots__: setattr(self, k, 0)
    def counter(self):
        c = Counter({k: getattr(self, k) for k in self.__slots__})
        return +c
    def __add__(self, o):
        r = Stats()
        for k in self.__slots__: setattr(r, k, getattr(self, k) + getattr(o, k))
        return r
    def __repr__(self):
        return ' '.join(f'{k}={getattr(self, k)}' for k in self.__slots__ if getattr(self, k))

def apply_v5(f, x, st, limit=None):
    """eager f x for V5 (I K C | W B B2 | triage handing back the beaten arms)"""
    K_, L_, R_, S_ = KIND, LEFT, RIGHT, SIZE
    stack = []; push = stack.append; pop = stack.pop
    steps = 0
    lim = limit if limit is not None else 1 << 62
    while True:
        kf = K_[f]
        if kf == 2:
            a = L_[f]; b = R_[f]; ka = K_[a]
            steps += 1
            if steps > lim: st.steps += steps; raise Limit()
            if ka == 0:
                kb = K_[b]
                if kb == 0: st.I += 1; v = x
                elif kb == 1: st.K += 1; st.dropped += S_[x]; v = L_[b]
                elif kb == 2: st.C += 1; push(R_[b]); f = L_[b]; continue
                else: st.steps += steps; raise Stuck('C/I/K needs shape of an atom')
            elif ka == 1:
                p = L_[a]; kp = K_[p]
                if kp == 0: st.W += 1; st.copied += S_[x]; st.copies += 1; push(x); f = b; continue
                elif kp == 1: st.B += 1; push(~L_[p]); f = b; continue
                elif kp == 2: st.B2 += 1; push(~L_[p]); push(~R_[p]); f = b; continue
                else: st.steps += steps; raise Stuck('W/B/B2 needs shape of an atom')
            elif ka == 2:
                kx = K_[x]
                if kx == 0: st.T0 += 1; push(b); f = L_[a]; x = R_[a]; continue
                elif kx == 1: st.T1 += 1; push(L_[x]); push(b); f = R_[a]; x = L_[a]; continue
                elif kx == 2: st.T2 += 1; push(R_[x]); push(L_[x]); push(R_[a]); f = b; x = L_[a]; continue
                else: st.steps += steps; raise Stuck('triage on an atom')
            else: st.steps += steps; raise Stuck('rule picked by an atom')
        elif kf == 0: v = stem(x)
        elif kf == 1: v = fork(L_[f], x)
        else: v = napp(f, x)
        # return v to the continuation
        if not stack:
            st.steps += steps; return v
        y = pop()
        if y >= 0: f = v; x = y
        else: f = ~y; x = v

def apply_tc(f, x, st, limit=None):
    """eager f x for tree calculus (K, S, triage that drops what it does not take)"""
    K_, L_, R_, S_ = KIND, LEFT, RIGHT, SIZE
    stack = []; push = stack.append; pop = stack.pop
    steps = 0
    lim = limit if limit is not None else 1 << 62
    while True:
        kf = K_[f]
        if kf == 2:
            a = L_[f]; b = R_[f]; ka = K_[a]
            steps += 1
            if steps > lim: st.steps += steps; raise Limit()
            if ka == 0: st.K += 1; st.dropped += S_[x]; v = b
            elif ka == 1:
                st.S += 1; st.copied += S_[x]; st.copies += 1
                push(('S', b, x)); f = L_[a]; continue
            elif ka == 2:
                kx = K_[x]; c = L_[a]; d = R_[a]
                if kx == 0: st.T0 += 1; st.dropped += S_[d] + S_[b]; v = c
                elif kx == 1: st.T1 += 1; st.dropped += S_[c] + S_[b]; f = d; x = L_[x]; continue
                elif kx == 2: st.T2 += 1; st.dropped += S_[c] + S_[d]; push(R_[x]); f = b; x = L_[x]; continue
                else: raise Stuck()
            else: raise Stuck()
        elif kf == 0: v = stem(x)
        elif kf == 1: v = fork(L_[f], x)
        else: v = napp(f, x)
        while True:
            if not stack:
                st.steps += steps; return v
            y = pop()
            if type(y) is tuple:      # S pending: v = p x; now compute b x, then apply v to it
                push(~v); f = y[1]; x = y[2]; break
            if y >= 0: f = v; x = y
            else: f = ~y; x = v
            break

def ev(t, apply, st, limit=None):
    """evaluate a prototype term ('L', strings = atoms, pairs = application), iteratively"""
    out = []; work = [(t, False)]
    while work:
        u, done = work.pop()
        if done:
            x = out.pop(); f = out.pop(); out.append(apply(f, x, st, limit)); continue
        if u == 'L': out.append(LEAF)
        elif isinstance(u, str): out.append(atom(u))
        elif isinstance(u, int): out.append(u)       # already a value id
        else:
            work.append((u, True)); work.append((u[1], False)); work.append((u[0], False))
    return out[0]

def run(t, calc='v5', limit=None):
    st = Stats()
    v = ev(t, apply_v5 if calc == 'v5' else apply_tc, st, limit)
    return v, st

def val(t):
    """prototype value tree -> id (no rule may fire)"""
    st = Stats(); v = ev(t, apply_v5, st)
    assert st.steps == 0
    return v

def term(i):
    """id -> prototype term"""
    memo = {}
    order = []; work = [i]
    while work:
        j = work.pop()
        if j in memo: continue
        k = KIND[j]
        if k == 0: memo[j] = 'L'
        elif k == 3: memo[j] = NAME[j]
        else:
            kids = [LEFT[j]] if k == 1 else [LEFT[j], RIGHT[j]]
            if all(c in memo for c in kids):
                if k == 1: memo[j] = ('L', memo[LEFT[j]])
                elif k == 2: memo[j] = (('L', memo[LEFT[j]]), memo[RIGHT[j]])
                else: memo[j] = (memo[LEFT[j]], memo[RIGHT[j]])
            else:
                work.append(j); work.extend(c for c in kids if c not in memo)
    return memo[i]

def size_term(t):
    n, stck = 0, [t]
    while stck:
        u = stck.pop()
        if isinstance(u, tuple): stck.append(u[0]); stck.append(u[1])
        elif u == 'L': n += 1
        elif isinstance(u, int): n += SIZE[u]
    return n

def show(i, depth=60):
    k = KIND[i]
    if depth <= 0: return '…'
    if k == 0: return '△'
    if k == 3: return NAME[i]
    if k == 1: return f'(△ {show(LEFT[i], depth-1)})'
    if k == 2: return f'(△ {show(LEFT[i], depth-1)} {show(RIGHT[i], depth-1)})'
    return f'[{show(LEFT[i], depth-1)} {show(RIGHT[i], depth-1)}]'
