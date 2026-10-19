# Prototype reader (research/TREE_READBACK.md): a tree back as a lambda term, from a name table, data
# decoders, a compile-time origin table (probe.ts), partial-application patterns derived from the named
# definitions, and bracket abstraction inverted (S, K, I, triage) where the table has nothing; then
# resugared. Run: python reader.py ORIGINS.json TERMS.json (drawer.mjs's dump of a player run).
import json, itertools, re, statistics, sys, collections
sys.setrecursionlimit(100000)
import nbe
from nbe import parse_ternary, ternary, L

ORIG, NAMES = {}, {}
fresh = (f"v{i}" for i in itertools.count())
MIN_NAME = 7          # trees this small (the leaf is also "b0", K is two nodes) are not named
PLUMBING = 64         # a fix definition's body plus this many nodes of unfolding reads as the definition (a heuristic)
ALIASES = {'pair_snd': 'nat_pred', 'b1': 'succ', 'b0': '0'}   # one tree, several names: the ones number code uses

# ---- terms: ('v',x) ('n',name) ('d',literal) ('t',) ('app',f,x) ('lam',[xs],body) ('match',z,w,x,y)
#              ('if',c,a,b) ('delay',x) ('opaque',what)
def fv(t):
    k = t[0]
    if k == 'v': return {t[1]}
    if k == 'lam': return fv(t[2]) - set(t[1])
    if k in ('app', 'match', 'if', 'delay'): return set().union(*(fv(c) for c in t[1:]))
    return set()

def subst(t, x, s):
    k = t[0]
    if k == 'v': return s if t[1] == x else t
    if k in ('app', 'match', 'if', 'delay'): return (k,) + tuple(subst(c, x, s) for c in t[1:])
    if k == 'lam':
        vs, b = list(t[1]), t[2]
        if x in vs: return t
        clash = fv(s)
        for i, v in enumerate(vs):
            if v in clash:
                nv = next(fresh); b = subst(b, v, ('v', nv)); vs[i] = nv
        return ('lam', vs, subst(b, x, s))
    return t

def app(f, x):
    """f x, beta-reduced, and S, K, I by their rules."""
    if f[0] == 'lam':
        b = subst(f[2], f[1][0], x) if f[1][0] != '_' else f[2]
        return b if len(f[1]) == 1 else ('lam', f[1][1:], b)
    if f == ('n', 'I'): return x
    if f[0] == 'app' and f[1] == ('n', 'K'): return f[2]
    if f[0] == 'app' and f[1][0] == 'app' and f[1][1] == ('n', 'S'): return app(app(f[1][2], x), app(f[2], x))
    return ('app', f, x)

def norm(t, depth=0):
    if depth > 60: return t
    k = t[0]
    if k == 'app': return app(norm(t[1], depth + 1), norm(t[2], depth + 1))
    if k == 'lam': return ('lam', t[1], norm(t[2], depth + 1))
    if k in ('match', 'if', 'delay'): return (k,) + tuple(norm(c, depth + 1) for c in t[1:])
    return t

def mk_app(f, args):
    for a in args: f = ('app', f, a)
    return f

def spine(t):
    args = []
    while t[0] == 'app': args.append(t[2]); t = t[1]
    return t, list(reversed(args))

# ---- data
def nat_of(t):
    n = 0
    while t[0] == 'F' and t[1] == L: n += 1; t = t[2]
    return n if t == L else None

def data_of(t):
    n = nat_of(t)
    if n is not None and t != L: return str(n)
    xs = []
    while t[0] == 'F': xs.append(t[1]); t = t[2]
    if t == L and xs:
        ns = [nat_of(x) for x in xs]
        if all(c is not None and 32 <= c < 0x110000 for c in ns): return json.dumps(''.join(map(chr, ns)))
        ds = [data_of(x) for x in xs]
        if all(d is not None for d in ds): return '[' + ', '.join(ds) + ']'
    return None

# ---- origins: probe.ts's structured lowered terms
def from_struct(s):
    k = s[0]
    if k == 'var': return ('v', s[1])
    if k == 'name': return ('n', s[1])
    if k == 'lit': return read(parse_ternary(s[1])) if s[1] != '?' else ('opaque', 'lit')
    if k in ('S', 'K', 'I'): return ('n', k)
    if k == 'app': return ('app', from_struct(s[1]), from_struct(s[2]))
    if k == 'lam': return ('lam', [s[1]], from_struct(s[2]))
    if k == 'orig': return ('lam', list(s[1]), from_struct(s[2]))
    raise ValueError(k)

# ---- partial applications of named definitions: apply each to holes; a tree result is a pattern
def nbe_stuck(t): return t[0] in ('A', 'M') or any(nbe_stuck(c) for c in t[1:] if isinstance(c, tuple))
def nbe_count(t, v): return int(t[1] == v) if t[0] == 'V' else sum(nbe_count(c, v) for c in t[1:] if isinstance(c, tuple))
def nbe_size(t): return 0 if t[0] == 'V' else 1 + sum(nbe_size(c) for c in t[1:] if isinstance(c, tuple))

def derive_patterns(max_args=3, min_size=3):
    pats = []
    for key, name in NAMES.items():
        if len(key) < MIN_NAME: continue
        f = parse_ternary(key)
        for j in range(1, max_args + 1):
            t = f
            for i in range(j): t = nbe.A(t, nbe.V(f"#{i}"))
            try: r = nbe.nf(t, [5000])
            except nbe.Fuel: break
            if r[0] == 'V' or nbe_stuck(r): break      # it inspects an argument, or is just one
            if all(nbe_count(r, f"#{i}") for i in range(j)) and nbe_size(r) >= min_size: pats.append((name, j, r))
    return sorted(pats, key=lambda p: -nbe_size(p[2]))

def match_pat(p, t, env):
    if p[0] == 'V':
        if p[1] in env: return env[p[1]] == t
        env[p[1]] = t; return True
    return p[0] == t[0] and all(match_pat(a, b, env) for a, b in zip(p[1:], t[1:]))

def contains(t, key):
    st = [t]
    while st:
        x = st.pop()
        k = ternary(x)
        if k == key: return True
        if len(k) > len(key): st.extend(c for c in x[1:] if isinstance(c, tuple))
    return False

PATS, FIXBODY = [], {}

# ---- the reader
memo = {}
def read(t, head=False, prefer=None):
    key = ternary(t)
    if (key, head) not in memo: memo[(key, head)] = read1(t, key, head, prefer)
    return memo[(key, head)]

def read1(t, key, head, prefer):
    if key in NAMES and len(key) >= MIN_NAME: return ('n', NAMES[key])
    for bk, d in FIXBODY.items():                                   # recursion's plumbing (heuristic)
        if len(bk) < len(key) <= len(bk) + PLUMBING and contains(t, bk): return ('n', d)
    if t == L: return ('t',)
    if not head and (d := data_of(t)) is not None: return ('d', d)
    if key in ORIG:                                                 # the compiler's origin table
        os = ORIG[key]
        o = next((o for o in os if prefer and o['def'] == prefer), None) or next((o for o in os if 'vars' in o), os[0])
        return norm(('lam', o['vars'], from_struct(o['body'])) if 'vars' in o else from_struct(o['app']))
    for name, j, p in PATS:                                         # a known definition, partly applied
        env = {}
        if match_pat(p, t, env): return mk_app(('n', name), [read(env[f"#{i}"]) for i in range(j)])
    if t[0] == 'S':                                                 # bracket abstraction, inverted
        return ('app', ('n', 'S'), read(t[1][1], True)) if t[1][0] == 'S' else ('app', ('t',), read(t[1]))
    a, b = t[1], t[2]
    if a == L: return ('lam', ['_'], read(b))                       # K b
    if a[0] == 'S':                                                 # S p b
        v = ('v', next(fresh))
        return ('lam', [v[1]], app(app(read(a[1], True), v), app(read(b, True), v)))
    w, x = a[1], a[2]                                               # triage
    if (w, x, b) == (L, L, L): return ('lam', ['z'], ('v', 'z'))   # I
    z = next(fresh)
    return ('lam', [z], ('match', ('v', z), read(w), read(x, True), read(b, True)))

# ---- resugaring: the elaborator's desugarings, undone
def resugar(t):
    k = t[0]
    if k == 'n': return ('n', ALIASES.get(t[1], t[1]))
    if k == 'lam': return ('lam', t[1], resugar(t[2]))
    if k in ('match', 'if', 'delay'): return (k,) + tuple(resugar(c) for c in t[1:])
    if k != 'app': return t
    f, args = spine(t)
    f, args = resugar(f), [resugar(a) for a in args]
    # cond c (λxs. A) (λxs. B) xs → if c then A else B: each branch is closed over its free variables
    if f == ('n', 'cond') and len(args) >= 3 and args[1][0] == 'lam' and args[2][0] == 'lam':
        n = len(args[1][1]); rest = args[3:3 + n]
        if len(rest) == n:
            def inst(lam):
                b, vs = lam[2], list(lam[1])
                while b[0] == 'lam' and len(vs) < n: vs += b[1]; b = b[2]
                for x, r in zip(vs, rest): b = subst(b, x, r)
                return b
            return mk_app(('if', args[0], inst(args[1]), inst(args[2])), args[3 + n:])
    # wait omega x → ⌛x, a delay
    if f == ('n', 'wait') and len(args) >= 2 and args[0] == ('n', 'omega'): return mk_app(('delay', args[1]), args[2:])
    return mk_app(f, args)

# ---- printing (lambda notation)
def show(t):
    k = t[0]
    if k in ('v', 'n', 'd'): return t[1]
    if k == 't': return 't'
    if k == 'opaque': return f'‹{t[1]}›'
    if k == 'if': return f"if {show(t[1])} then {show(t[2])} else {show(t[3])}"
    if k == 'delay': return f"⌛{atom(t[1])}"
    if k == 'lam':
        b, vs = t[2], list(t[1])
        while b[0] == 'lam': vs += b[1]; b = b[2]
        return f"λ{' '.join(vs)}. {show(b)}"
    if k == 'match': return f"match {atom(t[1])} {{ t => {show(t[2])}; t u => {atom(t[3])} u; t u v => {atom(t[4])} u v }}"
    f, args = spine(t)
    return ' '.join([atom(f)] + [atom(a) for a in args])

def atom(t):
    s = show(t)
    return s if t[0] in ('v', 'n', 'd', 't', 'opaque', 'delay') else f"({s})"

def load(path):
    global ORIG, NAMES, PATS, FIXBODY
    D = json.load(open(path))
    ORIG, NAMES = D['origins'], D['names']
    PATS = derive_patterns()
    for key, os in ORIG.items():
        for o in os:
            if 'app' in o and o['app'][0] == 'app' and o['app'][1] == ['name', 'fix']:
                body = o['app'][2]
                for k2, os2 in ORIG.items():
                    if any('vars' in x and ['orig', x['vars'], x['body']] == body for x in os2): FIXBODY[k2] = o['def']; break

def term_of(e, prefer):
    """drawer.mjs's encoding of a term: ["tree", ternary] | ["@", f, x] | [other kind, ...]."""
    if e[0] == 'tree': return read(parse_ternary(e[1]), prefer=prefer)
    if e[0] == '@': return app(term_of(e[1], prefer), term_of(e[2], prefer))
    return ('opaque', e[0])

if __name__ == '__main__':
    load(sys.argv[1])
    agents = json.load(open(sys.argv[2]))
    prefer = sys.argv[3] if len(sys.argv) > 3 else None
    pieces = [p for a in agents for p in a['terns']]
    exact = sum(p in ORIG for p in pieces)
    print(f"origin table: {len(ORIG)} trees; names: {len(NAMES)}; partial-application patterns: {len(PATS)}; fix bodies: {len(FIXBODY)}")
    print(f"unnamed code pieces in the run: {len(pieces)}; exactly in the origin table: {exact} ({exact / max(1, len(pieces)):.0%}), "
          f"distinct {len(set(pieces))} / {len(set(p for p in pieces if p in ORIG))}")
    texts = [show(resugar(norm(term_of(a['term'], prefer)))) for a in agents]
    opaque_now = sum('‹' in a['text'] for a in agents)
    unread = sum('‹' in s for s in texts)
    plumbing = sum(bool(re.search(r"\bS\b|omega|\bt \(|λ_anon", s)) for s in texts)
    print(f"agents {len(agents)}: with a ‹code› part now {opaque_now}; read back with an unread part {unread}, "
          f"with leftover plumbing (S, omega, t …) {plumbing}, clean {len(agents) - plumbing - unread}")
    for a, s in list(zip(agents, texts))[:0]: pass
    print("\nsamples (now → read back):")
    for a, s in [x for x in zip(agents, texts) if x[0]['term'][0] == '@'][:12:2]:
        print(f"  [{a['tag']}] {a['text'][:110]}\n      → {s[:240]}")
