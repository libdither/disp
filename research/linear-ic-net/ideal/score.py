"""Score interaction systems by description length, counted in bits, the way Tromp counts a self-interpreter in
its own language: the rule table, plus programs (a reader, a self-interpreter) written as nets of the system.

Systems scored:
  IC        Lafont's interaction combinators: eps (0 aux), gam, del (2 aux), six rules
  IC-de     the same without the del-eps rule (Lafont: still universal)
  old       the reflective nets of ../lean/Tables.lean (13 kinds, 33 rules), read from old_table.jsonl
  tables    the one-agent-closure design of ../nets/tables.py: its rules are pulled out of the reducer by
            firing each pair once, and its per-definition kinds (clo_k, cloL_k) depend on the program loaded

A rule is coded one of three ways, whichever is shortest:
  literal   made agents, then the wiring as one perfect matching of the free ends: log2((n-1)!!) bits
  schema    a generic rule shape that exists for any pair, named in a few bits: ANN (annihilate: aux i of one
            to aux pi(i) of the other), COM (Lafont's commutation: a copy of each agent per auxiliary port of
            the other, wired as a grid; the copies may be other kinds, e.g. an oriented copier), PASS (a one-
            port agent hands the other agent on, unchanged)
usage: python score.py
"""
import sys, os, json, math, itertools
from collections import Counter
HERE = os.path.dirname(os.path.abspath(__file__))
NETS = os.path.join(HERE, '..', 'nets')
sys.path.insert(0, NETS)
sys.setrecursionlimit(1000000)


def lg(x): return math.log2(x) if x > 1 else 0.0
def gamma(n):
    """Elias gamma code length for n >= 1"""
    return 2 * int(math.log2(n)) + 1
def dfact_bits(n):
    """log2 of the number of perfect matchings of n ends"""
    return sum(math.log2(i) for i in range(n - 1, 0, -2))


class System:
    def __init__(self, name, arity, polarized, rules, copiers=(), erasers=(), principal=None):
        self.name, self.arity, self.polarized, self.rules = name, arity, polarized, rules
        self.principal = principal or {}     # kind -> principal polarity, for polarized systems
        self.copiers, self.erasers = set(copiers), set(erasers)

    @property
    def k(self): return len(self.arity)


# ---- classifying a rule ----
def is_ann(S, r):
    a, b, made, wires = r
    if made or S.arity[a] != S.arity[b]: return None
    perm = {}
    for x, y in wires:
        if x[0] == 'R': x, y = y, x
        if x[0] != 'L' or y[0] != 'R': return None
        perm[x[1]] = y[1]
    ident = all(perm[i] == i for i in perm)
    return 1 + (0 if ident else lg(math.factorial(S.arity[a])))


def is_com(S, r):
    """Lafont's commutation, either way round. Returns parameter bits (kind substitutions) or None."""
    a, b, made, wires = r
    W = {}
    for x, y in wires:
        W[tuple(x)] = tuple(y); W[tuple(y)] = tuple(x)
    for (A, B, sa, sb) in ((a, b, 'L', 'R'), (b, a, 'R', 'L')):
        p, q = S.arity[A], S.arity[B]
        if len(made) != p + q: continue
        # copies of B sit on A's aux slots, copies of A on B's aux slots
        cb = [W.get((sa, i)) for i in range(1, p + 1)]
        ca = [W.get((sb, j)) for j in range(1, q + 1)]
        if any(e is None or e[0] != 'N' or e[2] != 0 for e in cb + ca): continue
        cbi, cai = [e[1] for e in cb], [e[1] for e in ca]
        if len(set(cbi + cai)) != p + q: continue
        if any(S.arity[made[j]] != q for j in cbi) or any(S.arity[made[j]] != p for j in cai): continue
        ok = all(W.get(('N', cai[j - 1], i)) == ('N', cbi[i - 1], j) for i in range(1, p + 1) for j in range(1, q + 1))
        if not ok: continue
        subs = sum(1 for j in cbi if made[j] != B) + sum(1 for j in cai if made[j] != A)
        return 1 + subs * lg(S.k)
    return None


def is_pass(S, r):
    a, b, made, wires = r
    for (A, B, sa, sb) in ((a, b, 'L', 'R'), (b, a, 'R', 'L')):
        if S.arity[A] != 1 or len(made) != 1 or S.arity[made[0]] != S.arity[B]: continue
        want = {frozenset([('N', 0, 0), (sa, 1)])} | {frozenset([('N', 0, n), (sb, n)]) for n in range(1, S.arity[B] + 1)}
        if {frozenset([tuple(x), tuple(y)]) for x, y in wires} == want:
            return 1 + (0 if made[0] == B else lg(S.k))
    return None


def literal_bits(S, r):
    a, b, made, wires = r
    ends = S.arity[a] + S.arity[b] + sum(S.arity[m] + 1 for m in made)
    # a polarized wiring is a bijection of out ends to in ends: log2((n/2)!) bits, fewer than a free matching
    wb = lg(math.factorial(ends // 2)) if S.polarized else dfact_bits(ends)
    return gamma(len(made) + 1) + len(made) * lg(S.k) + wb


SCHEMA_TAG = lg(4)   # literal, ANN, COM, PASS

def rule_bits(S, r):
    """(how coded, bits): the cheapest of the codes, after naming the pair"""
    pair = 2 * lg(S.k)
    opts = [('literal', literal_bits(S, r))]
    for name, f in (('ANN', is_ann), ('COM', is_com), ('PASS', is_pass)):
        v = f(S, r)
        if v is not None: opts.append((name, v))
    how, bits = min(opts, key=lambda o: o[1])
    return how, pair + SCHEMA_TAG + bits, pair + literal_bits(S, r)


def is_default(S, r):
    """the rule a global default gives: annihilate (straight) for two agents of one kind, commute otherwise"""
    a, b = r[0], r[1]
    if a == b: return is_ann(S, r) == 1
    return is_com(S, r) == 1


def meeting_pairs(S):
    ks = sorted(S.arity)
    return [(a, b) for i, a in enumerate(ks) for b in ks[i:]
            if not S.polarized or S.principal[a] != S.principal[b]]


def default_bits(S):
    """one bit per pair that can meet (default or not); a pair off the default pays a tag and its code"""
    have = {frozenset([r[0], r[1]]): r for r in S.rules}
    bits = 0.0
    for a, b in meeting_pairs(S):
        bits += 1
        r = have.get(frozenset([a, b]))
        if r is None: bits += lg(5)              # no rule: an exception
        elif not is_default(S, r): bits += rule_bits(S, r)[1] - 2 * lg(S.k) - SCHEMA_TAG + lg(5)
    return bits


def sig_bits(S):
    return gamma(S.k) + 1 + sum(gamma(a + 1) + ((a + 1) if S.polarized else 0) for a in S.arity.values())


def report(S, show_rules=False):
    rows = [(r, *rule_bits(S, r)) for r in S.rules]
    sch = sum(x[2] for x in rows)
    lit = sum(x[3] for x in rows)
    hows = Counter(x[1] for x in rows)
    dishonest = [r for r in S.rules if any(m in S.copiers for m in r[2]) and r[0] not in S.copiers and r[1] not in S.copiers]
    dishonest += [r for r in S.rules if any(m in S.erasers for m in r[2]) and r[0] not in S.erasers and r[1] not in S.erasers]
    maxar = max(S.arity.values())
    maxmade = max(len(r[2]) for r in S.rules)
    out = dict(name=S.name, kinds=S.k, rules=len(S.rules), sig=sig_bits(S), lit=lit, sch=sch, dflt=default_bits(S),
               pairs=len(meeting_pairs(S)),
               hows=dict(hows), dishonest=len(dishonest), maxar=maxar, maxmade=maxmade,
               lit_by_how={h: round(sum(x[3] for x in rows if x[1] == h)) for h in hows},
               dishonest_rules=sorted({f'{r[0]}x{r[1]}' for r in dishonest}))
    if show_rules:
        for r, how, b, l in rows: print(f'    {r[0]}x{r[1]}: {how} {b:.1f} (literal {l:.1f})')
    return out


# ---- the systems ----
def ic(drop_de=False):
    ar = {'eps': 0, 'gam': 2, 'del': 2}
    ann = lambda a: (a, a, [], [[['L', i], ['R', i]] for i in range(1, ar[a] + 1)])
    def com(a, b):
        p, q = ar[a], ar[b]
        made = [a] * q + [b] * p       # copies of a (one per aux of b), then copies of b
        w = [[['R', j + 1], ['N', j, 0]] for j in range(q)] + [[['L', i + 1], ['N', q + i, 0]] for i in range(p)]
        w += [[['N', j, i + 1], ['N', q + i, j + 1]] for i in range(p) for j in range(q)]
        return (a, b, made, w)
    rules = [ann('eps'), ann('gam'), ann('del'), com('eps', 'gam'), com('gam', 'del')]
    if not drop_de: rules.append(com('del', 'eps'))
    return System('IC without del-eps' if drop_de else 'interaction combinators', ar, False, rules,
                  copiers={'del'}, erasers={'eps'})


def old():
    ar, rules, pr = {}, [], {}
    for line in open(os.path.join(HERE, 'old_table.jsonl')):
        d = json.loads(line)
        if 'kind' in d: ar[d['kind']] = len(d['ports']) - 1; pr[d['kind']] = d['ports'][0]
        elif 'a' in d: rules.append((d['a'], d['b'], d['made'], d['wires']))
    return System('reflective nets (old)', ar, True, rules, copiers={'sig', 'dupI', 'dupO'}, erasers={'eraI', 'eraO'},
                  principal=pr)


def native_case_bits(S, arms=5, parts=(1, 1, 2, 1, 0)):
    """Inference aid: literal bits of five reflection rules done by a case agent C(p; k1..k5, r) that hands
    the chosen arm the other arms, then the parts, through a chain of applications (O agents): resource-honest
    like the view, but no λ-term is built"""
    tot = 0.0
    for p in parts:
        m = (arms - 1) + p
        ends = (arms + 1) + p + 3 * m
        tot += 2 * lg(S.k) + SCHEMA_TAG + gamma(m + 1) + m * lg(S.k) + lg(math.factorial(ends // 2))
    return tot


def extract(mod, n, a, b):
    """Fire the pair (a, b) once in reducer n (module mod); return the rule as (made, wires) in End form.
    a, b: (kind, tag or None)."""
    def mk(kind, tag):
        return n.new(kind, tag) if tag is not None else n.new(kind)
    x, y = mk(*a), mk(*b)
    stubs = {}
    for ag, side in ((x, 'L'), (y, 'R')):
        for i in range(1, n.ar[ag] + 1):
            s = n.new('root')
            stubs[s] = (side, i)
            n.link((ag, i), (s, 0))
    n.link((x, 0), (y, 0))
    before = max(n.kind)
    n.queue.clear()
    n.fire(x, y)
    new = sorted(g for g in n.kind if g > before and n.kind[g] not in ('root', 'ph'))
    idx = {g: j for j, g in enumerate(new)}
    def end(p):
        g, i = p
        if g in stubs: return list(stubs[g])
        return ['N', idx[g], i]
    wires, seen = [], set()
    for g in list(new) + list(stubs):
        for i in range(n.ar[g] + 1):
            p = (g, i)
            if g in stubs and i != 0: continue
            q = n.mate.get(p)
            if q is None or frozenset([p, q]) in seen: continue
            seen.add(frozenset([p, q]))
            wires.append([end(p), end(q)])
    made = [n.kind[g] + (f'_{n.tag[g]}' if g in n.tag else '') for g in new]
    for g in new + list(stubs): n.remove(g)
    return made, wires


def tables_system(t):
    """The table design's system for program t: fixed rules plus two kinds per definition of t."""
    import tables as N
    n = N.Net()
    n.load(t)
    D = len(n.defs)
    ar = {'app': 2, 'var': 1, 'rho': 0, 'tab': N.NSLOT, 'O': 2, 'T': 1, 'Ctab': N.NSLOT + 1, 'sig': 2, 'dup': 2, 'era': 0}
    clos = []
    for k in range(D):
        for c in ('clo', 'cloL'):
            ar[f'{c}_{k}'] = len(n.defs[k][3]); clos.append((c, k))
    code = [('app', None), ('var', None), ('rho', None), ('tab', None)] + clos
    name = lambda kt: kt[0] + (f'_{kt[1]}' if kt[1] is not None else '')
    pairs = [(('O', None), c) for c in clos] + [(('O', None), ('tab', None))]
    pairs += [(('Ctab', None), c) for c in code]
    pairs += [(('T', None), c) for c in code]
    pairs += [((cp, None), c) for cp in ('sig', 'dup', 'era') for c in code]
    rules = []
    m = N.Net(); m.defs = n.defs
    for a, b in pairs:
        made, wires = extract(N, m, a, b)
        rules.append((name(a), name(b), made, wires))
    return System(f'tables (D={D})', ar, False, rules, copiers={'sig', 'dup'}, erasers={'era'}), n, D


# ---- programs as nets ----
def net_bits(n, kinds):
    ags = [g for g in n.kind if n.kind[g] not in ('root', 'ph')]
    ports = sum(n.ar[g] + 1 if hasattr(n, 'ar') and isinstance(n.ar, dict) else 0 for g in ags)
    return len(ags), ports, len(ags) * lg(kinds) + dfact_bits(ports + (ports % 2))


def old_program(src):
    import net, check, ref
    t = ref.parse(check.PROGRAMS + '\n' + src)
    n = net.Net()
    n.load(t)
    n.ar = {g: net.ARITY[k] for g, k in n.kind.items()}
    return n


def run_both(label, src_old, src_new, limit=60000000):
    import check, tables_check
    import net
    st, res, stats = check.run_net(check.parse(check.PROGRAMS + '\n' + src_old), None, limit)
    st2, res2, stats2, _ = tables_check.run_net(tables_check.program(src_new), None, limit)
    reads_old = sum(v for k, v in stats.items() if k.startswith('R⋈'))
    reads_new = sum(v for k, v in stats2.items() if k.startswith('Ctab⋈'))
    er_old = sum(v for k, v in stats.items() if k.startswith('erase'))
    er_new = sum(v for k, v in stats2.items() if k.startswith('erase'))
    return dict(label=label, old=(st, stats['interactions'], reads_old, er_old), new=(st2, stats2['interactions'], reads_new, er_new))


def main():
    print('== Rule tables (bits) ==')
    print(f'{"system":<28} {"kinds":>5} {"rules":>5} {"pairs":>5} {"sig":>5} {"literal":>8} {"schema":>8} {"default":>8}  coded as / literal bits by code  | non-copy rules making copiers or erasers')
    systems = [ic(), ic(True), old()]
    for S in systems:
        r = report(S)
        print(f'{r["name"]:<28} {r["kinds"]:>5} {r["rules"]:>5} {r["pairs"]:>5} {r["sig"]:>5.0f} {r["lit"]+r["sig"]:>8.0f} {r["sch"]+r["sig"]:>8.0f} {r["dflt"]+r["sig"]:>8.0f}  {r["hows"]} {r["lit_by_how"]} | {r["dishonest"]} {r["dishonest_rules"]}  max arity {r["maxar"]}, max made {r["maxmade"]}')
    O = old()
    views = sum(rule_bits(O, r)[1] for r in O.rules if r[0] == 'R')
    print(f'old design: the five view rules take {views:.0f} bits; a native case agent doing the same reads, resource-honest, would take about {native_case_bits(O):.0f}')
    if '-v' in sys.argv:
        for S in systems: print(S.name); report(S, True)

    import tables_check, tables as N
    progs = ['size', 'interp', 'size suc', 'interp ' + tables_check.quote('add (suc zero) (suc zero)')]
    print('\n== The table design: its system depends on the program loaded ==')
    for p in progs:
        S, n, D = tables_system(tables_check.program(p))
        r = report(S)
        lay = [x for x in S.rules if x[0] == 'O' and x[1].startswith('clo')]
        laybits = sum(rule_bits(S, x)[1] for x in lay)
        print(f'program {p[:40]!r}: definitions D={D}, kinds {r["kinds"]}, rules {r["rules"]}, table {r["sch"]+r["sig"]:.0f} bits '
              f'(layout rules {laybits:.0f}), max arity {r["maxar"]}, max made {r["maxmade"]}, codes {r["hows"]}, '
              f'non-copy rules making copiers: {r["dishonest"]} {r["dishonest_rules"][:3]}')
    print('\n== Programs as nets (bits: log2 kinds per agent + log2 of the wiring) ==')
    for p in ('size', 'interp'):
        no = old_program(p)
        a, ports, b = net_bits(no, 13)
        S, nn, D = tables_system(tables_check.program(p))
        a2, ports2, b2 = net_bits(nn, S.k)
        rs = report(S); base = tables_system(tables_check.program('ρ'))[0]
        rb = report(base)
        extra = (rs['sch'] + rs['sig']) - (rb['sch'] + rb['sig'])
        print(f'{p}: old {a} agents {b:.0f} bits | tables {a2} agents {b2:.0f} bits as a net, + {extra:.0f} bits of rules its definitions add to the table (fixed table alone: {rb["sch"]+rb["sig"]:.0f})')
    print('\n== Cost on the reflective benchmark (rewrites; reads; erase rewrites) ==')
    q = tables_check.quote
    import check
    for label, so, sn in [('size suc', 'size suc', 'size suc'),
                          ('size size', 'size size', 'size size'),
                          ('interp: 1+1', 'interp ' + check.quote('add (suc zero) (suc zero)'), 'interp ' + q('add (suc zero) (suc zero)'))]:
        r = run_both(label, so, sn)
        (s1, w1, rd1, e1), (s2, w2, rd2, e2) = r['old'], r['new']
        print(f'{label:<14} old: {s1} {w1:>8} rewrites, {rd1:>3} reads, {e1:>7} erases | tables: {s2} {w2:>7} rewrites, {rd2:>3} reads, {e2:>6} erases | log2 ratio {math.log2(w1 / w2):.1f} bits')


if __name__ == '__main__':
    main()
