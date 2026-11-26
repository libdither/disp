"""The candidate net scored beside the older reflective net, the table design and the interaction combinators.

  1. the rule table in bits, by the codes of score.py (literal, per-rule shape, default plus exceptions) and
     by one more: the default with declared duals (a kind may name a dual; a kind meeting its dual
     annihilates, any other pair commutes). The plain default is the case where every kind is its own dual.
  2. the reflective programs as nets, in agents and bits
  3. J = table + self-interpreter
  4. cost of the same jobs: rewrites, and agents made (a rewrite of the table design can make 38 agents)
  5. what each rule does to the number of agents, and which right sides can be told apart afterwards

python kscore.py
"""
import sys, os, math
from collections import Counter, defaultdict
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', 'nets'))
sys.setrecursionlimit(1000000)
import score as S
from score import System, lg, gamma, report, rule_bits, is_ann, is_com, default_bits, sig_bits, meeting_pairs
import ktable, kcheck, knet, kref
import net as oldnet, check as oldcheck


def candidate():
    ports, rules, stuck = ktable.table()
    ktable.check(ports, rules)
    ar = {k: len(p) - 1 for k, p in ports.items()}
    pr = {k: p[0] for k, p in ports.items()}
    return System('quote-and-table net', ar, True, rules, copiers=set(ktable.COPIERS), erasers=set(ktable.ERASERS), principal=pr)


# ---- the default code with declared duals ----
def duals(Sy):
    """the pairs of kinds whose rule is a straight annihilation: each kind names at most one dual"""
    d = {}
    for r in Sy.rules:
        if is_ann(Sy, r) == 1 and r[0] not in d and r[1] not in d:
            d[r[0]] = r[1]; d[r[1]] = r[0]
    return d

def dual_default_bits(Sy):
    """signature: each kind names its dual or none (log2(k + 1) bits). Then one bit per pair that can meet;
    a pair off the default (duals annihilate, others commute) pays a tag and its code, as in score.py."""
    d = duals(Sy)
    have = {frozenset([r[0], r[1]]): r for r in Sy.rules}
    bits = Sy.k * lg(Sy.k + 1)
    for a, b in meeting_pairs(Sy):
        bits += 1
        r = have.get(frozenset([a, b]))
        if r is None: bits += lg(5)
        elif d.get(a) == b:
            if is_ann(Sy, r) != 1: bits += rule_bits(Sy, r)[1] - 2 * lg(Sy.k) - S.SCHEMA_TAG + lg(5)
        elif is_com(Sy, r) != 1: bits += rule_bits(Sy, r)[1] - 2 * lg(Sy.k) - S.SCHEMA_TAG + lg(5)
    return bits


# ---- programs as nets ----
def knet_of(src, given=None):
    n = knet.KNet()
    n.load(kcheck.program(src, given))
    n.ar = {g: knet.ARITY[k] for g, k in n.kind.items()}
    return n

def onet_of(src):
    return S.old_program(src)


# ---- running, with agents made counted ----
class OldNet(oldnet.Net):
    def fire(self, a, b):
        before = len(self.kind)
        try: super().fire(a, b)
        finally: self.stats['made'] += len(self.kind) - before + 2

def run_old(src, limit=300000000):
    n = OldNet()
    root = n.load(oldcheck.program(src))
    st = n.run(limit)
    reads = sum(v for k, v in n.stats.items() if k.startswith('R⋈'))
    erases = sum(v for k, v in n.stats.items() if k.startswith('erase'))
    return st, n.stats['interactions'], n.stats['made'], reads, erases, n

def run_new(src, given=None, limit=300000000):
    n = knet.KNet()
    root = n.load(kcheck.program(src, given))
    st = n.run(limit)
    reads = sum(v for k, v in n.stats.items() if k.startswith('read '))
    erases = sum(v for k, v in n.stats.items() if k.startswith('erase'))
    return st, n.stats['interactions'], n.stats['made'], reads, erases, n

def run_tables(src, limit=300000000):
    import tables_check
    st, res, stats, n = tables_check.run_net(tables_check.program(src), None, limit)
    reads = sum(v for k, v in stats.items() if k.startswith('Ctab⋈'))
    erases = sum(v for k, v in stats.items() if k.startswith('erase'))
    return st, stats['interactions'], n.made, reads, erases, n


# ---- what a rule does to the agents ----
def right_side_shape(Sy, r):
    """the right side as an open net, forgetting which free end came from which agent of the pair: the made
    kinds, the wires among them, and how many free ends each port has. Two rules with one shape leave the
    same thing behind."""
    a, b, made, wires = r
    internal, free, bare = [], [], 0
    for x, y in wires:
        nx, ny = x[0] == 'N', y[0] == 'N'
        if nx and ny: internal.append(tuple(sorted([(x[1], x[2]), (y[1], y[2])])))
        elif nx or ny:
            e = x if nx else y
            free.append((e[1], e[2]))
        else: bare += 1
    # canonical up to renaming the made agents: try every ordering of agents of one kind (few agents)
    import itertools
    idx = list(range(len(made)))
    best = None
    groups = defaultdict(list)
    for i in idx: groups[made[i]].append(i)
    kinds = sorted(groups)
    for perms in itertools.product(*[itertools.permutations(groups[k]) for k in kinds]):
        ren = {}
        for k, p in zip(kinds, perms):
            for new, old in zip(sorted(groups[k]), p): ren[old] = new
        key = (tuple(sorted(made)), tuple(sorted(tuple(sorted([(ren[i], p), (ren[j], q)])) for (i, p), (j, q) in internal)),
               tuple(sorted((ren[i], p) for i, p in free)), bare)
        if best is None or key < best: best = key
        if sum(len(v) for v in groups.values()) > 7: break      # big right sides: one ordering is enough to tell them apart here
    return best


def accounting(Sy):
    """per rule: agents in, agents out; and classes of rules by right side"""
    shapes = defaultdict(list)
    for r in Sy.rules: shapes[right_side_shape(Sy, r)].append(r)
    rows = {}
    for sh, rs in shapes.items():
        for r in rs:
            made = len(r[2])
            cls = ('no agent left' if made == 0 else 'the value alone' if S.is_pass(Sy, r) is not None and r[2] == [r[1]]
                   else 'same as another rule' if len(rs) > 1 else 'its own')
            rows[(r[0], r[1])] = (made - 2, cls, len(rs))
    return rows


def unpolar(k):
    return {'dupI': 'dup', 'dupO': 'dup', 'eraI': 'era', 'eraO': 'era'}.get(k, k)

def run_accounting(Sy, stats, label):
    """fold a run's rule counts (unpolarized labels) onto the table's classes"""
    rows = accounting(Sy)
    byu = defaultdict(list)
    for (a, b), v in rows.items(): byu[frozenset([unpolar(a), unpolar(b)])].append(v)
    tot = Counter(); n = 0
    for key, cnt in stats.items():
        if '⋈' in key: a, b = key.split('⋈')
        elif key.startswith('copy '): _, b, _, a = key.split()
        elif key.startswith('erase '): a, b = 'era', key.split()[1]
        else: continue
        vs = byu.get(frozenset([a, b]))
        if not vs: continue
        dm, cls, _ = vs[0]
        n += cnt
        tot[cls] += cnt
        tot['printed'] += cnt * max(0, dm); tot['freed'] += cnt * max(0, -dm)
        if dm < 0: tot['fewer agents out than in'] += cnt
    return n, tot


def main():
    print('== 1. Rule tables (bits, signature included) ==')
    print(f'{"system":<26} {"kinds":>5} {"rules":>5} {"stuck":>5} {"literal":>8} {"shape":>7} {"default":>8} {"dual default":>13}   coded as')
    C = candidate()
    systems = [S.ic(), S.old(), C]
    for Sy in systems:
        r = report(Sy)
        stuck = r['pairs'] - r['rules']
        print(f'{r["name"]:<26} {r["kinds"]:>5} {r["rules"]:>5} {stuck:>5} {r["lit"] + r["sig"]:>8.0f} {r["sch"] + r["sig"]:>7.0f} '
              f'{r["dflt"] + r["sig"]:>8.0f} {dual_default_bits(Sy) + r["sig"]:>13.0f}   {r["hows"]}  max made {r["maxmade"]}, max ports {r["maxar"] + 1}')
    O = S.old()
    views = sum(rule_bits(O, r)[1] for r in O.rules if r[0] == 'R')
    reads = sum(rule_bits(C, r)[1] for r in C.rules if r[0] == 'C')
    print(f'reading rules: the old five view rules {views:.0f} bits; the candidate\'s five table rules {reads:.0f} bits')
    for Sy in (O, C):
        d = duals(Sy)
        print(f'  duals in {Sy.name}: ' + ', '.join(sorted({" ↔ ".join(sorted([a, b])) for a, b in d.items()})))
    if '-v' in sys.argv:
        for r in C.rules:
            how, b, l = rule_bits(C, r)
            print(f'    {r[0]}⋈{r[1]}: {how} {b:.1f} (literal {l:.1f})')

    print('\n== 2. Programs as nets (agents; bits = log2 kinds per agent + log2 of the wirings) ==')
    kc, ko = C.k, O.k
    rows = []
    for name in ('kindOf', 'size', 'interp'):
        a, ports, b = S.net_bits(onet_of(name), ko)
        rows.append((f'old {name}', a, b))
    for name in ('kindOf', 'size', 'sizeK', 'interp', 'interpT', 'equal'):
        a, ports, b = S.net_bits(knet_of(name), kc)
        rows.append((f'candidate {name}', a, b))
    for label, a, b in rows: print(f'  {label:<22} {a:>5} agents {b:>7.0f} bits')
    bits = {label: b for label, a, b in rows}

    print('\n== 3. J = table + self-interpreter (bits) ==')
    ro, rc = report(O), report(C)
    for label, r, Sy, prog in (('old', ro, O, bits['old interp']), ('candidate, interp', rc, C, bits['candidate interp']),
                               ('candidate, interpT', rc, C, bits['candidate interpT'])):
        t_s, t_d, t_dd = r['sch'] + r['sig'], r['dflt'] + r['sig'], dual_default_bits(Sy) + r['sig']
        print(f'  {label:<20} shape code {t_s + prog:>6.0f}   default {t_d + prog:>6.0f}   dual default {t_dd + prog:>6.0f}')

    print('\n== 4. Cost of the same jobs (rewrites / agents made / reads / erase rewrites) ==')
    old_size = kcheck.old_code('size')
    q = oldcheck.quote
    import tables_check
    jobs = [
        ('size suc', lambda: run_old('size suc'), lambda: run_new('size suc'), lambda: run_new('sizeK suc'), lambda: run_tables('size suc')),
        ('size on the code of the old size', lambda: run_old('size ((^d. size) ρ)'),
            lambda: run_new("size 'oldsize", {'oldsize': old_size}), lambda: run_new("sizeK 'oldsize", {'oldsize': old_size}), None),
        ('size size (each its own size)', lambda: run_old('size size'), lambda: run_new('size size'), lambda: run_new('sizeK sizeK'), lambda: run_tables('size size')),
        ('interpret 1 + 1', lambda: run_old('interp ' + q('add (suc zero) (suc zero)')),
            lambda: run_new("interp '(add (suc zero) (suc zero))"), lambda: run_new("interpT '(add (suc zero) (suc zero))"),
            lambda: run_tables('interp ' + tables_check.quote('add (suc zero) (suc zero)'))),
        ('interpret 2 + 2', lambda: run_old('interp ' + q('add (suc (suc zero)) (suc (suc zero))')),
            lambda: run_new("interp '(add (suc (suc zero)) (suc (suc zero)))"), lambda: run_new("interpT '(add (suc (suc zero)) (suc (suc zero)))"), None),
        ('interpret the interpreter on (λx. x) (λy. y)', lambda: run_old('interp ' + q('interp ' + q('(\\x. x) (\\y. y)'))),
            lambda: run_new("interp '(interp '((\\x. x) (\\y. y)))"), lambda: run_new("interpT '(interpT '((\\x. x) (\\y. y)))"), None),
    ]
    keep = {}
    for label, fo, fn, ft, ftab in jobs:
        print(f'  {label}')
        for who, f in (('old', fo), ('candidate', fn), ('candidate, threaded', ft), ('table design', ftab)):
            if f is None: continue
            st, rw, made, reads, er, n = f()
            keep[(label, who)] = (rw, made, n.stats)
            print(f'      {who:<20} {st:<5} {rw:>9} rewrites {made:>9} made {reads:>5} reads {er:>8} erases   {rw / reads if reads else 0:>7.0f} rewrites a read')
    print('  log2 ratios, old over candidate (threaded): ' + ', '.join(
        f'{math.log2(keep[(l, "old")][0] / keep[(l, "candidate, threaded")][0]):.1f}' for l, *_ in jobs))

    print('\n== 5. Agents in and out, per rule and per run ==')
    for Sy in (O, C):
        rows = accounting(Sy)
        cls = Counter(v[1] for v in rows.values())
        dm = Counter('more' if v[0] > 0 else 'fewer' if v[0] < 0 else 'same' for v in rows.values())
        print(f'  {Sy.name}: {len(rows)} rules; right side {dict(cls)}; agents out against in {dict(dm)}')
    for label in ('size on the code of the old size', 'interpret 1 + 1'):
        for who, Sy in (('old', O), ('candidate, threaded', C)):
            rw, made, stats = keep[(label, who)]
            n, tot = run_accounting(Sy, stats, label)
            print(f'  {label} / {who}: {rw} rewrites ({n} matched to rules): printed {tot["printed"]} agents, freed {tot["freed"]}; '
                  f'{tot["fewer agents out than in"]} rewrites leave fewer agents than they took ({100 * tot["fewer agents out than in"] / n:.0f}%); '
                  f'right side: no agent {tot["no agent left"]}, same as another rule {tot["same as another rule"]}, its own {tot["its own"]}')


def breakdown(stats):
    """a run's rewrites by what they do"""
    out = Counter()
    for key, cnt in stats.items():
        if key.startswith('copy '): out['copy'] += cnt
        elif key.startswith('erase '): out['erase'] += cnt
        elif key == 'dup⋈dup': out['cancel'] += cnt
        elif key.startswith('T⋈'): out['thaw'] += cnt
        elif key.startswith(('C⋈', 'O⋈tab', 'A⋈tab', 'R⋈', 'O⋈rho')): out['read'] += cnt
        elif key.startswith(('O⋈', 'A⋈')): out['apply'] += cnt
    return out


def more():
    """where the rewrites go, and what a fabric with no history would have to forget"""
    C, O = candidate(), S.old()
    old_size = kcheck.old_code('size')
    print('\n== 6. Where the rewrites go (share of all rewrites) ==')
    jobs = [('old: size on the code of the old size', lambda: run_old('size ((^d. size) ρ)')),
            ('candidate, threaded: the same job', lambda: run_new("sizeK 'oldsize", {'oldsize': old_size})),
            ('old: interpret 1 + 1', lambda: run_old('interp ' + oldcheck.quote('add (suc zero) (suc zero)'))),
            ('candidate, threaded: interpret 1 + 1', lambda: run_new("interpT '(add (suc zero) (suc zero))")),
            ('candidate, threaded: the interpreter on itself', lambda: run_new("interpT '(interpT '((\\x. x) (\\y. y)))"))]
    for label, f in jobs:
        st, rw, made, reads, er, n = f()
        b = breakdown(n.stats)
        print(f'  {label:<48} {rw:>7}: ' + ', '.join(f'{k} {100 * b[k] / rw:.0f}%' for k in ('copy', 'cancel', 'erase', 'thaw', 'apply', 'read')))
    print('\n== 7. Rewrites that leave no trace, on adds that erase nothing (the candidate; same programs as the old net) ==')
    print('  a rewrite leaves no trace when its right side has no agent (two wires joined), is the value alone (a thaw')
    print('  absorbed), or is the right side of another rule too (a copy by a split or by a copy in flight)')
    for name, form in (('linear_tree_add', 't'), ('linear_list_add', '')):
        for bits in (4, 16, 64, 256):
            a, b = (1 << bits) - 1, (1 << (bits - 1)) + 1
            n = knet.KNet()
            n.load(oldcheck.program(f'{name} {a}{form} {b}{form}'))
            n.run(100000000)
            cnt, tot = run_accounting(C, n.stats, name)
            lost = tot['no agent left'] + tot['the value alone'] + tot['same as another rule']
            er = sum(v for k, v in n.stats.items() if k.startswith('erase'))
            print(f'  {name:<16} {bits:>4} bits: {cnt:>7} rewrites, {er} erases; no agent left {tot["no agent left"]:>6}, the value alone {tot["the value alone"]:>6}, '
                  f'same as another rule {tot["same as another rule"]:>6}: {100 * lost / cnt:.0f}% leave no trace ({lost / bits:.0f} a bit)')


if __name__ == '__main__':
    if '--more' in sys.argv: more()
    else:
        main()
        more()
