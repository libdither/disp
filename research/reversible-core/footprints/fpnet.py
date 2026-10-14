"""Footprint nets: a scratch prototype (not part of the repo).

Every agent has a front port (port 0, principal) and a back port (last port). A rewrite fuses
its two agents into one footprint that keeps their back links and every outside wire, then
emits the rule's products, each product's back port wired to the footprint. Going back, a
footprint takes back one product at a time (absorb) once nothing has used it, and a footprint
with nothing out splits into the two agents again (unfire). Nothing has a number that a rule
reads: ids here are only for the checker.

Also: the current 26-rule table (rules.rs, no P), run irreversibly with the lattice's
collection, under the same scheduler, for comparison.
"""
import sys, random
from collections import defaultdict, Counter

# ----------------------------------------------------------------------------------------------
# Footprint-net alphabet. Port 0 = front (principal), last port = back.
FP_PORTS = {
    'L': ['p', 'b'], 'S': ['p', 'c', 'b'], 'F': ['p', 'l', 'r', 'b'],
    'A': ['p', 'z', 'r', 'b'], 'T': ['p', 'y', 'z', 'r', 'b'], 'Q': ['p', 'w', 'x', 'y', 'r', 'b'],
    'D': ['p', 'o1', 'o2', 'b'], 'R': ['p', 'o', 'b'], 'N': ['p', 'o', 'b'], 'H': ['p', 'b'],
    'Out': ['in'],
}
FP_VALUES = {'L', 'S', 'F'}
FP_OUT = {'A': (2,), 'T': (3,), 'Q': (4,), 'D': (1, 2), 'R': (1,), 'N': (1,)}  # output ports
FP_MED = set(FP_OUT)


def parse_slot(spec, c, p, fresh, ports):
    who, name = spec.split('.')
    if who == 'C':
        return ('C', ports[c].index(name))
    if who == 'P':
        return ('P', ports[p].index(name))
    k = int(who)
    return ('X', k, ports[fresh[k]].index(name))


class Rule:
    def __init__(self, c, p, fresh, wires, t=0, ports=FP_PORTS, back=True):
        self.c, self.p, self.t, self.fresh = c, p, t, list(fresh)
        self.name = f"{c}·{p}" + (f"@{ports[p][t]}" if t else "")
        self.pairs = []
        self.partner = {}
        for a, b in wires:
            sa, sb = (parse_slot(x, c, p, self.fresh, ports) for x in (a.strip(), b.strip()))
            self.pairs.append((sa, sb))
            self.partner[sa] = sb
            self.partner[sb] = sa
        # perfect matching check over live slots (back ports handled generically)
        nb = 1 if back else 0
        need = {('C', i) for i in range(1, len(ports[c]) - nb)}
        need |= {('P', j) for j in range(0, len(ports[p]) - nb) if j != t}
        for k, f in enumerate(self.fresh):
            need |= {('X', k, q) for q in range(len(ports[f]) - nb)}
        got = [s for pr in self.pairs for s in pr]
        assert len(got) == len(set(got)), (self.name, 'endpoint used twice')
        assert set(got) == need, (self.name, sorted(need - set(got)), sorted(set(got) - need))


def R_(c, p, fresh, wires, t=None, ports=FP_PORTS, back=True):
    w = [tuple(x.split('-')) for x in wires.split()] if wires else []
    tt = 0 if t is None else ports[p].index(t)
    return Rule(c, p, fresh, w, tt, ports, back)


FP_RULES = [
    # apply: △ z = △z ; △x z = △xz ; △xy z = triage on x
    R_('A', 'L', ['S'], "0.p-C.r 0.c-C.z"),
    R_('A', 'S', ['F'], "0.p-C.r 0.l-P.c 0.r-C.z"),
    R_('A', 'F', ['T'], "0.p-P.l 0.y-P.r 0.z-C.z 0.r-C.r"),
    # T faces the fork's left child x, holds y and z
    R_('T', 'L', ['R', 'H'], "0.p-C.y 0.o-C.r 1.p-C.z"),                       # K: y, z held
    R_('T', 'S', ['D', 'A', 'A', 'A'],                                           # S: x z (y z)
       "0.p-C.z 0.o1-1.z 0.o2-2.z 1.p-P.c 1.r-3.p 2.p-C.y 2.r-3.z 3.r-C.r"),
    R_('T', 'F', ['Q'], "0.p-C.z 0.w-P.l 0.x-P.r 0.y-C.y 0.r-C.r"),
    # Q faces the argument z, holds the arms w, x, y
    R_('Q', 'L', ['R', 'H', 'H'], "0.p-C.w 0.o-C.r 1.p-C.x 2.p-C.y"),           # w
    R_('Q', 'S', ['A', 'H', 'H'], "0.p-C.x 0.z-P.c 0.r-C.r 1.p-C.w 2.p-C.y"),   # x u
    R_('Q', 'F', ['A', 'A', 'H', 'H'], "0.p-C.y 0.z-P.l 0.r-1.p 1.z-P.r 1.r-C.r 2.p-C.w 3.p-C.x"),  # y u v
    # copy one layer
    R_('D', 'L', ['L', 'L'], "0.p-C.o1 1.p-C.o2"),
    R_('D', 'S', ['S', 'S', 'D'], "0.p-C.o1 1.p-C.o2 2.p-P.c 2.o1-0.c 2.o2-1.c"),
    R_('D', 'F', ['F', 'F', 'D', 'D'], "0.p-C.o1 1.p-C.o2 2.p-P.l 3.p-P.r 2.o1-0.l 2.o2-1.l 3.o1-0.r 3.o2-1.r"),
    # pass a value on whole
    R_('R', 'L', ['L'], "0.p-C.o"),
    R_('R', 'S', ['S'], "0.p-C.o 0.c-P.c"),
    R_('R', 'F', ['F'], "0.p-C.o 0.l-P.l 0.r-P.r"),
    # normalize (the root's reader)
    R_('N', 'L', ['L'], "0.p-C.o"),
    R_('N', 'S', ['S', 'N'], "0.p-C.o 1.p-P.c 1.o-0.c"),
    R_('N', 'F', ['F', 'N', 'N'], "0.p-C.o 1.p-P.l 1.o-0.l 2.p-P.r 2.o-0.r"),
    # a holder touching an output: un-copy, and dead work
    R_('H', 'D', ['R'], "0.p-P.p 0.o-P.o2", t='o1'),
    R_('H', 'D', ['R'], "0.p-P.p 0.o-P.o1", t='o2'),
    R_('H', 'A', ['H', 'H'], "0.p-P.p 1.p-P.z", t='r'),
    R_('H', 'T', ['H', 'H', 'H'], "0.p-P.p 1.p-P.y 2.p-P.z", t='r'),
    R_('H', 'Q', ['H', 'H', 'H', 'H'], "0.p-P.p 1.p-P.w 2.p-P.x 3.p-P.y", t='r'),
    R_('H', 'R', ['H'], "0.p-P.p", t='o'),
    R_('H', 'N', ['H'], "0.p-P.p", t='o'),
    R_('H', 'H', [], ""),
]
# optional: holders that take a held value apart (an eraser that records)
FP_DISMANTLE = [
    R_('H', 'L', [], ""),
    R_('H', 'S', ['H'], "0.p-P.c"),
    R_('H', 'F', ['H', 'H'], "0.p-P.l 1.p-P.r"),
]


def index_rules(rules):
    ix = {}
    for i, r in enumerate(rules):
        key = (r.c, r.p, r.t)
        assert key not in ix, r.name
        ix[key] = i
    return ix


# ----------------------------------------------------------------------------------------------
def parse_ternary(s, i=0):
    """'0' leaf, '1x' stem, '2xy' fork -> nested tuples."""
    ch = s[i]
    if ch == '0':
        return ('L',), i + 1
    if ch == '1':
        c, i = parse_ternary(s, i + 1)
        return ('S', c), i
    if ch == '2':
        a, i = parse_ternary(s, i + 1)
        b, i = parse_ternary(s, i)
        return ('F', a, b), i
    raise ValueError(ch)


def tern(s):
    t, i = parse_ternary(s)
    assert i == len(s)
    return t


def to_tern(t):
    out = []
    st = [t]
    while st:
        x = st.pop()
        if x[0] == 'L':
            out.append('0')
        elif x[0] == 'S':
            out.append('1'); st.append(x[1])
        else:
            out.append('2'); st.append(x[2]); st.append(x[1])
    return ''.join(out)


# ----------------------------------------------------------------------------------------------
class FpNet:
    def __init__(self, dismantle=False):
        self.kind = {}           # agent id -> kind
        self.link = {}           # endpoint -> endpoint; endpoint = (id, int port) or (fp id, slot tuple)
        self.fps = {}            # fp id -> [rule index, out set, prod dict, cid, pid]
        self.n = 0
        self.rules = FP_RULES + (FP_DISMANTLE if dismantle else [])
        self.ix = index_rules(self.rules)
        self.stats = Counter()

    def new(self, k):
        self.n += 1
        self.kind[self.n] = k
        return self.n

    def connect(self, a, b):
        assert a not in self.link and b not in self.link, (a, b)
        self.link[a] = b
        self.link[b] = a

    def rewire(self, old, new):
        e = self.link.pop(old)
        self.link[e] = new
        self.link[new] = e

    def back(self, x):
        return len(FP_PORTS[self.kind[x]]) - 1

    # -- building
    def build(self, t):
        k = t[0]
        x = self.new(k)
        for q, child in enumerate(t[1:], start=1):
            self.connect((x, q), self.build(child))
        return (x, 0)

    def load_call(self, head, args):
        f = self.build(head)
        for a in args:
            ap = self.new('A')
            self.connect((ap, 0), f)
            self.connect((ap, 1), self.build(a))
            f = (ap, 2)
        nr = self.new('N')
        self.connect((nr, 0), f)
        out = self.new('Out')
        self.connect((nr, 1), (out, 0))
        self.out = out

    def snapshot(self):
        return dict(self.kind), dict(self.link)

    # -- the four laws
    def fire(self, cid, pid, ri):
        r = self.rules[ri]
        f = self.new_fp(ri, cid, pid)
        ck, pk = self.kind[cid], self.kind[pid]
        cb, pb = len(FP_PORTS[ck]) - 1, len(FP_PORTS[pk]) - 1
        assert self.link[(cid, 0)] == (pid, r.t)
        del self.link[(cid, 0)], self.link[(pid, r.t)]
        for i in range(1, cb):
            self.rewire((cid, i), (f, ('C', i)))
        if (cid, cb) in self.link:
            self.rewire((cid, cb), (f, ('Cb',)))
        for j in range(0, pb):
            if j != r.t:
                self.rewire((pid, j), (f, ('P', j)))
        if (pid, pb) in self.link:
            self.rewire((pid, pb), (f, ('Pb',)))
        del self.kind[cid], self.kind[pid]
        for k in range(len(r.fresh)):
            self.emit(f, k)
        return f

    def new_fp(self, ri, cid, pid):
        self.n += 1
        self.fps[self.n] = [ri, set(), {}, cid, pid]
        return self.n

    def emit(self, f, k):
        ri, out, prod, _, _ = self.fps[f]
        r = self.rules[ri]
        kk = r.fresh[k]
        x = self.new(kk)
        for q in range(len(FP_PORTS[kk]) - 1):
            s2 = r.partner[('X', k, q)]
            if s2[0] in ('C', 'P'):
                self.rewire((f, s2), (x, q))
            elif s2[1] in out:
                self.rewire((f, ('X', k, q)), (x, q))
            else:
                self.connect((x, q), (f, s2))
        self.connect((x, len(FP_PORTS[kk]) - 1), (f, ('child', k)))
        out.add(k)
        prod[k] = x

    def fresh(self, f, k):
        x = self.fps[f][2][k]
        return x in self.kind and self.link.get((f, ('child', k))) == (x, self.back(x))

    def absorb(self, f, k):
        ri, out, prod, _, _ = self.fps[f]
        r = self.rules[ri]
        x = prod[k]
        assert self.fresh(f, k)
        kk = self.kind[x]
        for q in range(len(FP_PORTS[kk]) - 1):
            s2 = r.partner[('X', k, q)]
            if s2[0] in ('C', 'P'):
                self.rewire((x, q), (f, s2))
            elif s2[1] in out and s2[1] != k:
                self.rewire((x, q), (f, ('X', k, q)))
            else:
                assert self.link[(x, q)] == (f, s2), (r.name, k, q)
                del self.link[(x, q)], self.link[(f, s2)]
        b = len(FP_PORTS[kk]) - 1
        del self.link[(x, b)], self.link[(f, ('child', k))]
        del self.kind[x]
        out.discard(k)
        del prod[k]

    def unfire(self, f):
        ri, out, prod, cid, pid = self.fps[f]
        assert not out
        r = self.rules[ri]
        self.kind[cid], self.kind[pid] = r.c, r.p
        cb, pb = len(FP_PORTS[r.c]) - 1, len(FP_PORTS[r.p]) - 1
        self.connect((cid, 0), (pid, r.t))
        for i in range(1, cb):
            self.rewire((f, ('C', i)), (cid, i))
        if (f, ('Cb',)) in self.link:
            self.rewire((f, ('Cb',)), (cid, cb))
        for j in range(0, pb):
            if j != r.t:
                self.rewire((f, ('P', j)), (pid, j))
        if (f, ('Pb',)) in self.link:
            self.rewire((f, ('Pb',)), (pid, pb))
        assert not any(e[0] == f for e in self.link) if self.slowcheck else True
        del self.fps[f]

    slowcheck = False

    # -- forward schedule: demand from the root reader, holders always busy
    def wanted(self):
        W = {x for x, k in self.kind.items() if k == 'N'}
        st = list(W)
        while st:
            w = st.pop()
            ports = (0, 1) if self.kind[w] == 'A' else (0,)
            for q in ports:
                e = self.link.get((w, q))
                if e is None:
                    continue
                y, pt = e
                ky = self.kind.get(y)
                if ky in FP_OUT and pt in FP_OUT[ky] and y not in W:
                    W.add(y)
                    st.append(y)
        return W

    def moves(self):
        W = self.wanted()
        used = set()
        mv = []
        # holders first: un-copy before a copy is made
        for h, k in list(self.kind.items()):
            if k != 'H' or h in used:
                continue
            e = self.link.get((h, 0))
            y, pt = e
            ky = self.kind.get(y)
            if ky is None or y in used:
                continue
            ri = self.ix.get(('H', ky, pt))
            if ri is not None and (ky != 'H' or pt == 0):
                used.add(h); used.add(y)
                mv.append((h, y, ri))
        for m in W:
            if m in used:
                continue
            y, pt = self.link[(m, 0)]
            ky = self.kind.get(y)
            if ky in FP_VALUES and pt == 0 and y not in used:
                ri = self.ix[(self.kind[m], ky, 0)]
                used.add(m); used.add(y)
                mv.append((m, y, ri))
        return mv

    def partial(self):
        # footprints that a backward step left with products still in: going forward they emit again
        return [(f, k) for f, (ri, out, prod, _, _) in self.fps.items()
                for k in range(len(self.rules[ri].fresh)) if k not in out]

    def forward(self, max_rounds=10**6, trace=None):
        rounds = 0
        widths = []
        while rounds < max_rounds:
            mv = self.moves()
            em = self.partial()
            if not mv and not em:
                break
            rounds += 1
            widths.append(len(mv) + len({f for f, _ in em}))
            for f, k in em:
                self.emit(f, k)
            for c, p, ri in mv:
                self.fire(c, p, ri)
                self.stats[self.rules[ri].name] += 1
        return rounds, widths

    # -- backward schedules
    def backward(self, mode='one', rng=None):
        rounds = 0
        widths = []
        while self.fps:
            act = []
            for f, (ri, out, prod, _, _) in self.fps.items():
                if not out:
                    act.append(('unfire', f, None))
                    continue
                fr = [k for k in out if self.fresh(f, k)]
                if not fr:
                    continue
                if mode == 'one':
                    act.append(('absorb', f, max(fr)))
                elif mode == 'all':
                    if len(fr) == len(out):
                        act.append(('undo', f, None))
            if not act:
                raise RuntimeError('stuck going back')
            rounds += 1
            widths.append(len(act))
            for kind_, f, k in act:
                if kind_ == 'unfire':
                    self.unfire(f)
                elif kind_ == 'absorb':
                    self.absorb(f, k)
                else:
                    for kk in sorted(self.fps[f][1], reverse=True):
                        self.absorb(f, kk)
                    self.unfire(f)
        return rounds, widths

    def random_walk_back(self, rng):
        steps = 0
        while self.fps:
            f = rng.choice(list(self.fps))
            ri, out, prod, _, _ = self.fps[f]
            if not out:
                self.unfire(f)
            else:
                fr = [k for k in out if self.fresh(f, k)]
                if not fr:
                    continue
                self.absorb(f, rng.choice(fr))
            steps += 1
        return steps

    # -- read the answer
    def readback(self):
        y, pt = self.link[(self.out, 0)]
        return self.read(y, pt)

    def read(self, y, pt):
        k = self.kind.get(y)
        if k not in FP_VALUES or pt != 0:
            return None
        kids = []
        for q in range(1, len(FP_PORTS[k]) - 1):
            c = self.read(*self.link[(y, q)])
            if c is None:
                return None
            kids.append(c)
        return (k, *kids)

    # -- what is left at the answer
    def leftover(self):
        live = Counter(self.kind.values())
        fp_by_rule = Counter(self.rules[v[0]].name for v in self.fps.values())
        # held values: what each holder's front faces, as matter
        held = 0
        held_copies = 0
        seen = set()
        for h, k in self.kind.items():
            if k != 'H':
                continue
            y, pt = self.link[(h, 0)]
            if self.kind.get(y) in FP_VALUES and pt == 0:
                n, nc = self.count_tree(y, seen)
                held += n
                held_copies += nc
        return live, fp_by_rule, held, held_copies

    def count_tree(self, y, seen):
        """agents of a held value, and how many of them are copies a copy footprint can take back."""
        n = nc = 0
        st = [y]
        while st:
            a = st.pop()
            if a in seen or self.kind.get(a) not in FP_VALUES:
                continue
            seen.add(a)
            n += 1
            b = self.link.get((a, self.back(a)))
            if b is not None and b[0] in self.fps and self.rules[self.fps[b[0]][0]].c == 'D':
                nc += 1
            for q in range(1, len(FP_PORTS[self.kind[a]]) - 1):
                st.append(self.link[(a, q)][0])
        return n, nc


# ----------------------------------------------------------------------------------------------
# The current table (rules.rs RULES without P), irreversible, with the lattice's collection.
CUR_PORTS = {
    'L': ['p'], 'S': ['p', 'c'], 'F': ['p', 'l', 'r'], 'Pair': ['p', 'fst', 'snd'],
    'A': ['p', 'arg', 'res'], 'T1': ['p', 'args', 'res'], 'Sel': ['p', 'arms', 'res'],
    'Unp': ['p', 'o1', 'o2'], 'Dn': ['p', 'c1', 'c2'], 'Eps': ['p'], 'Nrm': ['p', 'res'], 'Out': ['in'],
}
CUR_VALUES = {'L', 'S', 'F', 'Pair'}
CUR_OUT = {'A': (2,), 'T1': (2,), 'Sel': (2,), 'Unp': (1, 2), 'Dn': (1, 2), 'Nrm': (1,)}


def C_(c, p, fresh, wires):
    return R_(c, p, fresh, wires, ports=CUR_PORTS, back=False)


CUR_RULES = [
    C_('A', 'L', ['S'], "0.c-C.arg 0.p-C.res"),
    C_('A', 'S', ['F'], "0.l-P.c 0.r-C.arg 0.p-C.res"),
    C_('A', 'F', ['T1', 'Pair'], "0.p-P.l 1.fst-P.r 1.snd-C.arg 0.args-1.p 0.res-C.res"),
    C_('T1', 'L', ['Unp', 'Eps'], "0.p-C.args 0.o1-C.res 0.o2-1.p"),
    C_('T1', 'S', ['Unp', 'Dn', 'A', 'A', 'A'],
       "0.p-C.args 0.o1-2.p 0.o2-1.p 1.c1-2.arg 1.c2-3.arg 3.p-P.c 2.res-4.arg 3.res-4.p 4.res-C.res"),
    C_('T1', 'F', ['Unp', 'Sel', 'Pair', 'Pair'],
       "0.p-C.args 0.o1-3.snd 0.o2-1.p 1.arms-2.p 2.fst-P.l 2.snd-3.p 3.fst-P.r 1.res-C.res"),
    C_('Sel', 'L', ['Unp', 'Eps'], "0.p-C.arms 0.o1-C.res 0.o2-1.p"),
    C_('Sel', 'S', ['Unp', 'Unp', 'Eps', 'Eps', 'A'],
       "0.p-C.arms 0.o1-2.p 0.o2-1.p 1.o1-4.p 1.o2-3.p 4.arg-P.c 4.res-C.res"),
    C_('Sel', 'F', ['Unp', 'Unp', 'Eps', 'Eps', 'A', 'A'],
       "0.p-C.arms 0.o1-2.p 0.o2-1.p 1.o1-3.p 1.o2-4.p 4.arg-P.l 4.res-5.p 5.arg-P.r 5.res-C.res"),
    C_('Unp', 'Pair', [], "C.o1-P.fst C.o2-P.snd"),
    C_('Dn', 'L', ['L', 'L'], "0.p-C.c1 1.p-C.c2"),
    C_('Dn', 'S', ['Dn', 'S', 'S'], "0.p-P.c 0.c1-1.c 0.c2-2.c 1.p-C.c1 2.p-C.c2"),
    C_('Dn', 'F', ['Dn', 'Dn', 'F', 'F'], "0.p-P.l 1.p-P.r 0.c1-2.l 1.c1-2.r 0.c2-3.l 1.c2-3.r 2.p-C.c1 3.p-C.c2"),
    C_('Dn', 'Pair', ['Dn', 'Dn', 'Pair', 'Pair'], "0.p-P.fst 1.p-P.snd 0.c1-2.fst 1.c1-2.snd 0.c2-3.fst 1.c2-3.snd 2.p-C.c1 3.p-C.c2"),
    C_('Eps', 'L', [], ""),
    C_('Eps', 'S', ['Eps'], "0.p-P.c"),
    C_('Eps', 'F', ['Eps', 'Eps'], "0.p-P.l 1.p-P.r"),
    C_('Eps', 'Pair', ['Eps', 'Eps'], "0.p-P.fst 1.p-P.snd"),
    C_('Nrm', 'L', ['L'], "0.p-C.res"),
    C_('Nrm', 'S', ['Nrm', 'S'], "0.p-P.c 0.res-1.c 1.p-C.res"),
    C_('Nrm', 'F', ['Nrm', 'Nrm', 'F'], "0.p-P.l 1.p-P.r 0.res-2.l 1.res-2.r 2.p-C.res"),
]


class CurNet:
    def __init__(self):
        self.kind, self.link, self.n = {}, {}, 0
        self.ix = index_rules(CUR_RULES)
        self.stats = Counter()

    new = FpNet.new
    connect = FpNet.connect

    def build(self, t):
        x = self.new(t[0])
        for q, child in enumerate(t[1:], start=1):
            self.connect((x, q), self.build(child))
        return (x, 0)

    def load_call(self, head, args):
        f = self.build(head)
        for a in args:
            ap = self.new('A')
            self.connect((ap, 0), f)
            self.connect((ap, 1), self.build(a))
            f = (ap, 2)
        nr = self.new('Nrm')
        self.connect((nr, 0), f)
        self.out = self.new('Out')
        self.connect((nr, 1), (self.out, 0))

    def join(self, a, b):
        ea, eb = self.link.pop(a), self.link.pop(b)
        del self.link[ea], self.link[eb]
        if ea == b:
            return
        self.link[ea] = eb
        self.link[eb] = ea

    def rewire(self, old, new):
        e = self.link.pop(old)
        self.link[e] = new
        self.link[new] = e

    def fire(self, cid, pid, ri):
        """plain rewrite: the pair goes, the rule's agents come, wired by the template."""
        r = CUR_RULES[ri]
        self.n += 1
        hub = self.n
        del self.link[(cid, 0)], self.link[(pid, 0)]
        for i in range(1, len(CUR_PORTS[r.c])):
            self.rewire((cid, i), (hub, ('C', i)))
        for j in range(1, len(CUR_PORTS[r.p])):
            self.rewire((pid, j), (hub, ('P', j)))
        del self.kind[cid], self.kind[pid]
        fresh = [self.new(k) for k in r.fresh]

        def end(s):
            return (hub, s) if s[0] in ('C', 'P') else (fresh[s[1]], s[2])
        for sa, sb in r.pairs:
            ea, eb = end(sa), end(sb)
            xa, xb = sa[0] in ('C', 'P'), sb[0] in ('C', 'P')
            if not xa and not xb:
                self.connect(ea, eb)
            elif xa and not xb:
                self.rewire(ea, eb)
            elif xb and not xa:
                self.rewire(eb, ea)
            else:
                x = self.link[ea]
                if x == eb:
                    del self.link[ea], self.link[eb]
                    continue
                y = self.link[eb]
                del self.link[ea], self.link[eb]
                self.link[x] = y
                self.link[y] = x
        assert not any(isinstance(k[1], tuple) and k[0] == hub for k in self.link)

    def collect_moves(self, used, mv):
        # an eraser touching an output collects it, as the lattice does
        for h, k in list(self.kind.items()):
            if k != 'Eps' or h in used:
                continue
            y, pt = self.link[(h, 0)]
            ky = self.kind.get(y)
            if y in used or ky is None:
                continue
            if ky == 'Eps' and pt == 0:
                used |= {h, y}; mv.append(('epseps', h, y))
            elif ky == 'Dn' and pt in (1, 2):
                used |= {h, y}; mv.append(('uncopy', h, y, pt))
            elif ky in ('A', 'T1', 'Sel', 'Nrm') and pt == CUR_OUT[ky][0]:
                used |= {h, y}; mv.append(('dead', h, y))
            elif ky == 'Unp' and pt in (1, 2):
                other = self.link[(y, 3 - pt)]
                if self.kind.get(other[0]) == 'Eps' and other[1] == 0 and other[0] not in used:
                    used |= {h, y, other[0]}; mv.append(('unpaired', h, y, other[0]))

    def do_collect(self, m):
        if m[0] == 'epseps':
            _, h, y = m
            del self.link[(h, 0)], self.link[(y, 0)]
            del self.kind[h], self.kind[y]
        elif m[0] == 'uncopy':
            _, h, y, pt = m
            del self.link[(h, 0)], self.link[(y, pt)]
            self.join((y, 0), (y, 3 - pt))
            del self.kind[h], self.kind[y]
        elif m[0] == 'dead':
            _, h, y = m
            ky = self.kind[y]
            del self.link[(h, 0)], self.link[(y, CUR_OUT[ky][0])]
            del self.kind[h]
            ins = [q for q in range(len(CUR_PORTS[ky])) if q != CUR_OUT[ky][0]]
            for q in ins:
                e = self.new('Eps')
                self.rewire_(y, q, e)
            del self.kind[y]
        elif m[0] == 'unpaired':
            _, h, y, h2 = m
            del self.link[(h, 0)], self.link[(y, 1)], self.link[(h2, 0)], self.link[(y, 2)]
            e = self.new('Eps')
            self.rewire_(y, 0, e)
            del self.kind[h], self.kind[h2], self.kind[y]

    def rewire_(self, y, q, e):
        other = self.link.pop((y, q))
        del self.link[other]
        self.link[(e, 0)] = other
        self.link[other] = (e, 0)

    def wanted(self):
        W = {x for x, k in self.kind.items() if k == 'Nrm'}
        st = list(W)
        while st:
            w = st.pop()
            ports = (0, 1) if self.kind[w] == 'A' else (0,)
            for q in ports:
                y, pt = self.link[(w, q)]
                ky = self.kind.get(y)
                if ky in CUR_OUT and pt in CUR_OUT[ky] and y not in W:
                    W.add(y); st.append(y)
        return W

    def forward(self, max_rounds=10**6):
        rounds, widths = 0, []
        while rounds < max_rounds:
            W = self.wanted()
            used, mv = set(), []
            self.collect_moves(used, mv)
            for m in W | {x for x, k in self.kind.items() if k == 'Unp'}:
                if m in used:
                    continue
                y, pt = self.link[(m, 0)]
                ky = self.kind.get(y)
                if ky in CUR_VALUES and pt == 0 and y not in used:
                    ri = self.ix.get((self.kind[m], ky, 0))
                    if ri is None:
                        continue
                    if self.kind[m] == 'Unp' and m not in W:
                        # an unpair is wanted when one of its outputs is
                        pass
                    used |= {m, y}; mv.append(('fire', m, y, ri))
            # erasers walk into values too
            for h, k in list(self.kind.items()):
                if k != 'Eps' or h in used:
                    continue
                y, pt = self.link[(h, 0)]
                if self.kind.get(y) in CUR_VALUES and pt == 0 and y not in used:
                    used |= {h, y}; mv.append(('fire', h, y, self.ix[('Eps', self.kind[y], 0)]))
            if not mv:
                break
            rounds += 1
            widths.append(len(mv))
            for m in mv:
                if m[0] == 'fire':
                    self.fire(m[1], m[2], m[3])
                    self.stats[CUR_RULES[m[3]].name] += 1
                else:
                    self.do_collect(m)
                    self.stats[m[0]] += 1
        return rounds, widths

    def readback(self):
        y, pt = self.link[(self.out, 0)]
        return self.read(y, pt)

    def read(self, y, pt):
        k = self.kind.get(y)
        if k not in ('L', 'S', 'F') or pt != 0:
            return None
        kids = []
        for q in range(1, len(CUR_PORTS[k])):
            c = self.read(*self.link[(y, q)])
            if c is None:
                return None
            kids.append(c)
        return (k, *kids)
