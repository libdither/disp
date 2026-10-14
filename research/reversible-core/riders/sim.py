#!/usr/bin/env python
"""Abstract model of the 'riders' core: tree calculus where reading never consumes.

Values are a persistent DAG (loaded code/data and nodes the run builds). An application is a
frame holding a pointer to its function and one to its argument. A step moves a pointer down one
node or past one finished frame, or fires a tree-calculus rule as a frame transition. Nothing is
ever freed in a forward run: what a step leaves behind is the history.

Counts, per call: steps (local moves), rounds (every wanted frame takes one step a round, all at
once; what a round makes acts next round), frames made, forks built, sharing points (a second
holder of a value), receipts (frames that keep an argument K or a leaf triage dropped).
"""
import sys
sys.setrecursionlimit(100000)


class Node:
    __slots__ = ("k", "c", "built", "v")

    def __init__(self, k, c=(), built=False):
        self.k, self.c, self.built = k, tuple(c), built
        self.v = None


def label(n, nok=False):
    """Mark the loaded program's fork nodes with the movement shape they have (a label on
    persistent code: applying one is one step; anything else reads the plain node)."""
    st = [n]
    seen = set()
    while st:
        x = st.pop()
        if id(x) in seen:
            continue
        seen.add(id(x))
        st.extend(x.c)
        if x.k != "F":
            continue
        a, b = x.c
        if a.k == "L":
            if not nok:
                x.v = ("K", b)
        elif a.k == "S":
            s = a.c[0]
            if s.k == "F" and s.c[0].k == "L":
                x.v = ("B", s.c[1], b)
            elif b.k == "F" and b.c[0].k == "L":
                x.v = ("C", s, b.c[1])
            elif tern(s) == tern(b):
                x.v = ("W", s)
            else:
                x.v = ("Sx", s, b)
        elif a.k == "F" and a.c[0].k == "L" and a.c[1].k == "L" and b.k == "L":
            x.v = ("I",)


class Frame:
    __slots__ = ("st", "f", "a", "p", "q", "res", "kept", "id", "forked", "fd", "ad", "kp", "fin")
    n = 0

    def __init__(self, f, a):
        self.st, self.f, self.a = "app", f, a
        self.p = self.q = self.res = self.kept = None
        self.forked = False
        self.fd = self.ad = 1
        self.kp = False
        self.fin = -1
        Frame.n += 1
        self.id = Frame.n


def parse(s):
    pos = 0
    # iterative prefix parse: 0 leaf, 1x stem, 2xy fork
    stack = []  # entries: [kind, needed, kids]
    root = None
    while True:
        d = s[pos]; pos += 1
        if d == "0":
            node = Node("L")
        else:
            stack.append(["S" if d == "1" else "F", 1 if d == "1" else 2, []])
            continue
        while True:
            if not stack:
                root = node; break
            top = stack[-1]
            top[2].append(node)
            if len(top[2]) < top[1]:
                break
            stack.pop()
            node = Node(top[0], top[2])
        if root is not None:
            assert pos == len(s), "trailing digits"
            return root


def tern(n):
    out = []
    st = [n]
    while st:
        x = st.pop()
        if x.k == "L": out.append("0")
        elif x.k == "S": out.append("1"); st.append(x.c[0])
        else: out.append("2"); st.append(x.c[1]); st.append(x.c[0])
    return "".join(out)


class Run:
    def __init__(self, head, args, fork=True, clear=False):
        self.fork = fork
        self.clear = clear
        self.slow = "--slow" in sys.argv
        self.kpass = "--kpass" in sys.argv
        self.strict = "--strict" in sys.argv
        self.cur = None
        self.views = "--views" in sys.argv
        if self.views:
            label(head, nok="--nok" in sys.argv)
        self.steps = self.rounds = 0
        self.frames = 0
        self.forks_built = 0
        self.stems = 0
        self.shares = 0
        self.receipts = 0
        self.receipt_kinds = {"K": 0, "leaf": 0}
        self.by = {}
        self.rot = 0
        self.thread = 0
        f = head
        for x in args:
            f = self.frame(f, x)
        self.root = f
        self.peak_wanted = 0
        self.width = []

    def frame(self, f, a):
        self.frames += 1
        return Frame(f, a)

    def count(self, what):
        self.by[what] = self.by.get(what, 0) + 1
        self.steps += 1

    # A ref is a Node or a Frame. A finished frame is either a stem (the frame is the value
    # stem(a)) or a pass (its value is res).
    def shape(self, r):
        """('val', kind) for a value, ('pass', frame) to step past, ('wait', frame) if pending."""
        self.passed = 0
        if self.strict and isinstance(r, Frame) and r.fin == self.rounds and r is not self.cur:
            return ("wait", r, r)
        while self.clear and isinstance(r, Frame) and r.st == "pass" and not (self.kpass and r.kp):
            r = r.res
            self.passed += 1
            if self.strict and isinstance(r, Frame) and r.fin == self.rounds:
                return ("wait", r, r)
        if isinstance(r, Node):
            return ("val", r.k, r)
        if r.st == "stem":
            return ("val", "S", r)
        if r.st == "pass":
            return ("pass", r, r)
        return ("wait", r, r)

    @staticmethod
    def kids(r):
        if isinstance(r, Node):
            return r.c
        return (r.a,)  # a stem frame's child is its argument

    def step(self, fr, want):
        """One step of frame fr. Returns True if it moved."""
        if fr.st == "app":
            sh = self.shape(fr.f)
            if sh[0] == "pass":
                fr.f = sh[1].res; self.count("pass"); return True
            if sh[0] == "wait":
                want.add(sh[1]); return False
            k = sh[1]
            fr.f = sh[2]
            if self.slow and fr.fd > 1:
                fr.fd -= 1; self.rot += 1; self.thread += 1; self.count("rotate"); return True
            self.rot += fr.fd - 1 + self.passed; fr.fd = 1
            if k == "L":            # stem: the frame is the value
                fr.st = "stem"; self.stems += 1; self.count("A.L stem"); return True
            if k == "S":            # fork(b, a): a new fork node, left a pointer into the head
                b = self.kids(fr.f)[0]
                fr.st, fr.res = "pass", Node("F", (b, fr.a), built=True)
                self.thread += 1
                self.forks_built += 1; self.count("A.S fork"); return True
            v = fr.f.v if isinstance(fr.f, Node) and self.views else None
            if v is not None:
                return self.view(fr, v, want)
            # fork function: step into its left child
            fr.p, fr.q = self.kids(fr.f)
            fr.st = "hd"; self.thread += 1; self.count("A.F into left"); return True
        if fr.st == "hd":
            sh = self.shape(fr.p)
            if sh[0] == "pass":
                fr.p = sh[1].res; self.count("pass"); return True
            if sh[0] == "wait":
                want.add(sh[1]); return False
            k = sh[1]
            fr.p = sh[2]
            self.rot += self.passed
            if k == "L":            # K: value is q, the argument is kept (receipt)
                fr.st, fr.res, fr.kept = "pass", fr.q, fr.a
                fr.kp = True
                self.receipts += 1; self.receipt_kinds["K"] += 1; self.count("K"); return True
            if k == "S":            # S: c a (q a)
                c = self.kids(fr.p)[0]
                self.thread += 1
                f1 = self.frame(c, fr.a)
                f2 = self.frame(fr.q, fr.a)
                f2.forked = True
                f1.ad = f2.ad = fr.ad + 1
                self.shares += 2    # a second holder of a; a pointer into q
                if self.clear:          # the frame itself becomes (c a)(q a)
                    fr.f, fr.a, fr.st = f1, f2, "app"
                    fr.fd = fr.ad = 1
                else:
                    f3 = self.frame(f1, f2)
                    fr.st, fr.res = "pass", f3
                    want.add(f3)
                if self.fork:
                    want.add(f2)
                self.count("S spawn"); return True
            fr.st = "tr"; self.count("into arms"); return True
        if fr.st == "tr":
            sh = self.shape(fr.a)
            if sh[0] == "pass":
                fr.a = sh[1].res; self.count("pass"); return True
            if sh[0] == "wait":
                want.add(sh[1]); return False
            k = sh[1]
            fr.a = sh[2]
            if self.slow and fr.ad > 1:
                fr.ad -= 1; self.rot += 1; self.thread += 1; self.count("rotate"); return True
            self.rot += fr.ad - 1 + self.passed; fr.ad = 1
            w, v = self.kids(fr.p)
            if k == "L":            # value w, the leaf is kept (receipt)
                fr.st, fr.res, fr.kept = "pass", w, fr.a
                self.thread += 1
                fr.kp = True
                self.receipts += 1; self.receipt_kinds["leaf"] += 1; self.count("triage leaf"); return True
            if k == "S":            # v u: the same frame, pointers moved down
                (u,) = self.kids(fr.a)
                self.thread += 2
                fr.f, fr.a, fr.st = v, u, "app"
                fr.fd = fr.ad = 1
                self.count("triage stem"); return True
            u, t = self.kids(fr.a)  # q u t
            self.thread += 1
            f1 = self.frame(fr.q, u)
            self.shares += 1        # a pointer to t
            if self.clear:              # the frame itself becomes (q u) t
                fr.f, fr.a, fr.st = f1, t, "app"
                fr.fd = fr.ad = 1
            else:
                f2 = self.frame(f1, t)
                fr.st, fr.res = "pass", f2
                want.add(f2)
            self.count("triage fork"); return True
        return False

    def view(self, fr, v, want):
        c = fr.a
        self.thread += {"K": 1, "I": 0, "B": 3, "C": 2, "W": 2, "Sx": 2}[v[0]]
        if v[0] == "K":             # y; c kept
            fr.st, fr.res, fr.kept = "pass", v[1], c
            fr.kp = True
            self.receipts += 1; self.receipt_kinds["K"] += 1; self.count("view K"); return True
        if v[0] == "I":             # c, nothing lost
            fr.st, fr.res = "pass", c
            self.count("view I"); return True
        if v[0] == "B":             # f (g c): c goes right, unshared
            g = self.frame(v[2], c); g.ad = fr.ad
            g.forked = True             # as the lazy net's fork wants an apply feeding a wanted apply's argument
            fr.f, fr.a, fr.fd, fr.ad = v[1], g, 1, 1
            self.count("view B"); return True
        if v[0] == "C":             # (f c) g: c goes left, unshared
            h = self.frame(v[1], c); h.ad = fr.ad
            fr.f, fr.a, fr.fd, fr.ad = h, v[2], 1, 1
            self.count("view C"); return True
        if v[0] == "W":             # t t, t = s c worked out once and shared
            t = self.frame(v[1], c); t.ad = fr.ad
            self.shares += 1
            fr.f, fr.a, fr.fd, fr.ad = t, t, 2, 2
            self.count("view W"); return True
        # Sx s b: (s c)(b c)
        f1 = self.frame(v[1], c)
        f2 = self.frame(v[2], c)
        f2.forked = True
        f1.ad = f2.ad = fr.ad + 1
        self.shares += 2
        fr.f, fr.a, fr.st, fr.fd, fr.ad = f1, f2, "app", 1, 1
        self.count("view Sx"); return True

    def run(self, limit=10_000_000):
        while self.rounds < limit:
            want = self.wanted()
            if not want:
                return True
            moved = 0
            scratch = set()
            for fr in sorted(want, key=lambda x: x.id):
                self.cur = fr
                before = fr.st
                if self.step(fr, scratch):
                    moved += 1
                    if fr.st in ("stem", "pass") and before not in ("stem", "pass"):
                        fr.fin = self.rounds
            self.width.append(moved)
            self.rounds += 1
            if moved == 0:
                raise RuntimeError("stuck")
        return False

    def wanted(self):
        """Demand from the root: the answer's parts (read back to a normal form), what each
        pending frame waits on, and with fork the S rule's second half while it is reachable."""
        out = set()
        seen = set()
        stack = [self.root]
        while stack:
            r = stack.pop()
            if id(r) in seen:
                continue
            seen.add(id(r))
            if isinstance(r, Node):
                stack.extend(r.c); continue
            if r.st == "stem":
                stack.append(r.a); continue
            if r.st == "pass":
                stack.append(r.res); continue
            out.add(r)
            stack.append({"app": r.f, "hd": r.p, "tr": r.a}[r.st])
            if self.fork and isinstance(r.a, Frame) and getattr(r.a, "forked", False):
                stack.append(r.a)
        return out

    def readback(self):
        def go(r):
            while isinstance(r, Frame) and r.st == "pass":
                r = r.res
            if isinstance(r, Node):
                return Node(r.k, [go(x) for x in r.c])
            assert r.st == "stem", r.st
            return Node("S", [go(r.a)])
        return go(self.root)


def main():
    path = sys.argv[1]
    fork = "--no-fork" not in sys.argv
    print(f"{'call':<44} {'steps':>7} {'rot':>6} {'rounds':>6} {'width':>5} {'frames':>6} {'forks':>5} {'stems':>5} {'shares':>6} {'rcpt K/leaf':>11} {'ok':>3}")
    for line in open(path):
        line = line.strip()
        if not line:
            continue
        name, head, args, want = line.split("|")
        Frame.n = 0
        r = Run(parse(head), [parse(a) for a in args.split(",") if a], fork=fork, clear="--clear" in sys.argv)
        r.run()
        got = tern(r.readback())
        ok = "ok" if got == want else "BAD"
        js = r.shares + r.by.get("view B", 0) + r.by.get("view C", 0)
        if "--csv" in sys.argv:
            print(f"CSV|{name}|{r.steps}|{r.rounds}|{r.frames}|{js}|{r.receipt_kinds['K']}|{r.receipt_kinds['leaf']}|{ok}|{max(r.width)}|{r.thread}")
            continue
        print(f"{name[:44]:<44} {r.steps:>7} {r.rot:>6} {r.rounds:>6} {max(r.width):>5} {r.frames:>6} {r.forks_built:>5} {r.stems:>5} {r.shares:>6} {r.receipt_kinds['K']:>5}/{r.receipt_kinds['leaf']:<5} {ok:>3}")
        if "-v" in sys.argv:
            print("   ", sorted(r.by.items(), key=lambda kv: -kv[1]))


if __name__ == "__main__":
    main()
