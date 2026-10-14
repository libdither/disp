"""Random terms: answers against an independent tree-calculus evaluator, and random
interleavings of forward fires and backward absorbs/unfires ending exactly at the loaded net."""
import random, sys
from fpnet import FpNet
sys.setrecursionlimit(100000)

class Out(Exception): pass

def apply(f, x, fuel):
    """Tree calculus: L, S(a), F(a, b) values; eager."""
    fuel[0] -= 1
    if fuel[0] < 0: raise Out
    if f[0] == 'L': return ('S', x)
    if f[0] == 'S': return ('F', f[1], x)
    a, y = f[1], f[2]
    if a[0] == 'L': return y                                    # K
    if a[0] == 'S': return apply(apply(a[1], x, fuel), apply(y, x, fuel), fuel)  # S
    w, xx = a[1], a[2]                                          # triage on x
    if x[0] == 'L': return w
    if x[0] == 'S': return apply(xx, x[1], fuel)
    return apply(apply(y, x[1], fuel), x[2], fuel)

def rand_tree(rng, d):
    if d == 0 or rng.random() < 0.3: return ('L',)
    if rng.random() < 0.35: return ('S', rand_tree(rng, d - 1))
    return ('F', rand_tree(rng, d - 1), rand_tree(rng, d - 1))

def rand_moves_forward(f, rng, p):
    for fid, k in f.partial():
        if rng.random() < p: f.emit(fid, k)
    mv = f.moves()
    rng.shuffle(mv)
    k = 0
    for c, q, ri in mv:
        if rng.random() < p and c in f.kind and q in f.kind:
            # still a live pair?
            if f.link.get((c, 0)) == (q, f.rules[ri].t):
                f.fire(c, q, ri); k += 1
    return k

def rand_moves_back(f, rng, p):
    k = 0
    for fid in list(f.fps):
        if fid not in f.fps or rng.random() >= p: continue
        ri, out, prod, _, _ = f.fps[fid]
        if not out:
            f.unfire(fid); k += 1
        else:
            fr = [x for x in out if f.fresh(fid, x)]
            if fr:
                f.absorb(fid, rng.choice(fr)); k += 1
    return k

if __name__ != '__main__':
    raise SystemExit
rng = random.Random(int(sys.argv[1]) if len(sys.argv) > 1 else 1)
tested = agree = exact = 0
for trial in range(4000):
    head = rand_tree(rng, 5)
    args = [rand_tree(rng, 4) for _ in range(rng.randint(1, 3))]
    fuel = [400]
    try:
        v = head
        for a in args: v = apply(v, a, fuel)
    except (Out, RecursionError):
        continue
    f = FpNet(dismantle=True)
    f.load_call(head, args)
    k0, l0 = f.snapshot()
    # wander: forward and back at random, then finish forward
    for _ in range(rng.randint(0, 30)):
        if rng.random() < 0.6: rand_moves_forward(f, rng, 0.5)
        else: rand_moves_back(f, rng, 0.5)
    r, w = f.forward(max_rounds=5000)
    if f.moves():
        continue                      # too long: skip
    tested += 1
    agree += (f.readback() == v)
    # wander again, then all the way back in random order
    for _ in range(rng.randint(0, 30)):
        if rng.random() < 0.4: rand_moves_forward(f, rng, 0.5)
        else: rand_moves_back(f, rng, 0.5)
    f.random_walk_back(rng)
    k1, l1 = f.snapshot()
    exact += (k1 == k0 and l1 == l0)
print(f"terms {tested}, answers agree {agree}, back exactly to the loaded net {exact}")
