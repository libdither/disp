import random, sys
from fpnet import FpNet
from randtest import apply, rand_tree, Out
rng = random.Random(5)
bad = 0; n = 0
for trial in range(2000):
    head = rand_tree(rng, 5); args = [rand_tree(rng, 4) for _ in range(rng.randint(1, 3))]
    fuel = [400]
    try:
        v = head
        for a in args: v = apply(v, a, fuel)
    except (Out, RecursionError):
        continue
    f = FpNet(dismantle=True); f.load_call(head, args)
    f.forward(max_rounds=5000)
    if f.moves(): continue
    n += 1
    got = f.readback()
    if got != v:
        bad += 1
        if bad <= 3:
            print('head', head, 'args', args); print('want', v); print('got ', got)
            print('live', sorted(f.kind.values()))
print(n, bad)
