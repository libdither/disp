"""Which pairs meet, over random programs and the reflective tests."""
import random
from collections import Counter
from check import gen, run_net, program, TESTS
seen = Counter()
rng = random.Random(11)
for rho in (False, True):
    for _ in range(3000):
        t = gen(rng, rng.randint(3, 40), [], rho)
        seen.update(k for k in run_net(t, None, 20000)[2] if k not in ('interactions', 'annihilated across two copies'))
for _, src in TESTS:
    seen.update(k for k in run_net(program(src), None, 3000000)[2] if k != 'interactions')
for k in sorted(seen): print(f'  {k}')
