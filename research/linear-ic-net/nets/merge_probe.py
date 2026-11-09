"""Flaw 1 run for real: the variable split compiled as an ordinary copier (so a copy in flight
cancels against it instead of passing through). Counts wrong answers against the reference."""
import random
from collections import Counter
import net, check
from net import Net

def bind_merged(self, port, occ):
    if not occ:
        self.link(port, (self.new('era'), 0)); return
    while len(occ) > 1:
        s = self.new('dup')
        self.link(port, (s, 0)); self.link((s, 1), occ.pop(0)); port = (s, 2)
    self.link(port, occ[0])
Net.bind = bind_merged
net.SHARERS = ('sig', 'dup')

rng = random.Random(7)
tally = Counter()
example = None
for _ in range(3000):
    t = check.gen(rng, rng.randint(3, 40), [], False)
    kind, want, _ = check.run_ref(t)
    if kind != 'value': continue
    try:
        status, got, _ = check.run_net(t)
    except ValueError:
        status, got = 'unreadable', None
    ok = status == 'done' and got == want
    tally['right' if ok else status if status != 'done' else 'wrong'] += 1
    if not ok and (example is None or len(str(t)) < len(str(example[0]))): example = (t, status, got, want)
print(dict(tally))
t = check.parse('(\\f. f (f u)) (\\x. x x)')
try:
    got = check.run_net(t)[:2]
except ValueError as e:
    got = f'no answer ({e})'
print('copy λx. x x, apply twice:', got, '\n   reference:', check.run_ref(t)[:2])
print('smallest wrong one:', example)
