"""Properties (1) and (2) in isolation, as the code or the captured data grows. Total rewrites, queue order.
  copy-erase code N     (λf. (λa b. a) f f) big           big: a closed function with N applications
  copy-erase data N     (λd. (λf. (λa b. a) f f) (λx. d)) data      data: a list of N cells built at run time,
                        minus the same program without the copy, (λd. (λf. f) (λx. d)) data
  copy, run, keep inner (λf. (λa b. a) (f ρ) (f ρ)) (λx. λy. big body)   each copy runs, its inner λ is not
"""
import sys
from harness import run, problems
from check import run_ref
from engine import DESIGNS


def big(n):
    body = 'x'
    for _ in range(n): body = (body, 'x')
    return ('λ', 'x', body)

def data(n):
    cons = ('λ', 'h', ('λ', 't', ('λ', 'k', (('k', 'h'), 't'))))
    t = ('λ', 'k', 'k')
    for _ in range(n): t = ((cons, 'ρ'), t)
    return t

K = ('λ', 'a', ('λ', 'b', 'a'))
def copy_erase(v): return (('λ', 'f', ((K, 'f'), 'f')), v)
CASES = {
    'copy-erase code': lambda n: copy_erase(big(n)),
    'copy-erase data': lambda n: (('λ', 'd', copy_erase(('λ', 'x', 'd'))), data(n)),
    'copy, run, keep inner': lambda n: (('λ', 'f', ((K, ('f', 'ρ')), ('f', 'ρ'))), ('λ', 'x', ('λ', 'y', big(n)[2]))),
}
designs = sys.argv[1:] or [d for d in DESIGNS if d not in ('cancel-dup', 'ref+cancel', 'share+cancel', 'lazy+cancel')]
print(f"{'case':24} {'N':>5} " + ' '.join(f'{d:>8}' for d in designs))
for name, mk in CASES.items():
    for N in (10, 100, 1000):
        t = mk(N)
        want = run_ref(t, 10 ** 6)[1]
        row = []
        for d in designs:
            st, res, n = run(d, t)
            ok = st == 'done' and res == want and not problems(n)
            cost = n.stats['interactions']
            if name == 'copy-erase data':
                cost -= run(d, (('λ', 'd', (('λ', 'f', 'f'), ('λ', 'x', 'd'))), data(N)))[2].stats['interactions']
            row.append(f"{cost:8}" + ('' if ok else '!'))
        print(f'{name:24} {N:5} ' + ' '.join(row))
