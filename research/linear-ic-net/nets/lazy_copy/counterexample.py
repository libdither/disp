"""Why an eraser may cancel a sharer of a variable (sig) but not a copy in flight (dup).

(λv0. (λv1. ρ) (v0 (Λv1. v0))) ((ρ ρ) ρ): v0 is a view function, copied. Copy 1 is applied (its binder gets
the opened function) and what it returns is erased. The eraser reaches the copier of copy 1's body and,
under cancel-dup, turns it into a wire. But the copier on the binder's side is waiting for the other half
of the copy at a variable deeper in the body, which now never comes: the argument copy 1 was given is
stranded, and copy 2's binder is cut off from its variable."""
from harness import run, problems
from check import run_ref

LEAK = (('λ', 'v0', (('λ', 'v1', 'ρ'), ('v0', ('Λ', 'v1', 'v0')))), (('ρ', 'ρ'), 'ρ'))
# in queue order: an eraser ends up inside the result
ERA = (('λ', 'v0', ((('λ', 'v1', 'v0'), (('λ', 'v1', 'v0'), 'v0')), (('v0', ('ρ', 'ρ')), 'v0'))), ('ρ', ('Λ', 'v0', 'v0')))
# seed 3: a wrong value
WRONG = ((('λ', 'v0', ('λ', 'v1', ('λ', 'v2', ('λ', 'v3', (('λ', 'v4', 'v1'), ('λ', 'v4', 'v3')))))), ('λ', 'z', 'z')),
         ((((('λ', 'z', 'z'), ('λ', 'v0', 'v0')), ('λ', 'z', 'z')), ('λ', 'v0', 'v0')),
          (('λ', 'v0', (('v0', 'v0'), ('v0', ('v0', 'v0')))), ('λ', 'v0', ('v0', ('λ', 'v1', 'v0'))))))
for name, t in (('leak', LEAK), ('eraser in result', ERA), ('wrong value', WRONG)):
    kind, want, _ = run_ref(t)
    print(name, '- reference:', kind)
    for d in ('base', 'cancel', 'cancel-dup'):
        outs = []
        for seed in (None, 1, 2, 3, 4, 5):
            st, res, n = run(d, t, seed, 100000)
            outs.append('ok' if st == 'done' and res == want and not problems(n) else 'WRONG VALUE' if st == 'done' else st)
        print(f'   {d:11}', outs)
