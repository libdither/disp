"""Can the calculus interpret itself? An interpreter written in the calculus, run on programs and on itself.

Three rule tables are compared (net.VARIANT):
  four    the table as first designed: reading a function and reading an opened function give the same view
  five    an opened function gets its own arm, and is handed back as it is
  toggle  the design: five arms, and reading an opened function hands it back closed

`lazy` is the interpreter that only walks application code and hands every call to the host.
`full` also interprets function bodies: to call a function it opens it on the argument and interprets the body.
Under `four`, `full` cannot be written: it gets 6 of the 14 programs wrong, the ones that apply an opened
function, because it cannot tell an opened function from a closed one and must treat them differently.

Run with --quick for the reference interpreter only (about a minute); the full run also runs the nets."""
import sys, random
from collections import Counter
import net, ref, check
from ref import parse, canon, Machine, Stuck, Fuel

sys.setrecursionlimit(1000000)


def set_table(mode):
    net.VARIANT = mode


PRELUDE = r'''
zero := \f x. x
suc := \n f x. f (n f x)
add := \m n f x. m f (n f x)
true := \t e. t
false := \t e. e
Z := \g. (\h. g (\v. h h v)) (\h. g (\v. h h v))
no := ρ
err := (^d. d) ρ ρ
'''

# arms are named by what they are handed: the other arms, then the parts
SOURCES = {
    'four': PRELUDE + r'''
kindOf := \c. ρ c (\a v p F. true) (\l v p g b. false) (\l a p w. false) (\l a v. false)
size := Z (\self c. ρ c
   (\a v p F. suc (self (F ρ)))
   (\l v p g b. add (self g) (self b))
   (\l a p w. zero)
   (\l a v. zero))
lazy := Z (\self c. ρ c
   (\a v p F. c)
   (\l v p g b. (self g) (self b))
   (\l a p w. w)
   (\l a v. ρ))
full := Z (\self c. ρ c
   (\a v p F. c)
   (\l v p g b. (\vf va. ρ vf
        (\a v p F. self (F va))
        (\l v p g b. err)
        (\l a p w. err)
        (\l a v. ρ va)) (self g) (self b))
   (\l a p w. w)
   (\l a v. ρ))
''',
    'five': PRELUDE + r'''
kindOf := \c. ρ c (\o a v p F. true) (\f a v p F. true) (\f o v p g b. false) (\f o a p w. false) (\f o a v. false)
size := Z (\self c. ρ c
   (\o a v p F. suc (self (F ρ)))
   (\f a v p F. suc (self (F ρ)))
   (\f o v p g b. add (self g) (self b))
   (\f o a p w. zero)
   (\f o a v. zero))
lazy := Z (\self c. ρ c
   (\o a v p F. c)
   (\f a v p F. F)
   (\f o v p g b. (self g) (self b))
   (\f o a p w. w)
   (\f o a v. ρ))
full := Z (\self c. ρ c
   (\o a v p F. c)
   (\f a v p F. F)
   (\f o v p g b. (\vf va. ρ vf
        (\o a v p F. self (F va))
        (\f a v p F. F va)
        (\f o v p g b. err)
        (\f o a p w. err)
        (\f o a v. ρ va)) (self g) (self b))
   (\f o a p w. w)
   (\f o a v. ρ))
''',
    'toggle': check.PROGRAMS + 'full := interp\n',
}


def term(mode, body):
    return parse(SOURCES[mode] + '\n' + body)


def quote(t, mode):
    """A program whose value is t's code: open λd. t on ρ."""
    names = ['q1', 'q2', 'q3', 'F'] if mode == 'four' else ['q1', 'q2', 'q3', 'q4', 'F']
    arm = ('F', 'ρ')
    for x in reversed(names): arm = ('λ', x, arm)
    r = (('ρ', ('λ', 'd_', t)), arm)
    for _ in names[:-1]: r = (r, 'ρ')
    return r


def tests(mode):
    """the hand-written programs of check.py, with one more arm where the table has five"""
    l = 'no no no' if mode == 'four' else 'no no no no'
    A = '\\a v p F.' if mode == 'four' else '\\o a v p F.'
    return [
        ('identity applied', '(\\x. x) (\\y. y)'),
        ('two plus two', 'add (suc (suc zero)) (suc (suc zero))'),
        ('kind of a function', 'kindOf (\\x. x)'),
        ('kind of ρ', 'kindOf ρ'),
        ('binders in λx y. y x', 'size (\\x y. y x)'),
        ('binders in a closure', 'size ((\\x y. x) (\\a b c. c))'),
        ('binders in suc', 'size suc'),
        ('run after copying a closure', '(\\f. f (f zero)) (add (suc zero))'),
        ('open then read the body', f'ρ (\\x. x x) ({A} F ρ) {l}'),
        ('read an application part', f'ρ ((\\x. \\y. x y) zero) ({A} F ρ) {l}'),
        ('share a piece of code', f'(\\c k. k c c) (ρ (\\x. x x) ({A} F ρ) {l})'),
        ('share an opened binder', f'(\\g k. k g g) (ρ (\\x y. y x) ({A} F) {l})'),
        ('reflect an opened binder', f'kindOf (ρ (\\x. x) ({A} F) {l})'),
        ('open a shared copy twice', f'(\\g. g ρ (g zero)) (ρ (\\x y. x) ({A} F) {l})'),
    ]


def run_ref(t, fuel):
    m = Machine(fuel)
    try:
        return 'value', canon(m.run(t)), m.betas
    except Stuck:
        return 'stuck', None, m.betas
    except Fuel:
        return 'fuel', None, m.betas


def run_net(t, limit, seed=None):
    status, got, stats = check.run_net(t, seed, limit)
    if stats['annihilated across two copies']: status = 'CROSSED'
    return status, got, stats['interactions']


def show(v, width=46):
    s = str(v)
    return s if len(s) <= width else s[:width - 1] + '…'


def table(mode, interp, levels=1, nets=True, fuel=3000000, limit=30000000, nets_under=10**9):
    """each test run directly and under `levels` interpreters stacked; returns how many disagree.
    The net run is skipped when the interpreted run takes nets_under β-steps or more."""
    bad = 0
    for name, src in tests(mode):
        t = term(mode, src)
        kind, want, betas = run_ref(t, fuel)
        q = t
        for _ in range(levels): q = (term(mode, interp), quote(q, mode))
        ikind, got, ibetas = run_ref(q, fuel)
        same = (ikind, got) == (kind, want)
        line = f"  {'ok ' if same else 'BAD'} {name:28} {betas:5} β direct, {ibetas:8} β interpreted"
        if nets and ikind != 'fuel' and ibetas < nets_under:
            status, ngot, inter = run_net(q, limit)
            dstatus, dgot, dinter = run_net(t, limit)
            agrees = (status == 'done' and ngot == got) if ikind == 'value' else status in ('stuck', 'limit')
            same = same and agrees
            line += f"; net {dinter:6} → {inter:9} interactions{'' if agrees else '  NET DISAGREES: ' + status}"
        if not same:
            bad += 1
            line += f"\n        direct      {kind} {show(want)}\n        interpreted {ikind} {show(got)}"
        print(line)
    return bad


def random_programs(mode, interp, count, seed, nets_every=10):
    rng = random.Random(seed)
    tally = Counter()
    bad = []
    for i in range(count):
        t = check.gen(rng, rng.randint(3, 30), [], i % 2 == 1)
        kind, want, _ = run_ref(t, 3000)
        if kind == 'fuel':
            tally['skipped: direct run out of fuel'] += 1
            continue
        q = (term(mode, interp), quote(t, mode))
        ikind, got, _ = run_ref(q, 2000000)
        if ikind == 'fuel':
            tally['skipped: interpreted run out of fuel'] += 1
            continue
        ok = (ikind, got) == (kind, want)
        if ok and i % nets_every == 0:
            status, ngot, _ = run_net(q, 20000000, seed=i)
            ok = (status == 'done' and ngot == got) if kind == 'value' else status in ('stuck', 'limit')
            tally['of which also run as a net'] += 1
        tally[f'{kind}: {"same" if ok else "DIFFERENT"}'] += 1
        if not ok and len(bad) < 3: bad.append((t, kind, want, ikind, got))
    print('  ', dict(sorted(tally.items())))
    for t, kind, want, ikind, got in bad:
        print('   DIFFERENT', t, '\n      direct', kind, show(want, 90), '\n      interpreted', ikind, show(got, 90))
    return len(bad)


def main():
    quick = '--quick' in sys.argv
    print('== four arms (the table as first designed) ==')
    set_table('four')
    print(' lazy interpreter (hands every call to the host):')
    table('four', 'lazy', nets=not quick)
    print(' full interpreter (interprets function bodies):')
    table('four', 'full', nets=not quick)
    for mode in ('five', 'toggle'):
        print(f'== {mode} ==')
        set_table(mode)
        print(' full interpreter:')
        table(mode, 'full', nets=not quick)
        print(' full interpreter interpreting itself (two levels):')
        table(mode, 'full', levels=2, nets=not quick, fuel=60000000, limit=60000000, nets_under=60000)
        print(' random programs, full interpreter:')
        random_programs(mode, 'full', 300 if quick else 3000, seed=11)
    set_table('toggle')


if __name__ == '__main__':
    main()
