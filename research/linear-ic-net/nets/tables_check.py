"""Checks for tables.py, the net whose functions are one agent and whose only reader is a kept table.
  tests    every test of check.py, ported: the value net.py gives for the original program (the same term, or
           where the program text differs the same meaning), the reference agrees, three orders agree on the
           answer and the rewrite count, linear and reversible adds erase nothing. Rewrites next to net.py's.
  tables   reads only a table design can do, against the reference
  random   random programs, plain, with ρ, and with tables (nested at most three deep), against the reference
usage: python tables_check.py [random count per family] [parts: t,x,r] [--no-old]"""
import sys, os, random, time, threading
from collections import Counter
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tables as N
from tables_ref import parse, run_ref, canon, church, binders, NSLOT
import check as old

READERS = r'''
-- Church numerals and booleans, a fixpoint, and reflective programs reading code
zero := \f x. x
suc := \n f x. f (n f x)
add := \m n f x. m f (n f x)
true := \t e. t
false := \t e. e
Z := \g. (\h. g (\v. h h v)) (\h. g (\v. h h v))
-- Reading a value: a kept table keep[fun | opened | app | var | prim | table] applied to a value runs the
-- slot for the value's kind, handed the table itself and then the parts: a function comes opened (F), an
-- opened function closed again (L), an application as its two sides (g b), a spliced value as the value
-- (w), ρ as itself (q), a table as its six slots. A slot that reads again applies the table it is handed.
-- is it a function? each slot drops the table and the parts
kindOf := \c. keep[\t F. true | \t L. true | \t g b. false | \t w. false | \t q. false | \t a1 a2 a3 a4 a5 a6. false] c
-- count binders in code: open each function on ρ, add up the two sides of an application and the six slots of a table
size := keep[\t F. suc (t (F ρ)) | \t L. t L | \t g b. add (t g) (t b) | \t w. zero | \t q. zero
   | \t a1 a2 a3 a4 a5 a6. add (t a1) (add (t a2) (add (t a3) (add (t a4) (add (t a5) (t a6)))))]
-- a slot that is never taken: any value will do
no := ρ
-- an error: a piece of code that is not a function, applied
err := (^d. d) ρ ρ
-- reading a function opens it; reading the opened function closes it again
close := \F. keep[no | \t L. L | no | no | no | no] F
open := \L. keep[\t F. F | no | no | no | no | no] L
-- An interpreter for the calculus. Functions, opened functions, spliced values and ρ are values already.
-- An application is interpreted: both sides are, then a function is opened on the argument and its body is
-- interpreted, an opened function hands back its body as code, a table reads the argument, and applying
-- anything else is an error (err and applying ρ are stuck). A table is interpreted slot by slot.
interp := keep[\t F. close F | \t L. open L
   | \t g b. (\x y. keep[\u F t y. t (F y) | \u L t y. open L y | \u f a t y. err | \u w t y. err | \u q t y. q y
        | \u a1 a2 a3 a4 a5 a6 t y. keep[a1 | a2 | a3 | a4 | a5 | a6] y] x t y) (t g) (t b)
   | \t w. w | \t q. q | \t a1 a2 a3 a4 a5 a6. keep[t a1 | t a2 | t a3 | t a4 | t a5 | t a6]]
'''
PROGRAMS = READERS + old.PROGRAMS[old.PROGRAMS.index('-- The adds.'):]

def program(body):
    return parse(PROGRAMS + '\n' + body)

def quote(src):
    '''source of a program whose value is the code of src: an opened function applied to ρ'''
    return f'((^d. {src}) ρ)'

BODY = 'keep[\\t F. F ρ | no | no | no | no | no]'    # a table that opens a function on ρ and hands back its body
OPENED = 'keep[\\t F. F | no | no | no | no | no]'    # a table that hands back the function opened
ADDS = [(name, src) for name, src in old.TESTS if name.startswith(('linear', 'ordinary', 'reversible'))]
TESTS = [
    ('kind of a function', 'kindOf (\\x. x)'),
    ('kind of ρ', 'kindOf ρ'),
    ('binders in λx. x', 'size (\\x. x)'),
    ('binders in λx y. y x', 'size (\\x y. y x)'),
    ('binders in a closure', 'size ((\\x y. x) (\\a b c. c))'),
    ('binders in suc', 'size suc'),
    ('binders in add 2 2', 'size (add (suc (suc zero)) (suc (suc zero)))'),
    ('binders in size itself', 'size size'),
    ('run after copying a closure', '(\\f. f (f zero)) (add (suc zero))'),
    ('open then read the body', f'{BODY} (\\x. x x)'),
    ('read an application part', f'{BODY} ((\\x. \\y. x y) zero)'),
    ('share a piece of code', f'(\\c k. k c c) ({BODY} (\\x. x x))'),
    ('share a spliced value', f'(\\c k. k c c) ({BODY} (\\x. x))'),
    ('share an opened binder', f'(\\g k. k g g) ({OPENED} (\\x y. y x))'),
    ('reflect an opened binder', f'kindOf ({OPENED} (\\x. x))'),
    ('open a shared copy twice', f'(\\g. g ρ (g zero)) ({OPENED} (\\x y. x))'),
    ('read, close, then run', 'close (open suc) zero'),
    ('binders in an opened function', f'size ({OPENED} (\\x y. y x))'),
    ('an opened function as a quote', '(^d. (\\x. x x) (\\y. y)) ρ'),
    ('interpret a quote', 'interp ((^d. add (suc zero) (suc zero)) ρ)'),
    ('interpret: identity applied', 'interp ' + quote('(\\x. x) (\\y. y)')),
    ('interpret: two plus two', 'interp ' + quote('add (suc (suc zero)) (suc (suc zero))')),
    ('interpret: binders in suc', 'interp ' + quote('size suc')),
    ('interpret: open then read the body', 'interp ' + quote(f'{BODY} (\\x. x x)')),
    ('interpret: open a shared copy twice', 'interp ' + quote(f'(\\g. g ρ (g zero)) ({OPENED} (\\x y. x))')),
    ('interpret the interpreter', 'interp ' + quote('interp ' + quote('(\\x. x) (\\y. y)'))),
    ('interpret the interpreter, reading', 'interp ' + quote('interp ' + quote(f'{BODY} (\\x. x x)'))),
    *ADDS,
]
# what a test means where its program text differs from check.py's, so net.py's answer cannot be the same term
MEANING = {
    'binders in size itself': lambda got: church(got) == binders(program('size')),
}
# reads only a table design can do: the reference gives the answer; a count of binders is checked against the text
TABLE_TESTS = [
    ('binders in a table', 'size keep[\\t F. F | \\t L. L | no | no | no | no]'),
    ('binders in size, as code', 'size ' + quote('size')),
    ('kind of a table', 'kindOf size'),
    ('a table reads a table', 'keep[no | no | no | no | no | \\t a1 a2 a3 a4 a5 a6. a2] size'),
    ('interpret: binders in a table', 'interp ' + quote('size keep[\\t F. F | no | no | no | no | no]')),
    ('interpret: kind of a function', 'interp ' + quote('kindOf (\\x. x)')),
    ('interpret: a table built from interpreted slots', 'interp ' + quote('keep[\\t F. F | no | no | no | no | no]')),
    ('interpret: binders in size', 'interp ' + quote('size size')),
    ('interpret the interpreter on a table read', 'interp ' + quote('interp ' + quote('kindOf ρ'))),
]
TABLE_MEANING = {
    'binders in a table': lambda got: church(got) == 4,
    'binders in size, as code': lambda got: church(got) == binders(program('size')),
    'interpret: binders in a table': lambda got: church(got) == 2,
    'interpret: binders in size': lambda got: church(got) == binders(program('size')),
}

SEEDS = (None, 3, 4)


def reachable(n, root):
    seen, todo = set(), [root]
    while todo:
        a = todo.pop()
        if a in seen: continue
        seen.add(a)
        for i in range(n.ar[a] + 1):
            m = n.mate.get((a, i))
            if m is not None: todo.append(m[0])
    return len(seen)


def run_net(t, seed=None, limit=100000):
    """status, answer, stats, net"""
    n = N.Net(seed)
    root = n.load(t)
    status = n.run(limit)
    res = None
    if status == 'done':
        res = canon(N.readback(n, n.mate[(root, 0)]))
        garbage = len(n.kind) - reachable(n, root)
        if garbage: status = f'garbage {garbage}'
    return status, res, n.stats, n


STUCK = {('O', 'rho'), ('O', 'app'), ('O', 'var')}      # applying ρ, or code

def problems(n):
    """stuck pairs other than applying ρ or code"""
    return sorted({f'{a}⋈{b}' for a, b in n.stuck if (a, b) not in STUCK and (b, a) not in STUCK})


def erases(stats):
    return sum(v for k, v in stats.items() if k.startswith('erase'))


def orders(t, limit=60000000):
    """the runs in the three orders: (status, answer, rewrites, problems)"""
    return [(st, res, stats['interactions'], problems(n)) for st, res, stats, n in (run_net(t, s, limit) for s in SEEDS)]


def tests(with_old=True):
    bad = 0
    old_src = dict(old.TESTS)
    rows = []
    print(f"{'':4}{'test':46}{'net.py':>10}{'tables':>9}{'ratio':>10}  answer")
    for name, src in TESTS:
        t = program(src)
        runs = orders(t)
        st, got, count, _ = runs[0]
        same_orders = all(r[0] == st and r[1] == got and r[2] == count and not r[3] for r in runs)
        rk, rv, _ = run_ref(t, 3000000)
        ref_ok = rk == 'value' and rv == got
        lin_ok = not name.startswith(('linear', 'reversible')) or erases(run_net(t, None, 60000000)[2]) == 0
        before, verdict = None, 'not compared'
        if with_old:
            ost, want, ostats = old.run_net(old.program(old_src[name]), None, 60000000)
            before = ostats['interactions']
            if ost != 'done': verdict = None
            elif got == want: verdict = 'the same term'
            elif name in MEANING and MEANING[name](got): verdict = 'the same meaning'
            elif church(got) is not None and church(got) == church(want): verdict = f'the same number {church(got)}'
            else: verdict = None
        ok = st == 'done' and same_orders and ref_ok and lin_ok and verdict is not None
        bad += not ok
        ratio = f'{before / count:9.1f}x' if before else ''
        print(f"{'ok ' if ok else 'BAD'} {name:46}{before or '':>10}{count:>9}{ratio:>10}  {verdict}"
              + ('' if ok else f'\n    status {st}, orders {same_orders}, ref {rk} {ref_ok}, erase-free {lin_ok}\n    got {str(got)[:200]}'))
        rows.append((name, before, count))
    return bad, rows


def table_tests():
    bad = 0
    for name, src in TABLE_TESTS:
        t = program(src)
        runs = orders(t)
        st, got, count, _ = runs[0]
        same_orders = all(r[0] == st and r[1] == got and r[2] == count and not r[3] for r in runs)
        rk, rv, _ = run_ref(t, 3000000)
        ok = st == 'done' and same_orders and rk == 'value' and rv == got and TABLE_MEANING.get(name, lambda g: True)(got)
        bad += not ok
        print(f"{'ok ' if ok else 'BAD'} {name:50}{count:>9} rewrites   {str(got)[:60]}"
              + ('' if ok else f'\n    status {st}, orders {same_orders}, ref {rk} {rv == got}, meaning {church(got)}'))
    return bad


# ---- random programs ----
NPARTS = [1, 1, 2, 1, 1, NSLOT]

def gen(rng, n, env, rho, tables, d=0):
    """a random closed term with about n nodes whose names come from env; with rho, ρ and opened functions appear
    too; with tables, tables are read and handed round as values, d of them deep at most three"""
    if n == 1:
        pool = env + (['ρ', 'ρ'] if rho else [])
        return rng.choice(pool) if pool else 'ρ' if rho else ('λ', 'z', 'z')
    if tables and d < 3 and n >= 3:
        r = rng.random()
        if r < 0.2: return (table(rng, env, d), gen(rng, max(1, n // 2), env, rho, tables, d))
        if r < 0.27: return table(rng, env, d)
    if n == 2 or rng.random() < 0.3:
        x = f'v{len(env)}'
        binder = 'Λ' if rho and rng.random() < 0.15 else 'λ'
        return (binder, x, gen(rng, n - 1, env + [x], rho, tables, d))
    k = rng.randint(1, n - 2)
    return (gen(rng, k, env, rho, tables, d), gen(rng, n - 1 - k, env, rho, tables, d))

def table(rng, env, d):
    """a table each of whose slots takes the table and the parts, its body a small random term over them"""
    slots = []
    for i in range(NSLOT):
        xs = [f'v{len(env) + j}' for j in range(1 + NPARTS[i])]
        body = gen(rng, rng.randint(1, 4), env + xs, True, True, d + 1)
        for x in reversed(xs): body = ('λ', x, body)
        slots.append(body)
    return ('keep',) + tuple(slots)


def compare(t, limit=100000):
    """None when the net agrees with the reference in every order, 'skip', 'limit', or a description"""
    kind, want, _ = run_ref(t)
    if kind == 'fuel': return 'skip'
    count = None
    for s in SEEDS:
        status, got, stats, n = run_net(t, s, limit)
        if status == 'limit': return 'stuck, then runs on' if kind == 'stuck' and n.stuck else 'limit'
        p = problems(n)
        if p: return f'seed {s}: {p}'
        if count is not None and stats['interactions'] != count: return f'seed {s}: {stats["interactions"]} rewrites, not {count}'
        count = stats['interactions']
        if kind == 'value' and (status != 'done' or got != want): return f'seed {s}: net {status} {got}\n   ref {want}'
        if kind == 'stuck' and status != 'stuck': return f'seed {s}: ref stuck, net {status} {got}'
    return None


def random_tests(count, seed=7):
    rng = random.Random(seed)
    tally, bad, limits = Counter(), [], []
    for family in ('plain', 'with ρ', 'tables'):
        for _ in range(count):
            t = gen(rng, rng.randint(3, 40 if family != 'tables' else 30), [], family != 'plain', family == 'tables')
            r = compare(t)
            if r == 'limit':
                limits.append(t)
            outcome = run_ref(t)[0]
            tally[(family, outcome, r if r in ('skip', 'limit', 'stuck, then runs on') else 'ok' if r is None else 'BAD')] += 1
            if r not in (None, 'skip', 'limit', 'stuck, then runs on') and len(bad) < 5: bad.append((t, r))
    for k in sorted(tally): print('  ', *k, tally[k])
    for t, r in bad: print('  MISMATCH', str(t)[:400], '\n  ', r)
    for t in limits:
        r = compare(t, 5000000)
        print(f'   past 100000 rewrites, rerun to 5M: {"agrees" if r is None else r}')
        if r not in (None, 'skip', 'stuck, then runs on'): bad.append((t, r))
    return len(bad)


def main():
    t0 = time.time()
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    count = int(args.pop(0)) if args and args[0].isdigit() else 1500
    parts = args[0].split(',') if args else ['t', 'x', 'r']
    bad = 0
    if 't' in parts:
        print('## check.py\'s tests, ported (rewrites: net.py on the original, tables.py on the port)')
        b, _ = tests('--no-old' not in sys.argv); bad += b
        print(f'  ({time.time() - t0:.0f}s)')
    if 'x' in parts:
        print('## reads only a table design can do (against the reference)')
        bad += table_tests()
        print(f'  ({time.time() - t0:.0f}s)')
    if 'r' in parts:
        print(f'## random programs, {count} a family, three orders, against the reference')
        bad += random_tests(count)
        print(f'  ({time.time() - t0:.0f}s)')
    print('ALL OK' if not bad else f'{bad} FAILURES', f'({time.time() - t0:.0f}s)')


def in_thread(main):
    sys.setrecursionlimit(1000000)
    threading.stack_size(512 * 1024 * 1024)
    th = threading.Thread(target=main)
    th.start()
    th.join()


if __name__ == '__main__':
    in_thread(main)
