"""The candidate net against its reference: hand-written reflective programs and random ones, in three orders.

  python kcheck.py            everything (random programs, the reflective programs, the adds of ../nets)
  python kcheck.py --quick    fewer random programs, no long runs
"""
import sys, os, random, time
from collections import Counter
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', 'nets'))
import kref
from kref import Machine, Stuck, Fuel, canon, parse, NSLOT
from knet import KNet, readback, ARITY, LIVE

sys.setrecursionlimit(1000000)

PROGRAMS = r'''
-- Church numerals and booleans, as in ../nets/check.py
zero := \f x. x
suc := \n f x. f (n f x)
add := \m n f x. m f (n f x)
true := \t e. t
false := \t e. e
and := \x y. x y false
Z := \g. (\h. g (\v. h h v)) (\h. g (\v. h h v))
-- an error: the atom applied
err := ρ ρ
-- a quote of a value: the code ~v
stem := \v. 'v
-- A table [fun | app | quote | atom | table] applied to a value runs the slot for its kind, handed the
-- table itself and then the parts. Slots below name what they are handed: t (or u, v) is the table.
kindOf := [\t f. true | \t g b. false | \t w. false | \t. false | \t a b c d e. false]
-- count binders in code: a function is opened on ρ ('f ρ is its body, as code), an application is read
-- on both sides. The table is its own loop: no fixpoint.
size := [ \t f. suc (t ('f ρ))
        | \t g b. add (t g) (t b)
        | \t w. zero
        | \t. zero
        | \t a b c d e. add (t a) (add (t b) (add (t c) (add (t d) (t e)))) ]
-- the same with the table threaded through every read: it is never copied, and dropped once at the end
sizeK := \c. [ \t f k. t ('f ρ) (\t n. k t (suc n))
             | \t g b k. t g (\t n. t b (\t m. k t (add n m)))
             | \t w k. k t zero
             | \t k. k t zero
             | \t a b c d e k. t a (\t n1. t b (\t n2. t c (\t n3. t d (\t n4. t e (\t n5. k t (add n1 (add n2 (add n3 (add n4 n5)))))))))
             ] c (\t n. n)
-- An interpreter for the calculus. ap x t y applies the value x to the value y, interpreting. 'x y is the
-- code that x y would run, handed back unrun: for a function its body, for a table the slot for y's kind
-- applied to the table and y's parts. So both are opened on y and the code is interpreted. A quote applied
-- opens what it quotes; nothing runs.
ap := [ \u f t y. t ('f y)
      | \u g b t y. err
      | \u w t y. 'w y
      | \u t y. err
      | \u s1 s2 s3 s4 s5 t y. (\x. t ('x y)) [s1 | s2 | s3 | s4 | s5] ]
interp := [ \t f. f
          | \t g b. ap (t g) t (t b)
          | \t w. w
          | \t. ρ
          | \t a b c d e. [t a | t b | t c | t d | t e] ]
-- Equality of programs. fresh a acc wraps acc in one quote for every node of a, so fresh a (fresh b ρ) is
-- deeper than anything inside a or b: no part of either can be mistaken for it. eqd a d b compares code;
-- two functions are both opened on the mark d and compared under the next mark, 'd.
fresh := [ \t f acc. stem (t ('f ρ) acc)
         | \t g b acc. t g (t b acc)
         | \t w acc. stem (t w acc)
         | \t acc. stem acc
         | \t a b c d e acc. t a (t b (t c (t d (t e acc)))) ]
eqd := [ \t f d b. [ \u g t f d. t ('f d) (stem d) ('g d)
                   | \u g2 b2 t f d. false
                   | \u w t f d. false
                   | \u t f d. false
                   | \u a b c d2 e t f d. false ] b t f d
       | \t g1 b1 d b. [ \u g t g1 b1 d. false
                       | \u g2 b2 t g1 b1 d. and (t g1 d g2) (t b1 d b2)
                       | \u w t g1 b1 d. false
                       | \u t g1 b1 d. false
                       | \u a b c d2 e t g1 b1 d. false ] b t g1 b1 d
       | \t w1 d b. [ \u g t w1 d. false
                    | \u g2 b2 t w1 d. false
                    | \u w2 t w1 d. t w1 d w2
                    | \u t w1 d. false
                    | \u a b c d2 e t w1 d. false ] b t w1 d
       | \t d b. [ \u g t d. false
                 | \u g2 b2 t d. false
                 | \u w t d. false
                 | \u t d. true
                 | \u a b c d2 e t d. false ] b t d
       | \t a1 a2 a3 a4 a5 d b.
                 [ \u g t a1 a2 a3 a4 a5 d. false
                 | \u g2 b2 t a1 a2 a3 a4 a5 d. false
                 | \u w t a1 a2 a3 a4 a5 d. false
                 | \u t a1 a2 a3 a4 a5 d. false
                 | \u b1 b2 b3 b4 b5 t a1 a2 a3 a4 a5 d.
                     and (t a1 d b1) (and (t a2 d b2) (and (t a3 d b3) (and (t a4 d b4) (t a5 d b5))))
                 ] b t a1 a2 a3 a4 a5 d ]
equal := \a b. eqd a (fresh a (fresh b ρ)) b
-- The interpreter with both tables threaded through every read (t interprets, u applies): neither is ever
-- copied, and each is dropped once, at the end. Every slot takes a continuation k.
apT := [ \u f t y k. t ('f y) u k
       | \u g b t y k. err
       | \u w t y k. k t u ('w y)
       | \u t y k. err
       | \u s1 s2 s3 s4 s5 t y k. (\x. t ('x y) u k) [s1 | s2 | s3 | s4 | s5] ]
interpT := \c. [ \t f u k. k t u f
               | \t g b u k. t g u (\t u x. t b u (\t u y. u x t y k))
               | \t w u k. k t u w
               | \t u k. k t u ρ
               | \t a b c d e u k. t a u (\t u va. t b u (\t u vb. t c u (\t u vc. t d u (\t u vd. t e u (\t u ve. k t u [va | vb | vc | vd | ve])))))
               ] c apT (\t u v. v)
-- A reader that erases nothing: it swaps the two sides of every application in a piece of code, uses every
-- part it is handed, and hands its table back beside the answer.
pair := \x y k. k x y
mirror := \c. [ \t f k. k t f
              | \t g b k. t g (\t g2. t b (\t b2. k t '(b2 g2)))
              | \t w k. k t 'w
              | \t k. k t ρ
              | \t a b c d e k. k t [a | b | c | d | e]
              ] c (\t v. pair v t)
'''

_OLD = {}
def old_code(name):
    """a closed term of the older calculus (../nets/check.py), to be read as code here"""
    if name not in _OLD:
        import ref, check
        _OLD[name] = ref.parse(check.PROGRAMS + '\n' + name)
    return _OLD[name]

def program(body, given=None):
    return parse(PROGRAMS + '\n' + body, given)

def quote(src):
    return f"'({src})"

TESTS = [
    ('kind of a function', 'kindOf (\\x. x)'),
    ('kind of ρ', 'kindOf ρ'),
    ('kind of a quote', "kindOf '(ρ ρ)"),
    ('binders in λx. x', 'size (\\x. x)'),
    ('binders in λx y. y x', 'size (\\x y. y x)'),
    ('binders in a closure', 'size ((\\x y. x) (\\a b c. c))'),
    ('binders in suc', 'size suc'),
    ('binders in add 2 2', 'size (add (suc (suc zero)) (suc (suc zero)))'),
    ('binders in size itself', 'size size'),
    ('binders in size, table threaded', 'sizeK size'),
    ('binders in the interpreter', 'size interp'),
    ('run after copying a closure', '(\\f. f (f zero)) (add (suc zero))'),
    ('open a function', "''(\\x. x x) ρ"),
    ('open a function held in a variable', "(\\f. 'f ρ) ((\\x. \\y. x y) zero)"),
    ('share a piece of code', "(\\c k. k c c) (''(\\x. x x) ρ)"),
    ('open a shared function twice', "(\\g. 'g ρ ('g zero)) (\\x y. x)"),
    ('a quote of an application', "'((\\x. x x) (\\y. y))"),
    ('a quote that mentions a variable', "(\\x. '(x x)) zero"),
    ('read a quote, then its parts', "[\\t f. ρ | \\t g b. g | \\t w. w | \\t. ρ | \\t a b c d e. ρ] ((\\x. '(x x)) zero)"),
    ('a table read as a table', '[\\t f. ρ | \\t g b. ρ | \\t w. ρ | \\t. ρ | \\t a b c d e. c] kindOf'),
    ('open a table on a function', "(\\k. 'k (\\x. x)) kindOf"),
    ('open a table, then read the code it leaves', "size ((\\k. 'k ρ) kindOf)"),
    ('interpret: a table read as a table', 'interp ' + quote('[\\t f. ρ | \\t g b. ρ | \\t w. ρ | \\t. ρ | \\t a b c d e. c] kindOf')),
    ('equal: λx. x and λy. y', 'equal (\\x. x) (\\y. y)'),
    ('equal: λx y. x and λx y. y', 'equal (\\x y. x) (\\x y. y)'),
    ("equal: λx. x and λx. 'ρ (a forged mark)", "equal (\\x. x) (\\x. 'ρ)"),
    ('equal: suc and suc', 'equal suc suc'),
    ('equal: suc and add', 'equal suc add'),
    ('equal: kindOf and kindOf', 'equal kindOf kindOf'),
    ('equal: size and size', 'equal size size'),
    ('equal: size and sizeK', 'equal size sizeK'),
    ('interpret: identity applied', 'interp ' + quote('(\\x. x) (\\y. y)')),
    ('interpret: one plus one', 'interp ' + quote('add (suc zero) (suc zero)')),
    ('interpret: two plus two', 'interp ' + quote('add (suc (suc zero)) (suc (suc zero))')),
    ('interpret: kind of a function', 'interp ' + quote('kindOf (\\x. x)')),
    ('interpret: binders in suc', 'interp ' + quote('size suc')),
    ('interpret: open a function', 'interp ' + quote("''(\\x. x x) ρ")),
    ('interpret: open a shared function twice', 'interp ' + quote("(\\g. 'g ρ ('g zero)) (\\x y. x)")),
    ('interpret: equal λx. x and λy. y', 'interp ' + quote('equal (\\x. x) (\\y. y)')),
    ('interpret the interpreter', 'interp ' + quote('interp ' + quote('(\\x. x) (\\y. y)'))),
    ('interpret the interpreter, opening', 'interp ' + quote('interp ' + quote("''(\\x. x x) ρ"))),
    ('interpret the interpreter: one plus one', 'interp ' + quote('interp ' + quote('add (suc zero) (suc zero)'))),
    ('threaded: identity applied', 'interpT ' + quote('(\\x. x) (\\y. y)')),
    ('threaded: one plus one', 'interpT ' + quote('add (suc zero) (suc zero)')),
    ('threaded: two plus two', 'interpT ' + quote('add (suc (suc zero)) (suc (suc zero))')),
    ('threaded: kind of a function', 'interpT ' + quote('kindOf (\\x. x)')),
    ('threaded: binders in suc', 'interpT ' + quote('size suc')),
    ('threaded: binders in suc, table threaded', 'interpT ' + quote('sizeK suc')),
    ('threaded: open a shared function twice', 'interpT ' + quote("(\\g. 'g ρ ('g zero)) (\\x y. x)")),
    ('threaded: equal λx. x and λy. y', 'interpT ' + quote('equal (\\x. x) (\\y. y)')),
    ('threaded interpreter interpreting itself', 'interpT ' + quote('interpT ' + quote('(\\x. x) (\\y. y)'))),
    ('threaded interpreter on itself: one plus one', 'interpT ' + quote('interpT ' + quote('add (suc zero) (suc zero)'))),
    ('threaded interpreter on itself, opening', 'interpT ' + quote('interpT ' + quote("''(\\x. x x) ρ"))),
    ('direct interpreter on the threaded one', 'interp ' + quote('interpT ' + quote('(\\x. x) (\\y. y)'))),
]
LONG = {'interpret the interpreter: one plus one', 'equal: size and size', 'equal: size and sizeK'}


# ---- running ----
def reachable(n, root):
    seen, todo = set(), [root]
    while todo:
        a = todo.pop()
        if a in seen: continue
        seen.add(a)
        for i in range(ARITY[n.kind[a]] + 1):
            m = n.mate.get((a, i))
            if m is not None: todo.append(m[0])
    return len(seen)


def run_net(t, seed=None, limit=100000, net=KNet):
    n = net(seed)
    root = n.load(t)
    status = n.run(limit)
    res = None
    if status == 'done':
        res = canon(readback(n, n.mate[(root, 0)]))
        garbage = len(n.kind) - reachable(n, root)
        if garbage: status = f'garbage {garbage}'
    return status, res, n.stats, n


def run_ref(t, fuel=4000):
    m = Machine(fuel)
    try:
        return 'value', canon(m.run(t)), m
    except Stuck:
        return 'stuck', None, m
    except Fuel:
        return 'fuel', None, m


def problems(stats):
    out = []
    if stats['annihilated across two copies']: out.append('annihilated across copies')
    live = [k for k in stats if k.split()[0] in ('copy', 'erase') and k.split()[1] in LIVE]
    if live: out.append(f'live agents copied or erased: {live}')
    return out


def compare(t, seeds=(None, 1, 2)):
    """None when the reference and the net agree in every order, 'skip' when the reference ran out of fuel"""
    kind, want, _ = run_ref(t)
    if kind == 'fuel': return 'skip'
    for s in seeds:
        status, got, stats, _ = run_net(t, s)
        if status == 'limit' and kind == 'value':        # a long run: give it room before calling it wrong
            status, got, stats, _ = run_net(t, s, 30000000)
        bad = problems(stats)
        if bad: return f'seed {s}: {bad}'
        if kind == 'value' and (status != 'done' or got != want):
            return f'seed {s}: net {status} {got}\n   ref {want}'
        if kind == 'stuck' and status not in ('stuck', 'limit'):
            return f'seed {s}: ref stuck, net {status} {got}'
    return None


# ---- random programs ----
PARTS = [1, 2, 1, 0, NSLOT]

def gen(rng, n, env, full, readers=False):
    """a random closed term of about n nodes; with full, quotes, ρ and tables appear; with readers, some
    subterms read a value with a table that has a slot for every kind and uses the parts"""
    if n <= 1:
        pool = env + (['ρ', 'ρ'] if full else [])
        return rng.choice(pool) if pool else 'ρ' if full else ('λ', 'z', 'z')
    if readers and n >= 3 and rng.random() < 0.2:
        return reader(rng, n, env)
    r = rng.random()
    if full and env and n >= 3 and r > 0.94:          # open what a variable holds
        return (('~', rng.choice(env)), gen(rng, n - 2, env, full, readers))
    if n == 2 or r < 0.3:
        x = f'v{len(env)}'
        return ('λ', x, gen(rng, n - 1, env + [x], full, readers))
    if full and r < 0.42:
        return ('~', gen(rng, n - 1, env, full, readers))
    k = rng.randint(1, n - 2)
    return (gen(rng, k, env, full, readers), gen(rng, n - 1 - k, env, full, readers))


def table(rng, env, readers):
    slots = []
    for parts in PARTS:
        xs = [f'v{len(env) + i}' for i in range(1 + parts)]
        body = gen(rng, rng.randint(1, 4), env + xs, True, readers)
        for x in reversed(xs): body = ('λ', x, body)
        slots.append(body)
    return ('tab',) + tuple(slots)


def reader(rng, n, env):
    return (table(rng, env, True), gen(rng, max(1, n // 2), env, True, True))


def random_programs(count):
    rng = random.Random(7)
    tally, bad = Counter(), []
    for family in ('plain', 'quotes', 'readers'):
        for i in range(count):
            t = gen(rng, rng.randint(3, 40), [], family != 'plain', family == 'readers')
            r = compare(t)
            outcome = run_ref(t)[0]
            tally[(family, outcome, 'skip' if r == 'skip' else 'ok' if r is None else 'BAD')] += 1
            if r not in (None, 'skip') and len(bad) < 5: bad.append((t, r))
    print('random programs:')
    for k in sorted(tally): print('  ', *k, tally[k])
    for t, r in bad: print('  MISMATCH', t, '\n  ', r)
    return not bad


def church(v):
    """the number a Church numeral value is, or None: it is run on a step that adds a quote, from ρ"""
    try:
        m = Machine(100000)
        r, n = m.apply(m.apply(v, ('λ', 'q', ('~', 'q'))), 'ρ'), 0
    except (Stuck, Fuel, AssertionError, RecursionError):
        return None
    while kref.is_quote(r): r, n = r[1], n + 1
    return n if r == 'ρ' else None

def show(v):
    if v == canon(parse('\\t e. t')): return 'true'
    if v == canon(parse('\\t e. e')): return 'false'
    c = church(v)
    if c is not None: return str(c)
    s = str(v)
    return s if len(s) < 60 else s[:57] + '…'


def hand_written(quick):
    ok_all = True
    print('reflective programs (β and reads from the reference; rewrites and agents made from the net):')
    for name, src in TESTS:
        if quick and name in LONG: continue
        t = program(src)
        kind, want, m = run_ref(t, 30000000)
        status, got, stats, _ = run_net(t, None, 200000000)
        ok = kind == 'value' and status == 'done' and got == want and not problems(stats)
        others = [run_net(t, s, 200000000)[1] == got for s in (3, 4)]
        ok = ok and all(others)
        ok_all &= ok
        reads = sum(v for k, v in stats.items() if k.startswith('read '))
        print(f"{'ok ' if ok else 'BAD'} {name:44} = {show(want) if kind == 'value' else kind:8} | {m.betas:6} β {reads:5} reads"
              f" | {stats['interactions']:8} rewrites {stats['made']:8} made" + ('' if ok else f'\n   net {status} {got}\n   ref {want}'))
    return ok_all


def interpreted_random(count, nets_every=20):
    """random programs run directly and under each interpreter (in the reference; some as nets too)"""
    ok = True
    for name in ('interp', 'interpT'):
        rng = random.Random(11)
        I = program(name)
        tally, bad = Counter(), []
        for i in range(count):
            t = gen(rng, rng.randint(3, 30), [], i % 3 != 0, i % 3 == 2)
            kind, want, _ = run_ref(t, 3000)
            if kind == 'fuel':
                tally['skipped: direct run out of fuel'] += 1
                continue
            q = (I, ('~', t))
            ikind, got, _ = run_ref(q, 3000000)
            if ikind == 'fuel':
                tally['skipped: interpreted run out of fuel'] += 1
                continue
            same = (ikind, got) == (kind, want)
            if same and i % nets_every == 0:
                status, ngot, stats, _ = run_net(q, i, 30000000)
                same = ((status == 'done' and ngot == got) if kind == 'value' else status in ('stuck', 'limit')) and not problems(stats)
                tally['of which also run as a net'] += 1
            tally[f'{kind}: {"same" if same else "DIFFERENT"}'] += 1
            if not same and len(bad) < 3: bad.append((t, kind, want, ikind, got))
        print(f'random programs under {name}:', dict(sorted(tally.items())))
        for b in bad: print('   DIFFERENT', b)
        ok &= not bad
    return ok


def rename(t, env=None, n=[0]):
    """the same term with every binder renamed"""
    env = env or {}
    if isinstance(t, str): return env.get(t, t)
    if kref.is_quote(t): return ('~', rename(t[1], env))
    if kref.is_tab(t): return ('tab',) + tuple(rename(k, env) for k in t[1:])
    if kref.is_lam(t):
        n[0] += 1
        x = f'r{n[0]}'
        return ('λ', x, rename(t[2], {**env, t[1]: x}))
    return (rename(t[0], env), rename(t[1], env))


def equal_random(count):
    """equal on random functions: a function against itself with its binders renamed, and against others"""
    rng = random.Random(5)
    E = program('equal')
    TRUE, FALSE = canon(parse('\\t e. t')), canon(parse('\\t e. e'))
    tally = Counter()
    vals = []
    while len(vals) < count:
        t = gen(rng, rng.randint(2, 9), [], rng.random() < 0.6)
        if kref.is_lam(t): vals.append(t)
    def ask(a, b):
        kind, got, _ = run_ref(((E, a), b), 20000000)
        return None if kind != 'value' else True if got == TRUE else False if got == FALSE else 'neither'
    for i, a in enumerate(vals):
        r = ask(a, rename(a))
        tally['same function, renamed: ' + ('true' if r is True else f'WRONG {r}')] += 1
        b = vals[(i * 7 + 3) % len(vals)]
        want = canon(a) == canon(b)
        r = ask(a, b)
        tally[f'two functions ({"equal" if want else "different"}): ' + ('right' if r is want else f'WRONG {r}')] += 1
    print('equal on random functions:', dict(sorted(tally.items())))
    return not any('WRONG' in k for k in tally)


def linear_reader():
    t = program("mirror '((\\x. x x) (ρ ρ) '(ρ (\\y. y)))")
    kind, want, m = run_ref(t, 100000)
    status, got, stats, _ = run_net(t, None, 1000000)
    erases = sum(v for k, v in stats.items() if k.startswith('erase'))
    sigs = sum(v for k, v in stats.items() if k.startswith('copy') and k.endswith('by sig'))
    ok = kind == 'value' and status == 'done' and got == want and erases == 0 and sigs == 0
    print(f"{'ok ' if ok else 'BAD'} a reader that erases nothing: {m.reads} reads, {stats['interactions']} rewrites, {erases} erase rewrites, "
          f"{sigs} copies started by a variable used twice, {sum(v for k, v in stats.items() if k.startswith('copy'))} copy rewrites (the slots read)")
    return ok


def old_adds():
    """the adds of ../nets/check.py use only λ: they must run here in exactly as many rewrites as there"""
    import check as oc, net as onet
    names = [(n, s) for n, s in oc.TESTS if 'add' in n and 'binders' not in n and 'interp' not in n and 'closure' not in n]
    same = True
    for name, src in names:
        t = oc.program(src)
        a = oc.run_net(t, None, 60000000)
        b = run_net(t, None, 60000000)
        ok = a[0] == b[0] == 'done' and a[2]['interactions'] == b[2]['interactions'] and str(a[1]) == str(b[1])
        same &= ok
        if not ok: print('  DIFFERENT', name, a[0], a[2]['interactions'], b[0], b[2]['interactions'])
    print(f"the {len(names)} adds of ../nets/check.py: {'same answers and rewrites as the old net' if same else 'DIFFER'}")
    return same


def main():
    quick = '--quick' in sys.argv
    t0 = time.time()
    ok = random_programs(400 if quick else 4000)
    ok &= hand_written(quick)
    ok &= linear_reader()
    ok &= interpreted_random(300 if quick else 3000)
    ok &= equal_random(40 if quick else 300)
    ok &= old_adds()
    print(f"{'ALL OK' if ok else 'FAILURES'}  ({time.time() - t0:.0f}s)")


if __name__ == '__main__':
    main()
