"""Write ../lean/QuoteKit.lean: the quote-and-table net (ktable.py) and its kit (`reify` below, and a native
`run`) as Lean data for the fixed problem statement (../lean/Statement.lean, v0.2).

`reify` is written in the net's own term language (kref.py), checked here against the reference machine on
every closed pure λ-term up to a size and on random larger ones, quoted the way the statement quotes them
('t), its answer compared literally with the statement's `encode` (after reading values in place); then on
the net itself (knet.py) for a sample. It is compiled by knet.py's `emit` and exported as agents plus
wiring; knet.py's unpolarized eraser on a binder becomes `eraI`.

`run` is the thaw: λq. T q, two agents, written out by hand (the term language has no thaw). The table
interpreter of the v0.1 record is still exported, as `interpProg`, to price native running against it.

  python kexport.py            check, then write ../lean/QuoteKit.lean
  python kexport.py --check N  check on N random terms only
"""
import sys, os, random
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(HERE, '..', 'nets'))
sys.setrecursionlimit(100000)
import kref, ktable
from kref import parse, Machine, Stuck, Fuel, canon
from knet import KNet

# A table [fun | app | quote | atom | table] applied to code runs the slot for its kind on the table and the
# parts. Pure λ-terms never show an atom or a table, so those slots are ρ.
KIT = r'''
-- reify: reading code at depth d (a Scott numeral) with token chain T answers its `encode`. A function is
-- opened on a token T2 (its variables read back as quotes of T2) and its body read one deeper. The token
-- is the chain T2 = λp. p ρ T: applied to the depth numeral at a use it strips one successor and passes the
-- rest to the enclosing binder's token, down to the root's λn. n, so a use answers (use depth - binder
-- depth - 1), the de Bruijn index, as a piece of the depth numeral. Children are built before parents.
reify := \q. [ \t f d T. (\T2. (\e. \a b c. c e) (t ('f T2) (\z s. s d) T2)) (\p. p ρ T)
             | \t g x d T. (\G X. \a b c. b G X) (t g d T) (t x d T)
             | \t w d T. (\i. \a b c. a i) (w d)
             | ρ | ρ ] q (\z s. z) (\n. n)
-- reifyK (measured, not used): the same in continuation-passing style, the table handed back instead of
-- shared, so it is not copied at each application: fewer moves and records, more agents and rounds.
reifyK := \q. [ \t f d T k. (\T2. t ('f T2) (\z s. s d) T2 (\E. k (\a b c. c E))) (\p. p ρ T)
              | \t g x d T k. t g d T (\G t2. t2 x d T (\X. k (\a b c. b G X)))
              | \t w d T k. (\i. k (\a b c. a i) t) (w d)
              | ρ | ρ ] q (\z s. z) (\n. n) (\E t. E)
-- interp (v0.1's, kept to compare): a function is its own value; an application runs the function, then
-- the argument, then opens the function on it and runs that; a variable is a quote of its value.
interp := [ \t f. f | \t g b. (\x. t ('x (t b))) (t g) | \t w. w | ρ | ρ ]
'''


def prog(name):
    return parse(KIT + '\n' + name)


def gen(rng, n, d):
    """a random closed pure λ-term of about n nodes; binders are named by depth"""
    if n <= 1 and d > 0: return f'v{rng.randrange(d)}'
    if n <= 2 or d == 0 or rng.random() < 0.35: return ('λ', f'v{d}', gen(rng, n - 1, d + 1))
    k = rng.randint(1, n - 2)
    return (gen(rng, k, d), gen(rng, n - 1 - k, d))


def run(t, fuel):
    try: return 'value', canon(Machine(fuel).run(t))
    except Stuck: return 'stuck', None
    except Fuel: return 'fuel', None


def unquote(t):
    """the term as the net reads it back: a variable given a value is that value, in place"""
    if isinstance(t, str): return t
    if kref.is_quote(t): return unquote(t[1])
    if kref.is_lam(t): return ('λ', t[1], unquote(t[2]))
    return (unquote(t[0]), unquote(t[1]))


def scott(n):
    """λz s. z, and λz s. s n'"""
    t = ('λ', 'z', ('λ', 's', 'z'))
    for _ in range(n): t = ('λ', 'z', ('λ', 's', ('s', t)))
    return canon(t)


def encode(t, env=()):
    """the statement's `encode`: λa b c. a ⌜i⌝ | b ⌜f⌝ ⌜x⌝ | c ⌜e⌝, de Bruijn indices as Scott numerals"""
    abc = lambda x: ('λ', 'a', ('λ', 'b', ('λ', 'c', x)))
    if isinstance(t, str): return abc(('a', scott(env[::-1].index(t))))
    if kref.is_lam(t): return abc(('c', encode(t[2], env + (t[1],))))
    return abc((('b', encode(t[0], env)), encode(t[1], env)))


def closed_terms(n, d=0):
    """every term of exactly n nodes with free variables below depth d"""
    if n == 1: return [f'v{i}' for i in range(d)]
    out = [('λ', f'v{d}', b) for b in closed_terms(n - 1, d + 1)]
    for k in range(1, n - 1):
        out += [(f, a) for f in closed_terms(k, d) for a in closed_terms(n - 1 - k, d)]
    return out


def check_one(R, RK, I, t, net=False):
    """both reifiers on t compared literally with encode t; the interpreter where t has a value; with net,
    reify also on the net"""
    bad = 0
    for name, r in (('reify', R), ('reifyK', RK)):
        k, v = run((r, ('~', t)), 10 ** 7)
        if k != 'value' or canon(unquote(v)) != canon(encode(t)): bad += 1; print(name, 'differs on', t)
    want = run(t, 3000)
    if want[0] == 'value':
        k, v = run((I, ('~', t)), 300000)
        if k != 'value' or canon(unquote(v)) != canon(unquote(want[1])): bad += 1; print('interp differs on', t)
    if net:
        import kcheck
        st, got, _, _ = kcheck.run_net((R, ('~', t)), limit=10 ** 7)
        if st != 'done' or canon(unquote(got)) != canon(encode(t)): bad += 1; print('reify on the net:', st, t)
    return bad


def check(count, upto=9, size=120):
    R, RK, I = prog('reify'), prog('reifyK'), prog('interp')
    rng = random.Random(3)
    every = [t for n in range(1, upto + 1) for t in closed_terms(n)]
    rand = [gen(rng, rng.randint(1, size), 0) for _ in range(count)]
    bad = sum(check_one(R, RK, I, t, net=(i % 25 == 0)) for i, t in enumerate(every))
    bad += sum(check_one(R, RK, I, t, net=(i % 5 == 0)) for i, t in enumerate(rand))
    print(f'reify, reifyK, interp on all {len(every)} closed terms up to size {upto} and {count} random ones up '
          f'to size {size} (reify also on the net for one in 25 and one in 5): {"ok" if not bad else f"{bad} BAD"}')
    return not bad


def export(t):
    """agents (in order made) and wiring of the code for t, its principal port as the out end"""
    n = KNet()
    p = n.emit(t, {})
    ids = sorted(n.kind)
    ix = {a: j for j, a in enumerate(ids)}
    kinds = ['eraI' if n.kind[a] == 'era' else n.kind[a] for a in ids]
    def end(q): return f'.port {ix[q[0]]} {q[1]}'
    wires, seen = [f'(.out, {end(p)})'], set()
    for q, r in n.mate.items():
        if (r, q) in seen: continue
        seen.add((q, r))
        wires.append(f'({end(q)}, {end(r)})')
    return kinds, wires


# λq. T q: the lam's binder feeds the thaw, whose output is the body.
THAW = (['lam', 'T'], ['(.out, .port 0 0)', '(.port 0 1, .port 1 0)', '(.port 1 1, .port 0 2)'])


def lean():
    ports, rules, stuck = ktable.table()
    out = ['import Statement', '',
           '/-! The quote-and-table net and its kit as data, written by `../ideal/kexport.py`; checked and',
           'priced in `RecordQuote.lean`. -/', '', 'namespace QuoteRec', 'open Statement', '',
           '/-- The 14 kinds: code (lam app var rho tab), running (O apply, A open-apply, T thaw, C a table',
           'reading), resources (sig sharer, dupI/dupO copiers, eraI/eraO erasers). -/',
           'inductive Q', '  | ' + ' | '.join(ports), '  deriving DecidableEq, Repr', '',
           'def Q.all : List Q := [' + ', '.join('.' + k for k in ports) + ']', '',
           '/-- Port directions, principal first. -/', 'def Q.ports : Q → List Pol']
    out += [f'  | .{k} => [' + ', '.join('.' + c for c in p) + ']' for k, p in ports.items()]
    out += ['', 'def qsig : Sig Q := ⟨Q.ports⟩', '']
    def end(e): return f'.left {e[1]}' if e[0] == 'L' else f'.right {e[1]}' if e[0] == 'R' else f'.new {e[1]} {e[2]}'
    rows = [f'  ⟨.{a}, .{b}, [' + ', '.join('.' + m for m in made) + '], [' +
            ', '.join(f'({end(x)}, {end(y)})' for x, y in wires) + ']⟩' for a, b, made, wires in rules]
    out += [f'/-- The {len(rules)} rules (`ktable.py`). -/', 'def qtable : List (GRule Q) := [', ',\n'.join(rows) + ']', '']
    out += ['def qstuck : List (Q × Q) := [' + ', '.join(f'(.{a}, .{b})' for a, b in stuck) + ']', '']
    progs = [('reify', *export(prog('reify')), 'compiled by `knet.py`'),
             ('run', *THAW, 'the thaw, λq. T q, written out'),
             ('interp', *export(prog('interp')), 'compiled by `knet.py`; v0.1\'s interpreter, a `run` to compare'),
             ('reifyK', *export(prog('reifyK')), 'compiled by `knet.py`; the continuation-passing `reify`, to compare')]
    for name, kinds, wires, how in progs:
        out += [f'/-- `{name}` of `kexport.py`, {how}: {len(kinds)} agents. -/',
                f'def {name}Prog : Prog Q :=', '  { agents := [' + ', '.join('.' + k for k in kinds) + '],',
                '    wires := [' + ', '.join(wires) + '] }', '']
    out += ['end QuoteRec', '']
    return '\n'.join(out)


if __name__ == '__main__':
    if '--check' in sys.argv:
        check(int(sys.argv[sys.argv.index('--check') + 1]))
    else:
        assert check(500)
        path = os.path.join(HERE, '..', 'lean', 'QuoteKit.lean')
        open(path, 'w').write(lean())
        print('wrote', os.path.normpath(path))
