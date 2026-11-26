"""The candidate net's rule table as data, polarized the way ../lean/Tables.lean is: every port has a direction
(i: a value comes in, o: a value goes out), copiers and erasers come in two orientations.

`table()` gives (ports, rules, stuck): ports maps a kind to its port directions (principal first); a rule is
(a, b, made, wires) with ends ['L', i] (auxiliary port i of a), ['R', j] (of b), ['N', k, p] (port p of the
k-th made agent), the form ../ideal/score.py reads. `python ktable.py` prints the table; `python ktable.py
--lean` writes ../lean/QuoteNet.lean, where the same table is checked by `decide`.
"""
import sys, os

PORTS = {
    'lam': 'ooi', 'app': 'oii', 'var': 'oi', 'rho': 'o', 'tab': 'oiiiii',
    'O': 'iio', 'A': 'iio', 'T': 'io', 'C': 'iiiiiio',
    'sig': 'ioo', 'dupI': 'ioo', 'dupO': 'oii', 'eraI': 'i', 'eraO': 'o',
}
CODE = ['lam', 'app', 'var', 'rho', 'tab']
NSLOT = 5
COPIERS, ERASERS = ('sig', 'dupI', 'dupO'), ('eraI', 'eraO')

def L(i): return ['L', i]
def R(j): return ['R', j]
def N(k, p): return ['N', k, p]
def aux(k): return len(PORTS[k]) - 1


def copy_rule(c, x):
    """c (sig, dupI or dupO) meets x: two copies of x, a copier on each auxiliary port of x, facing the way
    that port faces"""
    made = [x, x]
    wires = [[N(0, 0), L(1)], [N(1, 0), L(2)]]
    for j in range(1, aux(x) + 1):
        made.append('dupI' if PORTS[x][j] == 'i' else 'dupO')
        d = len(made) - 1
        wires += [[N(d, 0), R(j)], [N(d, 1), N(0, j)], [N(d, 2), N(1, j)]]
    return (c, x, made, wires)


def erase_rule(e, x):
    made, wires = [], []
    for j in range(1, aux(x) + 1):
        made.append('eraI' if PORTS[x][j] == 'i' else 'eraO')
        wires.append([N(len(made) - 1, 0), R(j)])
    return (e, x, made, wires)


def read_rule(x):
    """C ⋈ x: a copier on the slot for x's kind; one copy back into a rebuilt table; the result is code, the
    other copy applied to that table and then to x's parts, every one of them under a quote"""
    i = CODE.index(x)
    made = ['dupI', 'tab']
    wires = [[N(0, 0), L(i + 1)], [N(0, 1), N(1, i + 1)]]
    wires += [[N(1, j + 1), L(j + 1)] for j in range(NSLOT) if j != i]
    if x == 'lam':
        made.append('lam')
        wires += [[N(2, 1), R(1)], [N(2, 2), R(2)]]
        parts = [N(2, 0)]
    else:
        parts = [R(j) for j in range(1, aux(x) + 1)]
    def quoted(p):
        made.append('var')
        wires.append([N(len(made) - 1, 1), p])
        return N(len(made) - 1, 0)
    cur = quoted(N(0, 2))
    for p in [N(1, 0)] + parts:
        q = quoted(p)
        made.append('app')
        a = len(made) - 1
        wires += [[N(a, 1), cur], [N(a, 2), q]]
        cur = N(a, 0)
    wires.append([cur, L(NSLOT + 1)])
    return ('C', x, made, wires)


def table():
    rules = [
        ('O', 'lam', ['T'], [[L(1), R(1)], [N(0, 0), R(2)], [N(0, 1), L(2)]]),
        ('O', 'var', ['A'], [[N(0, 0), R(1)], [N(0, 1), L(1)], [N(0, 2), L(2)]]),
        ('A', 'lam', [], [[L(1), R(1)], [L(2), R(2)]]),
        ('O', 'tab', ['C', 'T'], [[N(0, 0), L(1)]] + [[N(0, j), R(j)] for j in range(1, NSLOT + 1)] + [[N(0, NSLOT + 1), N(1, 0)], [N(1, 1), L(2)]]),
        ('A', 'tab', ['C'], [[N(0, 0), L(1)]] + [[N(0, j), R(j)] for j in range(1, NSLOT + 1)] + [[N(0, NSLOT + 1), L(2)]]),
    ]
    rules += [read_rule(x) for x in CODE]
    rules += [
        ('T', 'app', ['O', 'T', 'T'], [[N(1, 0), R(1)], [N(1, 1), N(0, 0)], [N(2, 0), R(2)], [N(2, 1), N(0, 1)], [N(0, 2), L(1)]]),
        ('T', 'var', [], [[L(1), R(1)]]),
        ('T', 'lam', ['lam'], [[N(0, 0), L(1)], [N(0, 1), R(1)], [N(0, 2), R(2)]]),
        ('T', 'rho', ['rho'], [[N(0, 0), L(1)]]),
        ('T', 'tab', ['tab'] + ['T'] * NSLOT, [[N(0, 0), L(1)]] + [w for j in range(1, NSLOT + 1) for w in ([N(j, 0), R(j)], [N(j, 1), N(0, j)])]),
    ]
    rules += [copy_rule(c, x) for c in ('sig', 'dupI') for x in CODE]
    rules += [copy_rule('dupO', 'sig'), ('dupI', 'dupO', [], [[L(1), R(1)], [L(2), R(2)]])]
    rules += [erase_rule('eraI', x) for x in CODE + ['dupO']]
    rules += [erase_rule('eraO', 'sig'), ('eraO', 'eraI', [], [])]
    have = {frozenset((r[0], r[1])) for r in rules}
    ins = [k for k in PORTS if PORTS[k][0] == 'i']
    outs = [k for k in PORTS if PORTS[k][0] == 'o']
    stuck = [(a, b) for a in ins for b in outs if frozenset((a, b)) not in have]
    return PORTS, rules, stuck


def check(ports, rules):
    """every port of the pair and of the made agents is wired exactly once, and every wire joins an out end
    to an in end (an auxiliary port of the pair counts from outside, so its direction flips)"""
    for a, b, made, wires in rules:
        want = {('L', i) for i in range(1, aux(a) + 1)} | {('R', j) for j in range(1, aux(b) + 1)}
        want |= {('N', k, p) for k, m in enumerate(made) for p in range(len(ports[m]))}
        seen = [tuple(e) for w in wires for e in w]
        assert sorted(seen) == sorted(want), (a, b, sorted(set(want) ^ set(seen)))
        def pol(e):
            if e[0] == 'N': return ports[made[e[1]]][e[2]]
            d = ports[a if e[0] == 'L' else b][e[1]]
            return 'o' if d == 'i' else 'i'
        for x, y in wires:
            assert pol(x) != pol(y), (a, b, x, y)
        assert ports[a][0] != ports[b][0], (a, b)
    return True


LEAN_HEAD = """import Search

/-! The candidate net ("quote-and-table net") as a rule table, written by `../ideal/ktable.py --lean`.
Not in the default build: check with `lake env lean QuoteNet.lean`. The table is checked by `decide`
against the same predicates as the older table (`Search.lean`, `Ideal.lean`): well formed, polarized, one
rule per pair, opposite principal ports (`TableOk`), no end wired twice (`RunOk`), every pair that can meet
has a rule or is listed stuck (`Total`), closed over one fixed list of kinds. So it gets the general
theorems of `Net.lean`, `Runs.lean` and `Moves.lean`: steps keep nets polarized, runs are strongly
confluent, and two finished runs end in the same net.
`Honest` is stated twice: with tables counted as copiers (a table is where a program asks for one more copy
of a slot) it holds; with the three plain copiers only it fails, on exactly the five reading rules. -/

/-! ## The constraints and the score, copied from `Ideal.lean` (which is not a built module, so it cannot
be imported) -/

@@IDEAL@@
/-! ## The table -/

inductive Q
  | lam | app | var | rho | tab | O | A | T | C | sig | dupI | dupO | eraI | eraO
  deriving DecidableEq, Repr

namespace Q
open Pol
def all : List Q := [lam, app, var, rho, tab, O, A, T, C, sig, dupI, dupO, eraI, eraO]
def ports : Q → List Pol
"""

LEAN_TAIL = """def copierQ : Q → Bool | .sig | .dupI | .dupO => true | _ => false
def tableOrCopierQ : Q → Bool | .sig | .dupI | .dupO | .tab | .C => true | _ => false
def eraserQ : Q → Bool | .eraI | .eraO => true | _ => false

theorem q_rules_polarized : ∀ r ∈ qtable, r.Polarized qsig := by
  show ∀ r ∈ qtable, ∀ w ∈ r.wires, r.wireOk qsig w = true; decide
theorem q_rules_madeOk : ∀ r ∈ qtable, r.MadeOk := by
  show ∀ r ∈ qtable, ∀ w ∈ r.wires, r.EndOk w.1 ∧ r.EndOk w.2; decide
theorem q_deterministic : Deterministic qtable := by
  show ∀ r ∈ qtable, ∀ r' ∈ qtable, ((r.a = r'.a ∧ r.b = r'.b) ∨ (r.a = r'.b ∧ r.b = r'.a)) → r = r'
  decide
theorem q_principals : PrincipalsOpposite qsig qtable := by
  show ∀ r ∈ qtable, qsig.pol r.a 0 ≠ qsig.pol r.b 0; decide
theorem q_tableOk : TableOk qsig qtable := ⟨q_rules_polarized, q_rules_madeOk, q_deterministic, q_principals⟩
theorem q_nodup : ∀ r ∈ qtable, (ends r.wires).Nodup := by decide
theorem q_runOk : RunOk qsig qtable := ⟨q_tableOk, q_nodup⟩

/-- Runs of the candidate are strongly confluent. -/
theorem q_strong_confluence {N N1 N2 : Net Q} (hinv : N.Involutive) (hfr : N.Fresh)
    (h1 : Step qsig qtable N N1) (h2 : Step qsig qtable N N2) :
    N1 = N2 ∨ ∃ M, Step qsig qtable N1 M ∧ Step qsig qtable N2 M :=
  q_tableOk.confluent hinv hfr h1 h2

/-- Two finished runs of a candidate program end in the same net, firing pairs and removing forwarders in
any order. -/
theorem q_finished_unique {keep : Ident → Prop} {N N1 N2 : Net Q} (hg : N.Good qsig)
    (h1 : Moves qsig qtable keep N N1) (h2 : Moves qsig qtable keep N N2)
    (f1 : Finished qsig qtable keep N1) (f2 : Finished qsig qtable keep N2) : N1 = N2 :=
  q_runOk.finished_unique hg h1 h2 f1 f2

theorem q_total : Total Q.all qsig qtable qstuck := by unfold Total; decide
theorem q_closed : ∀ r ∈ qtable, r.a ∈ Q.all ∧ r.b ∈ Q.all ∧ ∀ m ∈ r.made, m ∈ Q.all := by decide
/-- Only erasers erase; only copiers and tables (a table, and a table reading) copy. -/
theorem q_honest : Honest tableOrCopierQ eraserQ qtable := by unfold Honest; decide
theorem q_admissible : Admissible Q.all qsig qtable qstuck tableOrCopierQ eraserQ :=
  ⟨q_tableOk, q_total, q_honest, q_closed⟩
/-- With the reader not counted as a copier, the rules that break honesty are the five reading rules. -/
theorem q_reads_copy :
    (qtable.filter fun r => r.made.any copierQ && !(copierQ r.a || copierQ r.b)).map (fun r => (r.a, r.b))
      = [(.C, .lam), (.C, .app), (.C, .var), (.C, .rho), (.C, .tab)] := by decide
/-- No rule but an erase rule makes an eraser. -/
theorem q_only_erasing_drops : ∀ r ∈ qtable, r.made.any eraserQ → eraserQ r.a ∨ eraserQ r.b := by decide
/-- Code is inert: every code agent's principal port faces out, toward its user. -/
theorem q_code_faces_up : ∀ k ∈ [Q.lam, .app, .var, .rho, .tab], qsig.pol k 0 = .o := by decide
/-- Every kind of code has a reading rule, and the five take five different slots: the slot the copier
is put on is read off each rule's wiring. -/
theorem q_reads_tell_kinds_apart :
    (qtable.filter fun r => r.a == .C).map (fun r => (r.b, r.other (.new 0 0)))
      = [(.lam, some (.left 1)), (.app, some (.left 2)), (.var, some (.left 3)), (.rho, some (.left 4)),
         (.tab, some (.left 5))] := by decide

-- rules with a generic shape, all rules, stuck pairs, and the table's score in bits by `Ideal.lean`'s code
#eval ((qtable.filter fun r => r.isAnn qsig || r.isCom qsig || r.isPass qsig).length, qtable.length,
  qstuck.length, score Q.all qsig qtable 0)
"""

def lean():
    ports, rules, stuck = table()
    ideal = open(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'lean', 'Ideal.lean')).read()
    defs = ideal[ideal.index('open End\n\nvariable {κ : Type} [DecidableEq κ]'):ideal.index('/-- Ideal for a specification')].rstrip() + '\n'
    out = [LEAN_HEAD.replace('@@IDEAL@@', defs).rstrip('\n')]
    for k, p in ports.items(): out.append(f'  | .{k} => [' + ', '.join(p) + ']')
    out.append('end Q\n')
    out.append('def qsig : Sig Q := ⟨Q.ports⟩\n')
    def end(e):
        return f'.left {e[1]}' if e[0] == 'L' else f'.right {e[1]}' if e[0] == 'R' else f'.new {e[1]} {e[2]}'
    rows = []
    for a, b, made, wires in rules:
        ms = '[' + ', '.join('.' + m for m in made) + ']'
        ws = '[' + ', '.join(f'({end(x)}, {end(y)})' for x, y in wires) + ']'
        rows.append(f'  ⟨.{a}, .{b}, {ms}, {ws}⟩')
    out.append('def qtable : List (GRule Q) := [\n' + ',\n'.join(rows) + ']\n')
    out.append('def qstuck : List (Q × Q) := [' + ', '.join(f'(.{a}, .{b})' for a, b in stuck) + ']\n')
    out.append(LEAN_TAIL)
    return '\n'.join(out)


if __name__ == '__main__':
    ports, rules, stuck = table()
    check(ports, rules)
    if '--lean' in sys.argv:
        path = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'lean', 'QuoteNet.lean')
        open(path, 'w').write(lean())
        print('wrote', os.path.normpath(path))
    else:
        print(f'{len(ports)} kinds, {len(rules)} rules, {len(stuck)} stuck pairs; every rule well formed and polarized')
        for a, b, made, wires in rules: print(f'  {a} ⋈ {b}: makes {made}')
        print('  stuck:', ', '.join(f'{a}⋈{b}' for a, b in stuck))
