"""Runs the counted piece through the harness's run(): @ on map(L,L,L) with F(L,L). Run: python chem/map_check.py from search/."""
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import calculus as C, laws as Lw, physics as P
import design as D
print(D.NAME, len(D.REACTIONS), 'reactions;', D.SHAPES)
rl, bad = Lw.rule_laws(D)
print('rule laws', rl, bad[:6])
mf, worst = Lw.matter_laws(D)
print('matter fails', len(mf), mf[:3], 'worst cells/node %.2f' % worst)
p, x = C.map_(C.L, C.L, C.L), C.F(C.L, C.L)
r = Lw.run(D, p, x, limit=5000)
print('first x', r.first_x and C.show(r.first_x), 'first p', r.first_p and C.show(r.first_p))
print('t', r.t, 'still', r.still, 'reads', r.reads, 'moves', r.moves, 'never_moves', r.never_moves, 'sealed', r.sealed)
print('x_end', r.x_end and C.show(r.x_end), 'p_end', r.p_end and C.show(r.p_end), 'want', C.show(C.run(p, x)), 'outs', r.outs)
used = P.Rule(D.REACTIONS, [0] * len(D.RANGES))
Lw.run(D, p, x, rule=used, limit=5000)
print('reactions used in the run: %d of %d' % (len(used.used), len(D.REACTIONS)))
print('unused:', [i for i in range(len(D.REACTIONS)) if i not in used.used])
