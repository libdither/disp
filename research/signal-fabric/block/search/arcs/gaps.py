"""what happens on the cross cases the design doesn't build: left fork swapped, top fork swapped, arcs already crossed"""
import sys, os
sys.path.insert(0, os.path.abspath('.')); sys.path.insert(0, os.path.abspath('arcs'))
import laws as L, calculus as C, design as D
for name, p, x in [('cross;cross', C.seq(C.CROSS, C.CROSS), C.MARKED), ('first(swap);cross', C.seq(C.first(C.SWAP), C.CROSS), C.MARKED), ('swap;cross on left fork', C.seq(C.SWAP, C.CROSS), C.F(C.L, C.F(C.L, C.L)))]:
    r = L.run(D, p, x, limit=200000)
    print('%-26s still %s reads %s moves %s end %s want %s out %s' % (name, r.still, r.reads, r.moves, r.x_end and C.show(r.x_end), C.show(C.run(p, x)), [c[11] for c in r.out_sig]))
