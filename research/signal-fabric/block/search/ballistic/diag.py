import sys, importlib.util as u
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
import calculus as C, laws as L, physics as P
s=u.spec_from_file_location('gp','ballistic/grow_partial.py');D=u.module_from_spec(s);s.loader.exec_module(D)
rule,reader=P.Rule(D.REACTIONS,[0]*len(D.RANGES)),L.Reader(D)
def grid(cells):
    xs=[a for a,_ in cells];ys=[b for _,b in cells]
    for y in range(min(ys),max(ys)+1):
        print('%3d '%y+''.join({3:'LSF'[cells[(x,y)][1]] if (x,y) in cells and cells[(x,y)][0]==3 else '?',4:'E'}.get(cells[(x,y)][0],'*') if (x,y) in cells else '.' for x in range(min(xs),max(xs)+1)))
F,S,LL=C.F,C.S,C.L
for name,p,x in (('seq(UNIT,first(STEM))',C.seq(C.UNIT,C.first(C.STEM)),F(F(LL,LL),F(LL,LL))),
                 ('seq(UNIT,second(UNIT),first(STEM),second(first(STEM)))',C.seq(C.UNIT,C.second(C.UNIT),C.first(C.STEM),C.second(C.first(C.STEM))),LL)):
    r=L.run(D,p,x,rule,reader,30000)
    print(name,'still',r.still,'t',r.t,'reads',r.reads,'moves',r.moves,'x_end',r.x_end and C.show(r.x_end),'want',C.show(C.run(p,x)))
    print('moving things:',[(k,v) for k,v in r.cells.items() if v[0] in (1,2,5,6,7) or (v[0] in (3,4) and v[5])][:3])
    grid(r.cells)
