"""specific combinations the ladder does not test: what the design does with each"""
import sys, importlib.util as u
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
import calculus as C, laws as L, physics as P
s=u.spec_from_file_location('design','ballistic/design.py');D=u.module_from_spec(s);s.loader.exec_module(D)
rule,reader=P.Rule(D.REACTIONS,[0]*len(D.RANGES)),L.Reader(D)
F,S,LL=C.F,C.S,C.L
X=F(F(LL,LL),LL); XB=F(F(F(LL,LL),F(LL,LL)),F(LL,LL))
cases=[('cross then stem on top',C.seq(C.CROSS,C.STEM),X),('cross then unit',C.seq(C.CROSS,C.UNIT),X),('cross then first(swap)',C.seq(C.CROSS,C.first(C.SWAP)),X),
('cross then second(stem)',C.seq(C.CROSS,C.second(C.STEM)),X),('cross cross',C.seq(C.CROSS,C.CROSS),XB),('cross under a stem then stem',C.seq(C.CROSS,C.STEM,C.dag(C.STEM)),XB),
('swap then cross on real forks',C.seq(C.SWAP,C.CROSS),XB),('first(swap) then cross',C.seq(C.first(C.SWAP),C.CROSS),XB),('sink on swapped fork',C.seq(C.SWAP,C.SINK),F(LL,S(LL))),
('stem then sink on fork, swapped inside',C.seq(C.STEM,C.first(C.SWAP) ,C.SINK),F(F(LL,LL),LL)) ,
('unit undo on swapped F(x,L)',C.seq(C.SWAP,C.dag(C.UNIT)),F(F(LL,LL),LL)),('unit undo on laid F(L,x)',C.dag(C.UNIT),F(LL,F(LL,LL))),
('tri c=swap, left-left fork',C.tri(C.L,C.L,C.SWAP),XB),('tri c=first(swap)',C.tri(C.L,C.L,C.first(C.SWAP)),XB),('tri c=second(stem)',C.tri(C.L,C.L,C.second(C.STEM)),XB),('tri c=second(second(swap))',C.tri(C.L,C.L,C.second(C.second(C.SWAP))),F(F(LL,LL),F(LL,LL))),
('nested tri c',C.tri(C.L,C.L,C.tri(C.L,C.L,C.second(C.SWAP))),XB),
('fix with SELF at same data',C.fix(C.tri(C.L,C.L,C.L)),X),('nested fix',C.fix(C.map_(C.fix(C.map_(C.SELF,C.SELF,C.SWAP)),C.SELF,C.SELF)),F(F(LL,LL),F(LL,LL))),
('rep(unit) then deep first',C.seq(C.UNIT,C.UNIT,C.first(C.UNIT),C.first(C.first(C.STEM))),LL),('first(first(unit)) chain',C.rep(C.first(C.first(C.UNIT)),3),F(F(LL,LL),LL)),]
for name,p,x in cases:
    want=C.run(p,x); r=L.run(D,p,x,rule,reader,300000)
    out=r.outs[-1][3][5] if r.outs else 0
    res='right' if (want is not None and r.x_end==want and out==D._m['OUT']) else 'refused' if out==D._m.get('STUCK') else 'OTHER %s'%(r.x_end and C.show(r.x_end))
    print('%-42s %s (calculus %s)'%(name,res,'answers' if want is not None else 'stuck'))
