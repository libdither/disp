import sys, importlib.util as u
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
import calculus as C, laws as L, physics as P
s=u.spec_from_file_location('gp','ballistic/grow_partial.py');D=u.module_from_spec(s);s.loader.exec_module(D)
rule,reader=P.Rule(D.REACTIONS,[0]*len(D.RANGES)),L.Reader(D)
ps,xs=C.rung_grow()
ok=bad=0
for p in ps:
    f=0;n=0
    for x in xs:
        want=C.run(p,x)
        if want is None: continue
        n+=1
        r=L.run(D,p,x,rule,reader,30000)
        if not(r.still and r.x_end==want and r.p_end==p and r.reads and r.moves and r.sealed): f+=1
    ok+=n-f;bad+=f
    print('%-70s %d/%d'%(C.show(p)[:70],n-f,n))
print('total',ok,'of',ok+bad, 'reactions',len(D.REACTIONS),'bad pairs',len(P.one_partner(D.REACTIONS)))
