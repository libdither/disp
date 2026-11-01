"""general rung detail: python ballistic/gen.py <file>; which programs get refused and how often"""
import sys,time,collections; sys.path[:0]=['.','ballistic']
import calculus as C, laws as Lw, physics as P
import importlib.util as u
s=u.spec_from_file_location('design','ballistic/%s.py'%sys.argv[1]); D=u.module_from_spec(s); s.loader.exec_module(D)
rule,rd=P.Rule(D.REACTIONS,[0]*len(D.RANGES)),Lw.Reader(D)
ps,xs=C.rung_general(); cnt=collections.Counter(); per=collections.Counter(); ex={}
for p in ps:
    for x in xs:
        try: w=C.run(p,x)
        except C.TooDeep: continue
        if w is None: continue
        r=Lw.run(D,p,x,rule,rd,limit=300000)
        k='ok' if r.still and r.x_end==w and r.p_end==p and r.moves and r.reads and r.sealed else ('refuse' if r.still and r.x_end==x else 'bad')
        cnt[k]+=1
        if k!='ok':
            per[C.show(p)]+=1; ex.setdefault(C.show(p),(k,C.show(x),C.show(w)))
print(cnt,'distinct refused programs',len(per))
for q,n in per.most_common(45): print(n,q[:100],ex[q])
cells=set(q for b in rule.cache for q in b)
print('cells',len(cells),'used',len(rule.used),'of',len(D.REACTIONS))
