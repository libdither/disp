import sys,time,collections,os; sys.path[:0]=['.']
import calculus as C, laws as Lw, physics as P
import importlib.util as u
sys.path.insert(0,os.path.dirname(os.path.abspath(sys.argv[1])));s=u.spec_from_file_location('design',sys.argv[1]); D=u.module_from_spec(s); s.loader.exec_module(D)
rule,rd=P.Rule(D.REACTIONS,[0]*len(D.RANGES)),Lw.Reader(D)
ps,xs=C.rung_general(); cnt=collections.Counter(); ex={}; t0=time.time(); seen=set(); cells=set(); fine=None
for i,p in enumerate(ps):
    for x in xs:
        try: w=C.run(p,x)
        except C.TooDeep: cnt['deep']+=1; continue
        if w is None: cnt['calculus stuck']+=1; continue
        r=Lw.run(D,p,x,rule,rd,limit=300000)
        k='ok' if r.still and r.x_end==w and r.p_end==p and r.moves and r.reads and r.sealed else ('false refuse' if r.still and r.x_end==x and x!=w else ('same-answer-refuse?' if r.still and r.x_end==x else 'WRONG/other'))
        cnt[k]+=1
        if k!='ok' and len(ex.setdefault(k,[]))<6: ex[k].append((C.show(p),C.show(x),C.show(w)))
        seen.add(r.out_sig)
print(cnt, 'programs',len(ps),'trees',len(xs),'%.0fs'%(time.time()-t0),'outs',len(seen))
for k,v in ex.items(): print(k,v[:6])
blocks=rule.cache
c=set(q for b in blocks for q in b)
print('distinct cells seen in general-rung runs',len(c),'reactions used',len(rule.used),'of',len(D.REACTIONS))
