"""which reactions a full ladder run touches. python ballistic/used.py <file>"""
import sys, importlib.util as u
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
import calculus as C, laws as L, physics as P
s=u.spec_from_file_location('design','ballistic/%s.py'%sys.argv[1]);D=u.module_from_spec(s);s.loader.exec_module(D)
rule,reader=P.Rule(D.REACTIONS,[0]*len(D.RANGES)),L.Reader(D)
n=0
for name,f in C.RUNGS+[('stuck',C.rung_stuck)]:
    ps,xs=f()
    for p in ps:
        for x in xs:
            try:
                if C.run(p,x) is None and name!='stuck': continue
            except C.TooDeep: continue
            L.run(D,p,x,rule,reader,20000000 if name=='add' else 200000,watch=False); n+=1
    print(name,'used so far',len(rule.used),'of',len(D.REACTIONS),flush=True)
inv={v:k for k,v in D._m.items()}
un=[i for i in range(len(D.REACTIONS)) if i not in rule.used]
def lab(side):
    o=[]
    for c in side:
        if any(f!=('I',0) for f in c):
            m=c[5]; o.append(str(c[0][1] if c[0][0]=='I' else '?')+':'+str(c[1][1] if c[1][0]=='I' else '*')+':'+(inv.get(m[1],'%s'%m[1]) if m[0]=='I' else '*'))
        else: o.append('.')
    return ' '.join(o)
for i in un: print(i,lab(D.REACTIONS[i][0]),' <> ',lab(D.REACTIONS[i][1]))
