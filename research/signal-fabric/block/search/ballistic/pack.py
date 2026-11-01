"""computes the two tables at the end of a design: which reactions no rung touches, and which token names can share a number.
python ballistic/pack.py <file>  (prints DROP and PACK; the design pastes them)"""
import sys, importlib.util as u
sys.path[:0]=['.','ballistic']
import calculus as C, laws as L, physics as P
s=u.spec_from_file_location('design','ballistic/%s.py'%sys.argv[1]);D=u.module_from_spec(s);s.loader.exec_module(D)
rule,reader=P.Rule(D.REACTIONS,[0]*len(D.RANGES)),L.Reader(D)
for name,f in C.RUNGS+[('stuck',C.rung_stuck)]:
    ps,xs=f()
    for p in ps:
        for x in xs:
            try:
                if C.run(p,x) is None and name!='stuck': continue
            except C.TooDeep: continue
            L.run(D,p,x,rule,reader,20000000 if name=='add' else 300000,watch=False)
drop=sorted(i for i in range(len(D.REACTIONS)) if i not in rule.used)
keep=[r for i,r in enumerate(D.REACTIONS) if i not in drop]
print('DROP =',drop)
M,V,RO=5,6,4
def uses(field,key=lambda c:(c[0][1] if c[0][0]=='I' else '*',c[1][1] if c[1][0]=='I' else '*')):
    out={}
    for l,r in keep:
        for c in l+r:
            if c[field][0]=='I': out.setdefault(c[field][1],set()).add(key(c))
    return out
def compat(a,b):
    return not any(x==y or '*' in (x[1],y[1]) and x[0]==y[0] or x[0]=='*' or y[0]=='*' for x in a for y in b)
def color(us,fixed=()):
    col={};cls=[]
    for f in fixed: col[f]=f; cls.append([f])
    for n in sorted(us,key=lambda n:-len(us[n])):
        if n in col: continue
        for c in cls:
            if all(compat(us[n],us.get(o,set())) for o in c):
                c.append(n); col[n]=c[0]; break
        else:
            cls.append([n]); col[n]=n
    ids={c[0]:i for i,c in enumerate(cls)}
    return {n:ids[col[n]] for n in col},len(cls)
um=uses(M); pm,nm=color(um,fixed=(0,))
# the modes field: a ball or cell keyed by class and kind
keyv=lambda c:(c[5][1] if c[5][0]=='I' else '*',c[0][1] if c[0][0]=='I' else '*')
uv=uses(V,keyv); pv,nv=color(uv,fixed=(0,))
uro=uses(RO); pr,nr=color(uro,fixed=(0,))
print('PM =',pm);print('PV =',pv);print('PR =',pr);print('NM, NV, NR =',nm,',',nv,',',nr)
