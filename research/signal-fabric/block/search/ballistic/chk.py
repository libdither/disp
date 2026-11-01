"""python ballistic/chk.py <file> [upto] : overlaps, laws quick; prints failing rows"""
import sys, importlib.util as u
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
import physics as P, calculus as C, laws as L
s=u.spec_from_file_location('design','ballistic/%s.py'%sys.argv[1]);D=u.module_from_spec(s);s.loader.exec_module(D)
inv={v:k for k,v in D._m.items()}
def sh(side):
    return [''.join((str(f[1]) if f[0]=='I' else f[1])+',' for f in c) if any(f!=('I',0) for f in c) else '.' for c in side]
nl=[i for i,(l,r) in enumerate(D.REACTIONS) if not P.linear([(l,r)])]
bad=P.one_partner(D.REACTIONS)
print(len(D.REACTIONS),'rx; nonlinear',nl,'bad',len(bad),'modes',len(inv))
for a,b in bad[:5]:
    print(a,sh(D.REACTIONS[a//2][a%2]));print(b,sh(D.REACTIONS[b//2][b%2]))
for i in nl[:4]: print(i,sh(D.REACTIONS[i][0]),sh(D.REACTIONS[i][1]))
if not bad and not nl:
    L.report(D,want_graded=False,want_speed=False,upto=(sys.argv[2] if len(sys.argv)>2 else None))
