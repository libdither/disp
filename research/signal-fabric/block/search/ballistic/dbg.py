import sys
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
import design as D, physics as P, calculus as C
inv={v:k for k,v in D._m.items()}
print('nonlinear',[i for i,(l,r) in enumerate(D.REACTIONS) if not P.linear([(l,r)])][:20])
def sh(side):
    o=[]
    for c in side:
        o.append(''.join((str(f[1]) if f[0]=='I' else f[1])+',' for f in c))
    return o
bad=P.one_partner(D.REACTIONS)
print('bad',len(bad),[(a,b) for a,b in bad[:6]])
for a,b in bad[:4]:
    print(a,sh(D.REACTIONS[a//2][a%2])); print(b,sh(D.REACTIONS[b//2][b%2]))
for i in [i for i,(l,r) in enumerate(D.REACTIONS) if not P.linear([(l,r)])][:6]:
    print(i,sh(D.REACTIONS[i][0]),sh(D.REACTIONS[i][1]))
print(inv)

t=C.S(C.S(C.S(C.F(C.L,C.L))))
l=D.lay(t)
rule=P.Rule(D.REACTIONS,[0]*len(D.RANGES))
w=P.World(l.cells,rule)
for _ in range(4):
    ch=w.step()
    if ch: print('moved',w.t,ch,[w.cells.get(s) for s in ch])
for b,n in list(rule.cache.items()):
    if b!=n: print(b,n)

