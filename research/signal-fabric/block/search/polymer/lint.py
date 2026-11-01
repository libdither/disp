import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__))); sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import design as D, physics as P
names = {v: k for k, v in D.R.names.items()}
def show(c):
    return ' '.join('%s=%s' % (n, (v[1] if v[0]=='I' else '_')) for n, v in zip(D.FIELDS, c) if n in ('k','a','r','e','m','u','h','st') and v[0]=='I' and not (n in('h',) ))
for i,(l,r) in enumerate(D.REACTIONS):
    a=[f[1] for c in l for f in c if f[0]=='A']; b=[f[1] for c in r for f in c if f[0]=='A']
    if sorted(a)!=sorted(b) or len(set(a))!=len(a):
        print('nonlinear', i, set(a)^set(b), [show(c) for c in l], [show(c) for c in r])
print('reactions', len(D.REACTIONS), 'states', D.R.n, 'range check', D.R.n < D.RANGE['st'])
if '--pairs' in sys.argv:
    bad = P.one_partner(D.REACTIONS)
    def stn(c):
        v = c[7]
        return names.get(v[1], '?') if v[0]=='I' else '_'
    print('one_partner fails', len(bad))
    for i,j in bad[:12]:
        ri=D.REACTIONS[i//2]; rj=D.REACTIONS[j//2]
        print(i,j,[stn(c) for c in ri[i%2]],[show(c) for c in ri[i%2]],'|',[stn(c) for c in rj[j%2]],[show(c) for c in rj[j%2]])
