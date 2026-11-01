import sys, trace as T, calculus as C, physics as P, laws as Lw, design as D
p = eval(sys.argv[1], vars(C)); x = eval(sys.argv[2], vars(C)); lim = int(sys.argv[3]) if len(sys.argv) > 3 else 60000
rule = P.Rule(D.REACTIONS, [0]*len(D.RANGES)); cells,*_=Lw.start(D,p,x); w=P.World(cells,rule); last=None; seq=[]
while not w.still() and w.t<lim:
    w.step(); s,st,c=T.head(w)
    if st!=last:
        last=st; seq.append((w.t,st,s,(c[8],c[9]) if c else None,T.show(w,0),T.show(w,1)))
print('t',w.t,'still',w.still(),'changes',len(seq))
# the turning point of a retrace: the latest state-change whose neighbours mirror it for the longest run
best=(0,0)
for i in range(1,len(seq)-1):
    n=0
    while i-n-1>=0 and i+n+1<len(seq) and seq[i-n-1][1:3]==seq[i+n+1][1:3]: n+=1
    if n>best[0]: best=(n,i)
n,i=best
print('longest mirror',n,'at',i)
for r in seq[max(0,i-6):i+2]: print(r)
print('...')
for r in seq[-14:]: print(r)
print('=== center')
for r in seq:
    if lim//2-120 <= r[0] <= lim//2+40: print(r)
