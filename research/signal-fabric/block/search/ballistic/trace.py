
import sys
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
import design as D, physics as P, calculus as C, laws as L
inv={v:k for k,v in D._m.items()}
def trace(p,x,n=3000,every=False):
    cells,rp,ep,rx,ex,port=L.start(D,p,x)
    w=P.World(cells,P.Rule(D.REACTIONS,[0]*len(D.RANGES)))
    last=None
    for i in range(n):
        h=[(s,c) for s,c in w.cells.items() if c[0] in (1,2,5,6,7) or (c[0] in (3,4) and c[5])]
        if not h: print('no head',w.t); break
        s,c=h[0]
        key=(c[0],c[1:5],c[5:])
        lab=(s,'HV NMKPQ'[c[0]-1],inv.get(c[5]),'v',c[6],'d',c[7],'b',c[8],'s',c[9],'kfrro',c[1:5])
        if key!=last: print(w.t,lab)
        last=key
        if len(h)>1: print('TWO',h)
        w.step()
        if w.still(): print('still',w.t,w.outs); break
    print(C.show(L.Reader(D).read(w.cells,rx,ex) or 'L'))
if __name__=='__main__':
    trace(eval(sys.argv[1],vars(C)),eval(sys.argv[2],vars(C)),int(sys.argv[3]) if len(sys.argv)>3 else 3000)
