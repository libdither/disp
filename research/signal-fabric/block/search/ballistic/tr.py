"""python ballistic/tr.py <file> '<prog>' '<tree>' [lastN]: mode changes of the head in a run"""
import sys, importlib.util as u
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
s=u.spec_from_file_location('design','ballistic/%s.py'%sys.argv[1]);D=u.module_from_spec(s);s.loader.exec_module(D);sys.modules['design']=D
import calculus as C, laws as L, physics as P
inv={v:k for k,v in D._m.items()}
p,x=eval(sys.argv[2],vars(C)),eval(sys.argv[3],vars(C)); n=int(sys.argv[4]) if len(sys.argv)>4 else 25
cells,rp,ep,rx,ex,port=L.start(D,p,x)
w=P.World(cells,P.Rule(D.REACTIONS,[0]*len(D.RANGES)))
last=None;log=[]
rd=L.Reader(D)
for i in range(300000):
    h=[(a,c) for a,c in w.cells.items() if c[0] in (1,2,5,6,7) or (c[0] in (3,4) and c[5])]
    if not h: break
    a,c=h[0]; key=(c[5:],a)
    log.append((w.t,a,'.HVNMKPQ'[c[0]],inv.get(c[5]),'k',c[1],'f',c[2],'r',c[3],'ro',c[4],'v',c[6],'d',c[7],'b',c[8],'s',c[9],'e',c[10]))
    w.step()
    if len(log)>2*n+50: log=log[-n-40:]
print(*log[-n:],sep='\n')
print('t',w.t,'outs',[inv.get(o[3][5]) for o in w.outs],C.show(rd.read(w.cells,rx,ex) or 'L'))
xs=[a for a,_ in w.cells];ys=[b for _,b in w.cells]
for yy in range(min(ys),max(ys)+1):
    print('%3d '%yy+''.join(('LSF'[w.cells[(xx,yy)][1]] if w.cells[(xx,yy)][0]==3 else 'ehdcxqCs'[w.cells[(xx,yy)][1]] if w.cells[(xx,yy)][0]==4 else '*') if (xx,yy) in w.cells else '.' for xx in range(min(xs),max(xs)+1)))
