"""random programs on random trees, beyond the ladder. python ballistic/fuzz.py <file> <n> <seed>"""
import sys, random, importlib.util as u
sys.path.insert(0,'.');sys.path.insert(0,'ballistic')
import calculus as C, laws as L, physics as P
s=u.spec_from_file_location('design','ballistic/%s.py'%sys.argv[1]);D=u.module_from_spec(s);s.loader.exec_module(D)
rule,reader=P.Rule(D.REACTIONS,[0]*len(D.RANGES)),L.Reader(D)
rnd=random.Random(int(sys.argv[3]))
def prog(k):
    if k==0: return rnd.choice([C.L,C.SWAP,C.CROSS,C.SINK,C.STEM,C.UNIT])
    c=rnd.random()
    if c<.25: return prog(0)
    if c<.35: return C.dag(prog(k-1))
    if c<.6: return C.seq(prog(k-1),prog(k-1))
    if c<.85: return C.map_(prog(k-1),prog(k-1),prog(k-1))
    return C.tri(prog(k-1),prog(k-1),prog(k-1))
def tree(n):
    if n<=1: return C.L
    if rnd.random()<.3: return C.S(tree(n-1))
    a=rnd.randint(1,max(1,n-2)); return C.F(tree(a),tree(max(1,n-1-a)))
stats={};ex={}
for i in range(int(sys.argv[2])):
    p,x=prog(rnd.randint(0,3)),tree(rnd.randint(1,8))
    try: want=C.run(p,x)
    except C.TooDeep: continue
    r=L.run(D,p,x,rule,reader,300000)
    tag=('calc-ok' if want is not None else 'calc-stuck')
    if not r.still: res='never still'
    elif not r.reads or not r.moves: res='bad reading/moves'
    elif want is None: res='refused' if r.x_end==x and r.p_end==p and r.out_sig and r.outs[-1][3][5]==D._m['STUCK'] else 'wrong(stuck run gave answer)'
    elif r.x_end==want and r.p_end==p and r.outs and r.outs[-1][3][5]==D._m['OUT']: res='right'
    elif r.x_end==x and r.outs and r.outs[-1][3][5]==D._m['STUCK']: res='refused though calculus answers'
    else: res='WRONG'
    k=(tag,res); stats[k]=stats.get(k,0)+1
    if res not in('right','refused') and k not in ex: ex[k]=(C.show(p)[:70],C.show(x)[:30])
for k,v in sorted(stats.items()): print(k,v,ex.get(k,''))
