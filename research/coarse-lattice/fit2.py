import numpy as np, math, collections
from scipy.optimize import least_squares
exec(open(__import__("os").path.join(__import__("os").path.dirname(__import__("os").path.abspath(__file__)), "load.py")).read())
X = [d for d in data if d["adder"] not in ("added", "saved")]
def arr(q, S): return np.array([d[q] for d in S])
def fam(d): return "unary" if d["shape"] == "unary" else d["shape"]
def fit(name, model, p0, S, quiet=False):
    T = arr("T", S)
    res = least_squares(lambda p: np.log(np.maximum(model(p, S), 1e-9)) - np.log(T), p0)
    r = np.log(T) - np.log(model(res.x, S))
    rms = np.sqrt(np.mean(r**2))
    if not quiet: print(f"  {name:40s} rms={rms:.3f} (x{math.exp(rms):.2f}) max x{math.exp(np.max(np.abs(r))):.2f} params={np.round(res.x,3).tolist()}")
    return res.x, r
D=lambda S: arr("D",S); Wi=lambda S: arr("Wi",S); A=lambda S: arr("A",S); W=lambda S: arr("W",S)
P=lambda S: Wi(S)/D(S)
print("== round-cost models on all 304")
fit("T = D (a + b ln P)", lambda p,S: D(S)*(abs(p[0])+abs(p[1])*np.log(P(S))), [20,5], X)
fit("T = D (a + b P^g)", lambda p,S: D(S)*(abs(p[0])+abs(p[1])*P(S)**p[2]), [20,1,1], X)
fit("T = max(a D, b Wi)  (hard roofline)", lambda p,S: np.maximum(abs(p[0])*D(S), abs(p[1])*Wi(S)), [30,1], X)
fit("T = ((aD)^q + (bWi)^q)^(1/q)", lambda p,S: ((abs(p[0])*D(S))**p[2] + (abs(p[1])*Wi(S))**p[2])**(1/p[2]), [25,.8,1.5], X)
fit("T = a D + b Wi / A^d", lambda p,S: abs(p[0])*D(S)+abs(p[1])*Wi(S)/A(S)**p[2], [20,1,0.0], X)
fit("T = a D + b Wi * A^d", lambda p,S: abs(p[0])*D(S)+abs(p[1])*Wi(S)*A(S)**p[2], [20,1,0.0], X)
fit("T = a D + b Wi + c D sqrt(A)", lambda p,S: abs(p[0])*D(S)+abs(p[1])*Wi(S)+abs(p[2])*D(S)*np.sqrt(A(S)), [20,.7,.01], X)
fit("T = a D + b W(lattice)", lambda p,S: abs(p[0])*D(S)+abs(p[1])*W(S), [20,.7], X)
dn = lambda S: np.nan_to_num(arr("dn",S))
fit("T = a D + b1 W(1-dn) + b2 W dn", lambda p,S: abs(p[0])*D(S)+abs(p[1])*W(S)*(1-dn(S))+abs(p[2])*W(S)*dn(S), [20,.7,.7], X)
print("\n== Brent T = a D + b Wi per adder/shape (both sets)")
by = collections.defaultdict(list)
for d in X: by[(d["adder"], d["shape"])].append(d)
for k, S in sorted(by.items()):
    p, r = fit("", lambda p,S: abs(p[0])*D(S)+abs(p[1])*Wi(S), [20,.7], S, quiet=True)
    p2, r2 = fit("", lambda p,S: abs(p[0])*D(S), [20], S, quiet=True)
    Pm = np.median(P(S)); lam = np.median(W(S)/arr("T",S))
    print(f"  {k[0]:14s} {k[1]:6s} n={len(S):3d} a={abs(p[0]):6.1f} b={abs(p[1]):5.2f} rms={np.sqrt(np.mean(r**2)):.3f} | T=kD: k={p2[0]:5.1f} rms={np.sqrt(np.mean(r2**2)):.3f} | median P={Pm:6.1f} lattice rewrites/clock={lam:.2f}")
print("\n== throughput vs ideal parallelism (Little's law view), plain, inputs max")
S = sorted([d for d in X if d["sets"]=="plain" and d["input"] in ("max","b=1")], key=lambda d:(d["adder"],d["shape"],d["n"]))
for d in S:
    lam = d["W"]/d["T"]; Pb = d["Wi"]/d["D"]
    print(f"  {d['adder']:13s} {d['shape']:6s} n={d['n']:4d} D={d['D']:6.0f} Wi={d['Wi']:7.0f} P={Pb:6.1f} T/D={d['T']/d['D']:6.1f} sqrtA={math.sqrt(d['A']):6.1f} W/T={lam:.2f}  L=lam*23={lam*23:5.1f}")
