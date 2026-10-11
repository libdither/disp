import numpy as np, math, collections
from scipy.optimize import least_squares
exec(open(__import__("os").path.join(__import__("os").path.dirname(__import__("os").path.abspath(__file__)), "load.py")).read())
X = [d for d in data if d["adder"] not in ("added", "saved")]
def arr(q, S): return np.array([d[q] for d in S])
def fit(name, model, p0, S):
    T = arr("T", S)
    res = least_squares(lambda p: np.log(np.maximum(model(p, S), 1e-9)) - np.log(T), p0)
    r = np.log(T) - np.log(model(res.x, S)); rms = np.sqrt(np.mean(r**2))
    print(f"  {name:42s} rms={rms:.3f} (x{math.exp(rms):.2f}) max x{math.exp(np.max(np.abs(r))):.2f} params={np.round(res.x,3).tolist()}")
    return res.x, r
D=lambda S: arr("D",S); Wi=lambda S: arr("Wi",S); A=lambda S: arr("A",S); N0=lambda S: arr("N0",S)
P=lambda S: Wi(S)/D(S)
for label, S in [("trees", [d for d in X if d["shape"]=="tree"]), ("lists", [d for d in X if d["shape"]=="list"]), ("unary", [d for d in X if d["shape"]=="unary"]), ("all", X)]:
    print(f"== {label} n={len(S)}")
    fit("T = a D", lambda p,S: abs(p[0])*D(S), [30], S)
    fit("T = D (a + b P)            [Brent]", lambda p,S: D(S)*(abs(p[0])+abs(p[1])*P(S)), [20,.7], S)
    fit("T = D (a + b sqrt A)", lambda p,S: D(S)*(abs(p[0])+abs(p[1])*np.sqrt(A(S))), [20,.3], S)
    fit("T = D (a + b sqrt A0)  A0=agents at load", lambda p,S: D(S)*(abs(p[0])+abs(p[1])*np.sqrt(N0(S))), [20,.3], S)
    fit("T = D (a + b P + c sqrt A)", lambda p,S: D(S)*(abs(p[0])+abs(p[1])*P(S)+abs(p[2])*np.sqrt(A(S))), [20,.5,.1], S)
    fit("T = D (a + b sqrt(P A))", lambda p,S: D(S)*(abs(p[0])+abs(p[1])*np.sqrt(P(S)*A(S))), [20,.05], S)
    fit("T = a D + b sqrt A + c Wi", lambda p,S: abs(p[0])*D(S)+abs(p[1])*np.sqrt(A(S))+abs(p[2])*Wi(S), [20,1,.5], S)
    fit("log c = k + x log P + y log A", lambda p,S: D(S)*math.exp(p[0])*P(S)**p[1]*A(S)**p[2], [3,.3,.1], S)
