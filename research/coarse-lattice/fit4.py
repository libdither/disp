import numpy as np, math, collections
from scipy.optimize import least_squares
exec(open(__import__("os").path.join(__import__("os").path.dirname(__import__("os").path.abspath(__file__)), "load.py")).read())
X = [d for d in data if d["adder"] not in ("added", "saved")]
def arr(q, S): return np.array([d[q] for d in S])
D=lambda S: arr("D",S); Wi=lambda S: arr("Wi",S); A=lambda S: arr("A",S)
models = {
 "brent": (lambda p,S: abs(p[0])*D(S)+abs(p[1])*Wi(S), [20,.7]),
 "brent+area": (lambda p,S: abs(p[0])*D(S)+abs(p[1])*Wi(S)+abs(p[2])*D(S)*np.sqrt(A(S)), [20,.6,.1]),
 "D sqrtA": (lambda p,S: abs(p[0])*D(S)*np.sqrt(A(S)), [.6]),
 "D": (lambda p,S: abs(p[0])*D(S), [30]),
}
P = {}
for k,(m,p0) in models.items():
    T = arr("T", X)
    res = least_squares(lambda p: np.log(m(p, X)) - np.log(T), p0); P[k] = res.x
    r = np.log(T) - np.log(m(res.x, X))
    idx = np.argsort(-np.abs(r))[:8]
    print(f"== {k} params={np.round(np.abs(res.x),3).tolist()} worst:")
    for i in idx:
        d = X[i]; print(f"   {d['id']:28s} {d['sets']:7s} T={d['T']:8.0f} pred={m(res.x,[d])[0]:8.0f} ratio={math.exp(r[i]):.2f}")
# seed spread: how much of the residual is noise
sp = [math.log(d["Tmax"]/d["Tmin"])/2 for d in X if d["good"]>=2 and d["Tmin"]>0]
print(f"seed half-spread in log T: median {np.median(sp):.3f}, mean {np.mean(sp):.3f}")
# out-of-sample: disp and lambada programs (README movement-agents table plain, LINEAR_TREE_NETS §6 base rounds)
test = [  # name, D, Wi, T, W, A
 ("disp add 2 3", 415, 2389, 10500, 2149, 1323),
 ("disp fib 3", 756, 10185, 24100, 8980, 2738),
 ("disp mul 2 2", 691, 8530, 18600, 7630, 2272),
 ("disp rev [1, 2]", 608, 5803, 16800, 5236, 2690),
 ("disp isort [2, 1]", 1067, 10646, 33400, 9538, 5200),
 ("disp greet \"a\"", 668, 4534, 15900, 3978, 3163),
 ("lambada fib(2)", 300, 5473, 10400, 5246, 3841),
 ("lambada exp(1)", 375, 4599, 11900, 4343, 4076),
]
S = [dict(D=t[1], Wi=t[2], T=t[3], W=t[4], A=t[5]) for t in test]
print("\n== out of sample (fitted on adders, plain+views)")
for k,(m,_) in models.items():
    r = np.log(arr("T",S)) - np.log(m(P[k], S))
    print(f"  {k:11s} rms={np.sqrt(np.mean(r**2)):.3f} (x{math.exp(np.sqrt(np.mean(r**2))):.2f}) mean bias x{math.exp(np.mean(r)):.2f}  per program: " + ", ".join(f"{t[0].split()[-1] if False else t[0]}:{math.exp(x):.2f}" for t,x in zip(test,r)))
# sum-of-many (added/saved) as another out-of-sample family
Y = [d for d in data if d["adder"] in ("added","saved")]
print("\n== out of sample: sums of many numbers (added, saved)")
for k,(m,_) in models.items():
    r = np.log(arr("T",Y)) - np.log(m(P[k], Y))
    print(f"  {k:11s} rms={np.sqrt(np.mean(r**2)):.3f} bias x{math.exp(np.mean(r)):.2f}  " + ", ".join(f"{d['id']}:{math.exp(x):.2f}" for d,x in zip(Y,r)))
