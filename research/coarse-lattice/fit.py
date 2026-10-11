import numpy as np, math, collections
from scipy.optimize import least_squares
exec(open(__import__("os").path.join(__import__("os").path.dirname(__import__("os").path.abspath(__file__)), "load.py")).read())
X = [d for d in data if d["adder"] not in ("added", "saved")]
def arr(q, S=X): return np.array([d[q] for d in S])
def fam(d): return "unary" if d["shape"] == "unary" else d["shape"]
def report(name, pred, S=X, k=None):
    T = arr("T", S); r = np.log(T) - np.log(pred)
    out = f"{name:44s} rms(log)={np.sqrt(np.mean(r**2)):.3f} (x{math.exp(np.sqrt(np.mean(r**2))):.2f})  max x{math.exp(np.max(np.abs(r))):.2f}"
    by = collections.defaultdict(list)
    for d, ri in zip(S, r): by[(fam(d), d["sets"])].append(ri)
    out += "  | " + " ".join(f"{a[0][:4]}/{a[1][:5]}:{np.mean(v):+.2f}" for a, v in sorted(by.items()))
    if k is not None: out += f"  params={np.round(k, 4).tolist()}"
    print(out)
    return r
def fit(name, model, p0, S=X):
    T = arr("T", S)
    res = least_squares(lambda p: np.log(np.maximum(model(p, S), 1e-9)) - np.log(T), p0)
    return report(name, model(res.x, S), S, res.x), res.x
D, A, W, Wi, N0, Sx = (lambda S: arr("D", S)), (lambda S: arr("A", S)), (lambda S: arr("W", S)), (lambda S: arr("Wi", S)), (lambda S: arr("N0", S)), (lambda S: arr("S", S))
for sets in ["plain", "views=1", None]:
    S = [d for d in X if sets is None or d["sets"] == sets]
    print(f"\n=== sets={sets or 'both'}  n={len(S)}")
    fit("T = k D", lambda p, S: p[0] * D(S), [30], S)
    fit("T = k Wi", lambda p, S: p[0] * Wi(S), [3], S)
    fit("T = k W(lattice)", lambda p, S: p[0] * W(S), [3], S)
    fit("T = a D + b Wi   (Brent)", lambda p, S: abs(p[0]) * D(S) + abs(p[1]) * Wi(S), [20, 1], S)
    fit("T = k D sqrt(A)", lambda p, S: p[0] * D(S) * np.sqrt(A(S)), [0.5], S)
    fit("T = D (a + b sqrt(A))", lambda p, S: D(S) * (abs(p[0]) + abs(p[1]) * np.sqrt(A(S))), [20, 0.2], S)
    fit("T = a D + b sqrt(A)", lambda p, S: abs(p[0]) * D(S) + abs(p[1]) * np.sqrt(A(S)), [20, 10], S)
    fit("T = a D + b Wi/sqrt(A)?", lambda p, S: abs(p[0]) * D(S) + abs(p[1]) * Wi(S) / np.sqrt(A(S)), [20, 10], S)
    fit("T = D (a + b sqrt(A)) + c Wi", lambda p, S: D(S) * (abs(p[0]) + abs(p[1]) * np.sqrt(A(S))) + abs(p[2]) * Wi(S), [20, .2, .5], S)
    fit("T = k D^a A^b (free)", lambda p, S: math.exp(p[0]) * D(S) ** p[1] * A(S) ** p[2], [3, 1, .3], S)
    fit("T = k D^a A^b Wi^c (free)", lambda p, S: math.exp(p[0]) * D(S) ** p[1] * A(S) ** p[2] * Wi(S) ** p[3], [3, 1, .3, 0], S)
    fit("T = k D^a W^c (free)", lambda p, S: math.exp(p[0]) * D(S) ** p[1] * Wi(S) ** p[2], [3, 1, 0], S)
