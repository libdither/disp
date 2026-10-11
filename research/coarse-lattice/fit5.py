import numpy as np, math, collections
from scipy.optimize import least_squares
exec(open(__import__("os").path.join(__import__("os").path.dirname(__import__("os").path.abspath(__file__)), "load.py")).read())
X = [d for d in data if d["adder"] not in ("added", "saved")]
def arr(q, S): return np.array([d[q] for d in S])
for label, S in [("all", X), ("trees", [d for d in X if d["shape"]=="tree"]), ("lists", [d for d in X if d["shape"]=="list"]), ("unary",[d for d in X if d["shape"]=="unary"])]:
    A = arr("A",S); N0 = arr("N0",S); W = arr("Wi",S); D = arr("D",S); P = W/D
    M = np.column_stack([np.ones(len(S)), np.log(N0), np.log(P)])
    c, *_ = np.linalg.lstsq(M, np.log(A), rcond=None)
    r = np.log(A) - M@c
    M2 = np.column_stack([np.ones(len(S)), np.log(N0)])
    c2, *_ = np.linalg.lstsq(M2, np.log(A), rcond=None); r2 = np.log(A) - M2@c2
    # additive: A = u*N0 + v*P
    res = least_squares(lambda p: np.log(abs(p[0])*N0 + abs(p[1])*P) - np.log(A), [3, 50])
    r3 = np.log(A) - np.log(abs(res.x[0])*N0 + abs(res.x[1])*P)
    print(f"{label:6s} n={len(S)}: log A = {c[0]:.2f} + {c[1]:.2f} log N0 + {c[2]:.2f} log P  rms={np.sqrt(np.mean(r**2)):.3f} | A ~ N0^{c2[1]:.2f} rms={np.sqrt(np.mean(r2**2)):.3f} | A = {abs(res.x[0]):.2f} N0 + {abs(res.x[1]):.1f} P rms={np.sqrt(np.mean(r3**2)):.3f}")
# A*T and A*T^2 by adder at 16 bits, max input
print()
for d in sorted([d for d in X if d["n"]==16 and d["input"]=="max"], key=lambda d:(d["sets"], d["T"]*d["A"])):
    print(f"  {d['sets']:7s} {d['adder']:13s} {d['shape']:5s} T={d['T']:7.0f} A={d['A']:6.0f} AT={d['A']*d['T']:.2e} AT2={d['A']*d['T']**2:.2e} W={d['W']:6.0f} D={d['D']:5.0f}")
