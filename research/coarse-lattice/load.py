import csv, math, collections
D = __import__("os").path.dirname(__import__("os").path.abspath(__file__))
def rows(p):
    return list(csv.DictReader(open(p)))
lat = rows(f"{D}/summary.csv"); ide = rows(f"{D}/summary_ideal.csv")
I = {(r["id"], r["sets"]): r for r in ide}
def f(x):
    try: return float(x)
    except: return float("nan")
data = []
for r in lat:
    if r["id"] == "id": continue
    k = (r["id"], (r["sets"] + "|ideal") if r["sets"] else "|ideal")
    i = I.get(k)
    if not i or not r["clocks"]: continue
    d = dict(id=r["id"], sets=r["sets"] or "plain", adder=r["adder"], shape=r["shape"], n=int(r["n"]), input=r["input"],
             T=f(r["clocks"]), Tmin=f(r["clocks_min"]), Tmax=f(r["clocks_max"]), W=f(r["rewrites"]), A=f(r["peak_sites"]),
             S=f(r["peak_strands"]), N0=f(r["agents"]), D=f(i["depth"]), Wi=f(i["rewrites"]), good=int(r["good"]), runs=int(r["runs"]),
             dn=f(r["dn_share"]), eps=f(r["eps_share"]))
    if any(math.isnan(d[q]) for q in "T W A D Wi".split()): continue
    data.append(d)
