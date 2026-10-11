# Copies against one fixed unit (README "Fixed units"): m calls of relay_add on 4-bit trees side by side,
# each with its own copy of the adder, or all served by one unit d sites right of the program, unitlat
# clocks a call. Every run memory-capped and timed, its answer checked against the oracle; prints mean
# clocks to the answer (and peak sites) per configuration. Run from anywhere, after
# `cargo build --release` in crate/:
#   python evaluators/rust-ic-strands/programs/units.py [--calls 1,4,8] [--dist 2,8,16,32,64,128] [--lat 20,500] [--seeds 2] [--jobs 4]
import argparse, concurrent.futures, os, re, statistics, subprocess, tempfile

here = os.path.dirname(os.path.abspath(__file__))
repo = os.path.normpath(os.path.join(here, "../../.."))
run_bin = os.path.join(here, "../crate/target/release/strands-run")
ints = lambda s: [int(x) for x in s.split(",")]
ap = argparse.ArgumentParser()
ap.add_argument("--calls", type=ints, default=[1, 4, 8])
ap.add_argument("--dist", type=ints, default=[2, 8, 16, 32, 64, 128])
ap.add_argument("--lat", type=ints, default=[20, 500])
ap.add_argument("--seeds", type=int, default=2)
ap.add_argument("--jobs", type=int, default=4)
opt = ap.parse_args()

# The calls, compiled as the player compiles them: the adder's code, then its two arguments.
inputs = [(13, 6), (5, 9), (7, 7), (2, 11)]
printed = subprocess.run(["systemd-run", "--user", "--scope", "-q", "-p", "MemoryMax=3G", "-p", "MemorySwapMax=256M", "timeout", "600",
                          "npx", "tsx", "evaluators/rust-ic-strands/programs/bench.ts", "--data", "--print",
                          *[f"relay_add (bit_tree 2 {x}) (bit_tree 2 {y})" for x, y in inputs]],
                         cwd=repo, capture_output=True, text=True, check=True).stdout.split("\n")
calls = [line.split() for line in printed if line.strip()]
work = tempfile.mkdtemp(prefix="strands-units-")
code = os.path.join(work, "relay_add.code")
open(code, "w").write(calls[0][0])

def term(m):
    """m calls side by side, as the player's `a; b; …`: F(a, F(b, …))."""
    path = os.path.join(work, f"calls{m}.term")
    if not os.path.exists(path):
        parts = [" ".join(calls[i % len(calls)]) for i in range(m)]
        t = parts[-1]
        for p in reversed(parts[:-1]): t = f"F({p},{t})"
        open(path, "w").write(t)
    return path

def run(m, d, lat, seed):
    unit = [] if d is None else ["--unit", "@" + code, f"unitd={d}", f"unitlat={lat}"]
    out = subprocess.run(["systemd-run", "--user", "--scope", "-q", "-p", "MemoryMax=2G", "-p", "MemorySwapMax=256M", "timeout", "1800",
                          run_bin, "@" + term(m), "--latest", "--fit", "--seed", str(seed), *unit], capture_output=True, text=True).stdout
    ans = re.search(r"DONE — answer (\S+) \(want (\S+)\)", out)
    clocks, sites = re.search(r"^clocks (\d+)", out, re.M), re.search(r"peak live sites (\d+)", out)
    ok = bool(ans) and ans.group(1) == ans.group(2) and clocks and sites
    return (m, d, lat), (int(clocks.group(1)), int(sites.group(1))) if ok else None

configs = [(m, None, 0) for m in opt.calls] + [(m, d, lat) for m in opt.calls for lat in opt.lat for d in opt.dist]
jobs = [(*c, seed) for c in configs for seed in range(1, opt.seeds + 1)]
results = {}
with concurrent.futures.ThreadPoolExecutor(opt.jobs) as ex:
    for key, r in ex.map(lambda j: run(*j), jobs): results.setdefault(key, []).append(r)

def cell(key):
    rs = results[key]
    if any(r is None for r in rs): return "FAIL"
    c = statistics.mean(r[0] for r in rs)
    return f"{c / 1000:.1f}k" if c >= 1000 else f"{c:.0f}"
def sites(key): return "-" if any(r is None for r in results[key]) else f"{statistics.mean(r[1] for r in results[key]):.0f}"
for m in opt.calls:
    print(f"\n{m} call{'s' if m > 1 else ''}: copies {cell((m, None, 0))} clocks, {sites((m, None, 0))} peak sites")
    for lat in opt.lat:
        print(f"  unit, {lat} clocks a call: " + ", ".join(f"d={d} {cell((m, d, lat))}" for d in opt.dist)
              + f"  (peak sites {sites((m, opt.dist[0], lat))}–{sites((m, opt.dist[-1], lat))})")
