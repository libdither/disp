"""After the answer: run back every branch that did not lead to the answer (no heat), keeping
the answer. What is left: the footprints of the answer's past, plus dropped originals."""
import sys
from fpnet import FpNet, FP_VALUES
from run import calls
from collections import Counter

def answer_agents(f):
    seen = set(); st = [f.link[(f.out, 0)][0]]
    while st:
        a = st.pop()
        if a in seen or a not in f.kind: continue
        seen.add(a)
        for q in range(1, len(__import__('fpnet').FP_PORTS[f.kind[a]]) - 1):
            st.append(f.link[(a, q)][0])
    return seen

def uncompute_garbage(f):
    keep = answer_agents(f)
    rounds = 0
    while True:
        act = []
        for fid, (ri, out, prod, _, _) in f.fps.items():
            if not out:
                act.append(('u', fid, None)); continue
            for k in out:
                if f.fresh(fid, k) and prod[k] not in keep:
                    act.append(('a', fid, k)); break
        if not act: break
        rounds += 1
        for kind, fid, k in act:
            if kind == 'u': f.unfire(fid)
            else: f.absorb(fid, k)
    return rounds

def _main():
  for name, head, args, ans in calls():
      if sys.argv[1:] and name not in sys.argv[1:]: continue
      f = FpNet(dismantle=True); f.load_call(head, args)
      loaded = len(f.kind)
      f.forward()
      fp0 = len(f.fps)
      r = uncompute_garbage(f)
      ok = f.readback() is not None
      live = Counter(f.kind.values())
      resid = sum(v for k, v in live.items() if k != 'Out') - len(answer_agents(f))
      by = Counter(f.rules[v[0]].c for v in f.fps.values())
      print(f"{name[:40]:40s} footprints {fp0:6d} -> {len(f.fps):6d} ({len(f.fps)/fp0:4.0%}) in {r} rounds back | "
            f"answer intact {ok} | matter left beside the answer {resid} (loaded {loaded}) | left by kind {dict(by)}")

if __name__ == '__main__':
    _main()
