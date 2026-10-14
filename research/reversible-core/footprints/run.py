import sys, time, random
from fpnet import FpNet, CurNet, tern, to_tern

def calls(path='calls.txt'):
    for line in open(path):
        line = line.rstrip('\n')
        if not line:
            continue
        name, head, args, ans = line.split('|')
        yield name, tern(head), [tern(a) for a in args.split(',') if a], ans


def run_one(name, head, args, ans, back=True, dismantle=False, cur=True):
    out = {'name': name}
    t0 = time.time()
    if cur:
        c = CurNet()
        c.load_call(head, args)
        loaded_cur = len(c.kind)
        r, w = c.forward()
        got = c.readback()
        out['cur'] = dict(rounds=r, rewrites=sum(w), widest=max(w) if w else 0,
                          ok=(got is not None and to_tern(got) == ans), loaded=loaded_cur, left=len(c.kind))
    f = FpNet(dismantle=dismantle)
    f.load_call(head, args)
    k0, l0 = f.snapshot()
    r, w = f.forward()
    got = f.readback()
    live, fps, held, held_copies = f.leftover()
    out['fp'] = dict(rounds=r, fires=sum(w), widest=max(w) if w else 0,
                     ok=(got is not None and to_tern(got) == ans), loaded=len(k0),
                     live=sum(live.values()), footprints=len(f.fps), held=held, held_copies=held_copies,
                     stats=dict(f.stats), fp_by_rule=dict(fps))
    if back:
        for mode in ('one', 'all'):
            g = f if mode == 'one' else None
            if g is None:
                g = FpNet(dismantle=dismantle); g.load_call(head, args); g.forward()
            br, bw = g.backward(mode)
            k1, l1 = g.snapshot()
            out['fp']['back_' + mode] = dict(rounds=br, widest=max(bw) if bw else 0, exact=(k1 == k0 and l1 == l0))
    out['secs'] = round(time.time() - t0, 1)
    return out


if __name__ == '__main__':
    pick = sys.argv[1:] or None
    dismantle = False
    if pick and pick[0] == '--dismantle':
        dismantle = True; pick = pick[1:] or None
    for name, head, args, ans in calls():
        if pick and not any(p == name for p in pick):
            continue
        o = run_one(name, head, args, ans, dismantle=dismantle)
        print(o, flush=True)
