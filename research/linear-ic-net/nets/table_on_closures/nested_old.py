"""Problem (ii): what the four random programs with nested tables that did not finish in case_table's net are doing.
Runs each (from /tmp/mm.pkl) in casenet to growing limits (fresh each time) and prints: live agents, rewrites by
kind, reads, the most fired rules; and the reference's betas and the size of its answer.
usage: python nested_old.py [limits...]"""
import sys, os, pickle, time
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', 'case_table'))
sys.path.insert(0, os.path.join(HERE, '..'))
import casenet as C


def tsize(t):
    if isinstance(t, str): return 1
    if t[0] == '~': return 1 + tsize(t[1])
    if C.is_tab(t): return 1 + sum(tsize(a) for a in t[2])
    if t[0] == 'part': return 1 + sum(tsize(a) for a in t[1])
    if len(t) == 3: return 1 + tsize(t[2])
    return 1 + tsize(t[0]) + tsize(t[1])


def main():
    limits = [int(a) for a in sys.argv[1:]] or [100000, 400000, 1600000]
    progs = pickle.load(open('/tmp/mm.pkl', 'rb'))
    for k, (policy, t) in enumerate(progs):
        t0 = time.time()
        m = C.Machine(10 ** 7)
        try:
            v = m.run(t); ref = f'value, {m.betas} betas, {10 ** 7 - m.fuel} applications, answer size {tsize(v)}'
        except Exception as e:
            ref = f'{type(e).__name__}, {m.betas} betas'
        print(f'#{k} {policy} size {tsize(t)}  reference: {ref}  ({time.time() - t0:.1f}s)')
        for lim in limits:
            n = C.CNet(None)
            n.load(t)
            st = n.run(lim)
            g = C.group(n.stats)
            print(f'   limit {lim:8}: {st:5} alive {len(n.kind):8}  reads {n.stats["reads"]:6}  '
                  f'copy {g["copy"]:8} erase {g["erase"]:8} annih {g["annihilate"]:7} rest {g["rest"]:7}  ({time.time() - t0:.0f}s)')
            sys.stdout.flush()
            if st != 'limit': break
        top = sorted(((v, k2) for k2, v in n.stats.items() if k2 not in ('interactions', 'reads') and not k2.startswith('slot')), reverse=True)[:8]
        print('   top rules:', top)
        sys.stdout.flush()


if __name__ == '__main__':
    import threading, resource
    resource.setrlimit(resource.RLIMIT_AS, (3 << 30, 3 << 30))
    threading.stack_size(512 * 1024 * 1024)
    sys.setrecursionlimit(1000000)
    th = threading.Thread(target=main); th.start(); th.join()
