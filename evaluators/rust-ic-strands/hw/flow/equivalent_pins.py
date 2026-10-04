"""Find interchangeable input pins of every standard cell from its Liberty logic function, and
print KLayout LVS `equivalent_pins` declarations for them."""
import re, sys, itertools
lib = open(sys.argv[1]).read()
def cells(text):
    for m in re.finditer(r'\n\s*cell\s*\(\s*"?([\w]+)"?\s*\)\s*\{', text):
        start = m.end(); depth = 1; i = start
        while depth:
            depth += {'{': 1, '}': -1}.get(text[i], 0); i += 1
        yield m.group(1), text[start:i]
def pins(body):
    for m in re.finditer(r'\n\s*pin\s*\(\s*"?([\w]+)"?\s*\)\s*\{', body):
        start = m.end(); depth = 1; i = start
        while depth:
            depth += {'{': 1, '}': -1}.get(body[i], 0); i += 1
        b = body[start:i]
        d = re.search(r'direction\s*:\s*"?(\w+)', b)
        f = re.search(r'function\s*:\s*"([^"]+)"', b)
        yield m.group(1), d.group(1) if d else None, f.group(1) if f else None
def to_py(f):
    f = f.replace('!', ' not ').replace("'", '')
    f = re.sub(r'\^', ' != ', f)
    f = f.replace('&', ' and ').replace('*', ' and ').replace('|', ' or ').replace('+', ' or ')
    return f.strip()
for name, body in cells(lib):
    ps = list(pins(body))
    ins = [p for p, d, _ in ps if d == 'input']
    outs = [f for _, d, f in ps if d == 'output' and f]
    if not outs or len(ins) < 2 or len(ins) > 8: continue
    exprs = [compile(to_py(f), name, 'eval') for f in outs]
    if any(set(re.findall(r'[A-Za-z_]\w*', f)) - set(ins) for f in outs): continue
    def table(order):
        rows = []
        for bits in itertools.product([False, True], repeat=len(ins)):
            env = dict(zip(order, bits))
            rows.append(tuple(bool(eval(e, {}, env)) for e in exprs))
        return rows
    base = table(ins)
    groups = []
    for a, b in itertools.combinations(ins, 2):
        sw = [b if p == a else a if p == b else p for p in ins]
        if table(sw) == base:
            for g in groups:
                if a in g or b in g: g.update((a, b)); break
            else: groups.append({a, b})
    for g in groups:
        print('equivalent_pins("%s", %s)' % (name, ', '.join('"%s"' % p for p in sorted(g))))
