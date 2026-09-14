#!/usr/bin/env python
"""research/awesome-langs/_SCORES.md is the source of every score; this recomputes and propagates it.

  python scripts/awesome-scores.py            report: each axis ranked, symbol changes, ahead-of-disp sets, open cells
  python scripts/awesome-scores.py --write    also rewrite the % column in _SCORES.md and copy score, clause
                                              values and why into every write-up's scorecard, the master table
                                              cells, and disp's table in _AXES.md

A clause is 0, ½ or 1 (suffix † = from general knowledge, ? = open, scored 0); an axis is the mean
of its three clauses; the symbol is ✗ below 25, ◐ to 79, ✅ from 80; bold in the master table means
a higher percentage than disp's.
"""
import re, sys, pathlib

D = pathlib.Path(__file__).resolve().parent.parent / 'research' / 'awesome-langs'
AXES = ['A1', 'A2', 'A3', 'A4', 'A5', 'A6']
VAL = {'1': 1.0, '½': 0.5, '0': 0.0, '?': 0.0}
SUP = 'ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻ'
ROW = re.compile(r'^\| (A[1-6]) [^|]*\|')

def symbol(pct):
    return '✗' if pct < 25 else '◐' if pct < 80 else '✅'

def clean(name):
    return name.replace('\\', '').strip()

def master():
    """name -> (file, {axis: (symbol, ahead)}, line index) from the master table."""
    out, lines = {}, (D / 'AWESOME-LANGS.md').read_text().split('\n')
    for i, line in enumerate(lines):
        m = re.match(r'^\| \[\*\*(.+?)\*\*(.*?)\]\((.+?\.md)\) \|', line)
        if not m: continue
        cells = [c.strip() for c in line.split('|')[2:8]]
        row = {}
        for ax, cell in zip(AXES, cells):
            sym = next((ch for ch in cell.replace('**', '') if ch in '✅◐✗'), None)
            row[ax] = (sym, '**' in cell)
        out[clean(m.group(1) + m.group(2))] = (m.group(3), row, i)
    return out, lines

def disp_symbols():
    out = {}
    for line in (D / '_AXES.md').read_text().split('\n'):
        m = ROW.match(line)
        if m: out[m.group(1)] = (next(ch for ch in line.split('|')[2] if ch in '✅◐✗'), False)
    return out

def scores():
    """axis -> rows of dict(name, vals, pct, prov, knowledge, why, line); plus the file's lines."""
    lines = (D / '_SCORES.md').read_text().split('\n')
    out, ax = {}, None
    for i, line in enumerate(lines):
        m = re.match(r'^## (A[1-6]) ', line)
        if m: ax = m.group(1); out[ax] = []; continue
        if ax and line.startswith('| ') and not line.startswith('| Project') and not line.startswith('|---'):
            cells = [c.strip() for c in line.split('|')[1:-1]]
            if len(cells) < 6: continue
            name, vals, why = cells[0], cells[1:4], cells[5]
            for v in vals:
                if v.replace('†', '') not in VAL: sys.exit(f'{ax} {name}: bad clause value {v!r}')
            pct = round(sum(VAL[v.replace('†', '')] for v in vals) / 3 * 100)
            out[ax].append(dict(name=name, vals=vals, pct=pct, prov=any('?' in v for v in vals),
                                knowledge=any('†' in v for v in vals), why=why, line=i))
    return out, lines

def pct_text(r):
    return f"{r['pct']}%{'?' if r['prov'] else ''}"

def rewrite_scorecard(path, by_axis):
    """Score cell ← `sym pct (tag)`, a Clauses column ← `a · b · c — why`; header and rule grow a column."""
    lines = path.read_text().split('\n')
    first = None
    for i, line in enumerate(lines):
        m = ROW.match(line)
        if not m or m.group(1) not in by_axis: continue
        first = i if first is None else first
        cells = [c.strip() for c in line.split('|')[1:-1]]
        r = by_axis[m.group(1)]
        tag = re.search(r'\(([^)]+)\)\s*$', cells[1])
        cells[1] = f"{symbol(r['pct'])} {pct_text(r)}" + (f" ({tag.group(1)})" if tag else '')
        clauses = f"{' · '.join(r['vals'])} — {r['why']}"
        if len(cells) >= 4: cells[3] = clauses
        else: cells.append(clauses)
        lines[i] = '| ' + ' | '.join(cells) + ' |'
    if first is None: return False
    head, rule = first - 2, first - 1
    if lines[head].count('|') == 4: lines[head] = lines[head].rstrip() + ' Clauses |'
    if lines[rule].count('|') == 4: lines[rule] = lines[rule].rstrip() + '---|'
    path.write_text('\n'.join(lines))
    return True

def main():
    sc, lines = scores()
    ms, mlines = master()
    disp_pct = {ax: next(r['pct'] for r in rows if r['name'] == 'disp') for ax, rows in sc.items()}
    if '--write' in sys.argv:
        # 1. the % column of _SCORES.md
        for rows in sc.values():
            for r in rows:
                cells = lines[r['line']].split('|')
                cells[5] = f" {r['pct']}{'?' if r['prov'] else ''} "
                lines[r['line']] = '|'.join(cells)
        (D / '_SCORES.md').write_text('\n'.join(lines))
        # 2. each write-up's scorecard, and disp's table in _AXES.md
        by_name = {}
        for ax, rows in sc.items():
            for r in rows: by_name.setdefault(r['name'], {})[ax] = r
        touched = []
        for name, by_axis in by_name.items():
            path = D / '_AXES.md' if name == 'disp' else D / ms[name][0] if name in ms else None
            if path and rewrite_scorecard(path, by_axis): touched.append(path.name)
        # 3. master table cells: symbol, percent, bold when ahead; superscripts kept
        for name, (file, row, i) in ms.items():
            if name not in by_name: continue
            cells = mlines[i].split('|')
            for k, ax in enumerate(AXES):
                r = by_name[name].get(ax)
                if not r: continue
                sup = ''.join(ch for ch in cells[2 + k] if ch in SUP)
                text = f"{symbol(r['pct'])} {pct_text(r)}"
                cells[2 + k] = f" {'**' + text + '**' if r['pct'] > disp_pct[ax] else text}{sup} "
            mlines[i] = '|'.join(cells)
        (D / 'AWESOME-LANGS.md').write_text('\n'.join(mlines))
        print(f"written: _SCORES.md, AWESOME-LANGS.md, {len(touched)} scorecards ({', '.join(touched)})")
        ms, _ = master()
    old = {n: row for n, (f, row, i) in ms.items()}
    old['disp'] = disp_symbols()
    flips, ahead_changes, open_cells, knowledge = [], [], [], []
    for ax, rows in sc.items():
        print(f"\n== {ax}  (disp {disp_pct[ax]})")
        for r in sorted(rows, key=lambda r: -r['pct']):
            m = old.get(clean(r['name']), {}).get(ax, (None, False))
            sym, ahead = symbol(r['pct']), r['pct'] > disp_pct[ax]
            flag = ''
            if m[0] and m[0] != sym: flag += f'  flip {m[0]}→{sym}'; flips.append((ax, r['name'], m[0], sym, r['pct']))
            if r['name'] != 'disp' and ahead != m[1]:
                flag += '  ahead' if ahead else '  no longer ahead'; ahead_changes.append((ax, r['name'], ahead))
            if r['prov']: flag += '  ?'; open_cells.append((ax, r['name']))
            if r['knowledge']: flag += '  †'; knowledge.append((ax, r['name']))
            print(f"  {r['pct']:>3}{'?' if r['prov'] else ' '} {sym} {r['name']:<32} {' '.join(r['vals']):<9}{flag}")
    total = sum(len(v) for v in sc.values())
    print(f"\n== summary: {total} cells · {len(flips)} symbol changes vs the files · {len(ahead_changes)} ahead changes · "
          f"{len(open_cells)} open (?) · {len(knowledge)} from knowledge (†)")
    for ax, n, a, b, p in flips: print(f"  flip  {ax} {n}: {a} → {b} ({p})")
    for ax, n, a in ahead_changes: print(f"  ahead {ax} {n}: {'now ahead of disp' if a else 'no longer ahead'}")
    for ax, n in open_cells: print(f"  open  {ax} {n}")

if __name__ == '__main__':
    main()
