#!/usr/bin/env python
"""Recompute research/awesome-langs/_SCORES.md and report what the numbers change.

  python scripts/awesome-scores.py            report: rankings, symbol flips, ahead sets, open cells
  python scripts/awesome-scores.py --write    also rewrite the % column from the clause values
"""
import re, sys, pathlib

D = pathlib.Path(__file__).resolve().parent.parent / 'research' / 'awesome-langs'
AXES = ['A1', 'A2', 'A3', 'A4', 'A5', 'A6']
VAL = {'1': 1.0, '½': 0.5, '0': 0.0, '?': 0.0}

def symbol(pct):
    return '✗' if pct < 25 else '◐' if pct < 80 else '✅'

def clean(name):
    return name.replace('\\', '').strip()

def master():
    """name -> {axis: (symbol, ahead)} from the master table, plus disp from _AXES.md."""
    out = {}
    for line in (D / 'AWESOME-LANGS.md').read_text().split('\n'):
        m = re.match(r'^\| \[\*\*(.+?)\*\*(.*?)\]\(.+?\.md\) \|', line)
        if not m: continue
        cells = [c.strip() for c in line.split('|')[2:8]]
        row = {}
        for ax, cell in zip(AXES, cells):
            sym = next((ch for ch in cell.replace('**', '') if ch in '✅◐✗'), None)
            row[ax] = (sym, '**' in cell)
        out[clean(m.group(1) + m.group(2))] = row
    disp = {}
    for line in (D / '_AXES.md').read_text().split('\n'):
        m = re.match(r'^\| (A[1-6]) [^|]*\| ([^|]*)\|', line)
        if m: disp[m.group(1)] = (next(ch for ch in m.group(2) if ch in '✅◐✗'), False)
    out['disp'] = disp
    return out

def scores():
    """axis -> list of dict(name, vals, pct, prov, line_index); also the file's lines."""
    lines = (D / '_SCORES.md').read_text().split('\n')
    out, ax = {}, None
    for i, line in enumerate(lines):
        m = re.match(r'^## (A[1-6]) ', line)
        if m: ax = m.group(1); out[ax] = []; continue
        if ax and line.startswith('| ') and not line.startswith('| Project') and not line.startswith('|---'):
            cells = [c.strip() for c in line.split('|')[1:-1]]
            if len(cells) < 6: continue
            name, vals = cells[0], cells[1:4]
            for v in vals:
                if v.replace('†', '') not in VAL: sys.exit(f'{ax} {name}: bad clause value {v!r}')
            pct = round(sum(VAL[v.replace('†', '')] for v in vals) / 3 * 100)
            out[ax].append(dict(name=name, vals=vals, pct=pct, prov=any('?' in v for v in vals),
                                knowledge=any('†' in v for v in vals), line=i))
    return out, lines

def main():
    sc, lines = scores()
    ms = master()
    if '--write' in sys.argv:
        for ax, rows in sc.items():
            for r in rows:
                cells = lines[r['line']].split('|')
                cells[5] = f" {r['pct']}{'?' if r['prov'] else ''} "
                lines[r['line']] = '|'.join(cells)
        (D / '_SCORES.md').write_text('\n'.join(lines))
        print('percentages rewritten')
    flips, ahead_changes, open_cells, knowledge = [], [], [], []
    for ax, rows in sc.items():
        disp_pct = next(r['pct'] for r in rows if r['name'] == 'disp')
        print(f"\n== {ax}  (disp {disp_pct})")
        for r in sorted(rows, key=lambda r: -r['pct']):
            m = ms.get(clean(r['name']), {}).get(ax, (None, False))
            sym = symbol(r['pct'])
            flag = ''
            if m[0] and m[0] != sym: flag += f'  flip {m[0]}→{sym}'
            ahead = r['pct'] > disp_pct
            if r['name'] != 'disp' and ahead != m[1]:
                flag += '  ahead' if ahead else '  no longer ahead'
            if r['prov']: flag += '  ?'
            if r['knowledge']: flag += '  †'
            print(f"  {r['pct']:>3}{'?' if r['prov'] else ' '} {sym} {r['name']:<32} {' '.join(r['vals']):<9}{flag}")
            if m[0] and m[0] != sym: flips.append((ax, r['name'], m[0], sym, r['pct']))
            if r['name'] != 'disp' and ahead != m[1]: ahead_changes.append((ax, r['name'], ahead))
            if r['prov']: open_cells.append((ax, r['name']))
            if r['knowledge']: knowledge.append((ax, r['name']))
    total = sum(len(v) for v in sc.values())
    print(f"\n== summary: {total} cells · {len(flips)} symbol flips · {len(ahead_changes)} ahead changes · "
          f"{len(open_cells)} open (?) · {len(knowledge)} from knowledge (†)")
    for ax, n, a, b, p in flips: print(f"  flip  {ax} {n}: {a} → {b} ({p})")
    for ax, n, a in ahead_changes: print(f"  ahead {ax} {n}: {'now ahead of disp' if a else 'no longer ahead'}")
    for ax, n in open_cells: print(f"  open  {ax} {n}")

if __name__ == '__main__':
    main()
