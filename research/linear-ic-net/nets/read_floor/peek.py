"""The charge keys behind some causes: python peek.py program input 'cause substring'..."""
import sys
from account import table, detail, cause

prog, inp, *wanted = sys.argv[1:]
n, rows, reads = table(prog, inp)
for w in wanted:
    print(f'\n  {w}:')
    detail(n, reads, lambda key: w in cause(key))
