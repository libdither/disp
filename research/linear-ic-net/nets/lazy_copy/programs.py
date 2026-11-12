"""The hand-written programs of ../check.py, plus size2 (self handed to the arm after the choice)."""
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))
from check import PROGRAMS, TESTS, quote
from ref import parse

SIZE2 = r'''
size2 := Z (\self c. ρ c
   (\o a v p F self. suc (self (F ρ)))
   (\f a v p L self. self L)
   (\f o v p g b self. add (self g) (self b))
   (\f o a p w self. zero)
   (\f o a v self. zero) self)
'''

def program(body):
    return parse(PROGRAMS + SIZE2 + '\n' + body)

READING = [
    ('size suc', 'size suc'),
    ('size size', 'size size'),
    ('size2 suc', 'size2 suc'),
    ('size2 size', 'size2 size'),
    ('size2 size2', 'size2 size2'),
    ('kindOf', 'kindOf (\\x. x)'),
    ('interp quote 2+2', 'interp ' + quote('add (suc (suc zero)) (suc (suc zero))')),
    ('interp quote size suc', 'interp ' + quote('size suc')),
    ('interp interp quote id', 'interp ' + quote('interp ' + quote('(\\x. x) (\\y. y)'))),
]
ADDS = [(name, src) for name, src in TESTS if 'add' in name or 'subtract' in name]
LINEAR = [(name, src) for name, src in ADDS if name.startswith(('linear', 'reversible'))]
