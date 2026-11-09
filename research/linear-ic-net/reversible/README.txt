Reversible programs in LTC (V5), measured. Rerun everything: ./run_all.sh  (memory-capped, time-limited).
Prototype (read only, imported): ../tree-calculus
ltcfast.py   hash-consed iterative eager evaluator for V5 and tree calculus; xcheck.py proves it matches eager()/bench.run
common.py    compilers (prototype bracket abstraction), encodings, run() that asserts the exact size law on every V5 run
so2.py so3.py  brute-force superoptimizers (plain / with one continuation constant K); so*jobs.py the searches;
             so*_*.log their results (addx4.best_found reads the smallest tree per stage from these logs)
ord_add.py   ordinary binary add (LTC with K, tree calculus)
addx.py -> addx2.py -> addx3.py -> addx4.py -> addx6.py   reversible adder, each design step (addx6 = final, output reversed)
adderd.py    reversible adder in place (same chain layout in and out, direct style)
chainm.py    tail-recursive chain machine: reverse / bit reversal, map NOT, increment, decrement
chainm2.py   direct-style (order preserving) chain machine: in-place map NOT, increment, decrement, rotate, Gray code
gains.py     one bracket arm vs bracket stages vs superoptimized stages
ordchain.py ordmore.py   ordinary versions of the chain programs and of XOR
twochain.py  XOR of two words (CNOT per bit)
gates.py     NOT, CNOT, Toffoli, Fredkin, swap, controlled swap (ordinary and reversible); router.py: router-bit controlled swap
mult.py      unary multiply (reversible nested loops, ordinary)
mix.py       rule mix of the adders
