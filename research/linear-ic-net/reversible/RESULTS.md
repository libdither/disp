# Reversible programs in the 9-rule calculus: results

Measured by `run_all.sh` (see `README.txt` for the file map). Every reversible run asserts 0 K steps
and that the end term is exactly the program handed back plus the outputs; every LTC run asserts the
size law (copied − dropped = 3·steps + |end| − |start|). `ltcfast.py` is a fast evaluator that matches
the prototype's counters exactly on 19 runs (`xcheck.py`). Words are chains, least significant bit
first: bit 0 = `△ rest`, bit 1 = `△ △ rest`. "Steps/bit" is the marginal cost between 16 and 32 bits.
Rerun on 2026-11-08 from this folder: `xcheck.py` and `addx6.py` (the final adder); the other numbers
are from the original run.

**Why it's fast.** With no K, 3·steps = copied + |start| − |end|. A program that hands itself back
copies only the arm that just ran, so steps per element ≈ (size of that arm) / 3. That is also why the
reversible versions beat the compiled ordinary ones, which copy the whole program every round: an
ordinary program written to copy only what ran would cost about the same.

## Binary adder, (a, b) ↦ (a, a+b mod 2^w), w = 32, 30 random adds

| version | steps | K | W | copied | dropped | steps/bit |
|---|---|---|---|---|---|---|
| ordinary, tree calculus | 5880 | 2820 | 0 (copies by S) | 633,209 | 622,760 | 183 |
| ordinary, LTC (compiled) | 2144 | 372 | 33 | 23,265 | 18,324 | 67 |
| reversible, first version (compiled arms) | 3423 | 0 | 33 | 10,269 | 0 | 103 |
| reversible, final (output reversed) | 1016 | 0 | 33 | 3,046 | 0 | 29.3 |
| reversible, in place (same layout in and out) | 1483 | 0 | 33 | 4,451 | 0 | 43.4 |

How it got to 29.3 steps/bit: compiled arms (108), each arm's spare stored inside its own fork (103),
forks acting as arms so one triage feeds the next (62.8), staged arms (39.8), brute-force-smallest
stages (33.0), and the inactive carry packed into one value with the running sum first (29.3). With the state first
instead it's 33.3 (`addx6.py`); the 59.9 in the original report belongs to the earlier two-value layout in `addx5.py`.

For comparison, in gates rather than rewrite steps: Cuccaro's ripple-carry adder uses
[2n−3 Toffolis, 5n−7 CNOTs and 2n−6 NOTs mod 2ⁿ](https://arxiv.org/pdf/quant-ph/0410184#page=5)
with [a single ancillary qubit](https://arxiv.org/pdf/quant-ph/0410184#page=1); the VBE adder uses
[about 4n Toffolis and 4n CNOTs](https://arxiv.org/pdf/quant-ph/0410184#page=1) and an
[(n−1)-qubit carry register reset after each operation](https://arxiv.org/pdf/quant-ph/9511018#page=5).
The tree adder needs no spare register and no undo pass: the carry is which arm runs.

## Word programs, w = 32, steps per bit

| program | reversible | ordinary LTC | ordinary TC |
|---|---|---|---|
| reverse (bit reversal) | 20.5 | 30.5 | 77.5 |
| map NOT, in place | 23.2 | 30.5* | 77.5* |
| increment, in place | 24.7 | 34.5* | 80.5* |
| decrement, in place | 24.9 | 34.5* | 80.5* |
| rotate, in place | 25.7 | 34.8* | 80.9* |
| Gray code, in place | 31.5 | 35.1* | 81.1* |
| XOR of two words, output reversed | 23.2 | 44.0 | 102.0 |

\* The ordinary versions are tail recursive and return their output reversed, so they do less than
the in-place reversible ones. Tail-recursive reversible map NOT, increment and decrement also run at 20.5.

## Single operations (averaged over all inputs)

| operation | reversible | ordinary LTC | ordinary TC |
|---|---|---|---|
| NOT | 72 | 3.5 | 1.5 |
| CNOT | 72 | 5.8 | 3.2 |
| Toffoli | 162 | 7.6 | 6.1 |
| controlled swap | 31 by triage; 26–28 with the control bit as a routing program | 6.0 | 7.5 |
| swap | 13 | 2.0 | 4.0 |

Handing the program back has a fixed cost, so reversible versions only win inside loops.

## Unary multiply, (a, b, p) ↦ (a, b, p+ab), 20 × 20

| version | steps | K | W | copied | dropped | steps per unit |
|---|---|---|---|---|---|---|
| reversible | 10,201 | 0 | 441 | 31,008 | 0 | 25.5 |
| ordinary LTC | 9,547 | 905 | 462 | 46,875 | 18,426 | 23.9 |
| ordinary TC | 26,798 | 12,539 | – | 1,457,155 | 1,402,890 | 67.0 |

Binary multiply wasn't built: the adder's output layout differs from its input.

## Limits found

- Everything tried could be made drop-free; the limit is cost.
- A triage on a bit has two arms that can run and one free slot for a spare, so an extra triage level
  is needed (3–4 steps/bit).
- A triage chain carries only two data items, so the adder's carry is control state; switching it is
  about 42% of the adder's cost.
- Tail recursion reverses output; restoring order costs a wrap per element or a second pass.
- No K doesn't make a program one-to-one: triage merges paths (carry, overflow), and that information
  survives only in the step history.
- The superoptimizer stops near 21 nodes; five stages (mostly the carry switches) still use compiled
  arms, and that's where the remaining slack is.
