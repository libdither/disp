#!/usr/bin/env bash
# Rerun every measurement (memory-capped, time-limited). Superoptimized stage trees are read from so*_*.log.
cd "$(dirname "$0")"
cap() { systemd-run --user --scope -q -p MemoryMax=4G -p MemorySwapMax=1G timeout 1500 python "$@"; }
echo '## cross-check fast evaluator vs prototype';           cap xcheck.py
echo '## ordinary binary add (LTC, TC)';                      cap ord_add.py
echo '## reversible add: compiled design X';                  cap addx.py
echo '## reversible add: self-spared arms (X2)';              cap addx2.py
echo '## reversible add: X3 (frames as arms, fall-through)';  cap addx3.py
echo '## reversible add: X4 staged (bracket, superopt)';      cap addx4.py
echo '## reversible add: H-form inactive state (final)';     cap addx6.py
echo '## reversible add in place (direct style)';            cap adderd.py
echo '## chain machines, tail recursive';                     cap chainm.py
echo '## chain machines, direct style (in place)';           cap chainm2.py
echo '## chain machines: bracket vs staged vs superopt';     cap gains.py
echo '## ordinary chain programs';                            cap ordchain.py; cap ordmore.py
echo '## XOR of two words';                                   cap twochain.py
echo '## gates, swap, controlled swap';                      cap gates.py; cap router.py
echo '## unary multiply';                                     cap mult.py
echo '## rule mix of the adders';                             cap mix.py
