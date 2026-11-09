# The 9-rule linear tree calculus (V5)

One node, △. A redex is △ a b x; the shape of a, plus one more node, picks the rule. Every rule
eats exactly three △. Only K drops and only W copies; triage hands back the arms it didn't pick.

| rule | redex | result |
|---|---|---|
| I | △ △ △ x | x |
| K | △ △ (△ q) x | q |
| C | △ △ (△ q r) x | q x r |
| W | △ (△ △) b x | b x x |
| B | △ (△ (△ f)) b x | f (b x) |
| B′ | △ (△ (△ f g)) b x | f (g (b x)) |
| triage on a leaf | △ (△ c d) b △ | c d b |
| triage on a stem | △ (△ c d) b (△ u) | d c b u |
| triage on a fork | △ (△ c d) b (△ u v) | b c d u v |

Compiling λ-terms (bracket abstraction, `family.py` `make_compiler`): λx. x is I; an unused x gives
K M; λx. M x with x not in M is M; x only on the right gives B M (λx. N), and B M (B g r) merges to
B′ M g r; x only on the left gives C (λx. M) N; x on both sides gives W (λx. λx′. M N′).

`final.py` runs every check in one go and prints the rules. On 2026-11-08 it reported: left-linear,
only K drops, only W copies, three △ per rule, no overlaps; 11,997 runs in random rule order reach the
same tree; 3,000 random typed programs agree between a λ reference, TC and LTC; 3,000 random linear
programs never fire K or W; on five recursive programs run eagerly, V5 does 0.47× TC's rewrites.

## Files

The calculus and its checks:
- `rules.py`: LTC and TC rules as data, one engine for both.
- `family.py`: nine-slot designs in three families by the function's left child, and the compiler.
- `final.py`: the final design, every check in one run.
- `check_rules.py`, `check_dynamic.py`: static checks (linearity, overlaps, △ eaten per rule)
  and random-term confluence.
- `compile.py`, `lower.py`: λ-terms to TC and LTC; source programs written with triage.
- `test_compile.py`, `test_linear.py`, `family_test.py`: random typed and linear programs
  against a λ reference.
- `bench.py`: eager runs of recursive programs on TC and LTC.

Design exploration:
- `ltc2.py`, `check2.py`, `test2.py`, `bench2.py`: an earlier nine-rule design (LTC2), its checks and benchmarks.
- `variant.py`, `variants.py`: what the ninth slot should be (B′ won), and triage protocols compared.
- `superopt.py`, `superopt2.py`: shortest trees for common routing patterns against what bracket
  abstraction emits.
- `strategy.py`: linear programs take the same number of steps eagerly and in normal order.
- `ltc7.py`, `ltc7w.py`, `ltc7_check.py`, `ltc7c_check.py`, `ltc7_bench.py`: seven-rule variants.
- `ltc_small.py`, `ltc_small_check.py`, `ltc_small_bench.py`: two drop-free designs and the Kraft-sum
  argument about rule-set sizes.
- `loopstyle.py`: two ways to loop in ordinary programs.

Reversible programs (the early ones; `../reversible/` has the measured set):
- `radd.py`, `radd2.py`, `radd_search.py`: reversible add, and the cheapest argument order.
- `rmirror.py`: reversible mirror of a tree.
- `rev_study.py`, `rev_study2.py`: running LTC backward, and Bennett's method against the reversible add.
