# Type system goals

I want types that mean exactly what running programs show, and checkers that are the evaluator itself, run on unknowns.
> I want a type to be a program like any other. Programs are binary trees, and applying one to another is deterministic and may never finish. A type's meaning should be a search for a counterexample, and its checker the same evaluator taking sound shortcuts, so that it can say yes to members in finite time. Better checkers should accept more programs without ever changing what a type means.

To hold a design (a former, an optimization, a checker) against this, whoever proposes it, the optimizer included, answer each Check:

1. **A type's meaning is a refuter.** Run on a program, it stops with a counterexample exactly when the program is not a member. Members are the programs it never stops on.
   - *Check:* what is the counterexample, and which bounded run confirms it? "Finishes on every input" has none, so it is not a meaning; "finishes within a stated number of steps" has one.
2. **A checker's yes is never refuted.** On a member it may say no or run forever; that refutes nothing, and the member stays open until a better checker accepts it. Run with a budget, any checker stops and stays sound.
   - *Check:* which program could it say yes to that the meaning refutes? (Search can look for one.)
3. **A checker is the evaluator, optimized.** It runs the program on unknowns, each standing for every program its type's checker accepts. A yes means: for every filling, if the program finishes, the result type's checker says yes to its result. An optimization answers an application that touches an unknown without running it: split the unknown into its type's constructors, answer a checker applied to an unknown of its own type by yes, answer a recursive call on an accepted input by an unknown of the result type (if the run finishes, that call finished in fewer steps).
   - Each optimization must be sound on its own, so any mix of them is.
   - An unknown is never read as a tree or built by a program: an application touching one is answered by an optimization, or the check says no.
   - *Check:* for each application answered without running, which filling of a finishing run gives something the answer does not cover?
4. **Types come only from formers.** A former builds a meaning and a checker together from its parts. A type is admitted when its parts are, and its former, given them again, builds the identical tree.
   - *Check:* does an admitted former rebuild the type exactly, and can you name a pair it could build that fails 1 or 2?
5. **Meaning never moves.** A function type's meaning ranges over the inputs its domain's checker accepts. So a checker is a program like any other: one that accepts the same programs may replace it, and one that accepts more is a new type.
   - *Check:* does any existing type's refuter now stop, or its checker say yes, on a different set of programs?
