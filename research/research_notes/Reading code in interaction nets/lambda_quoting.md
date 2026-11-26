# How λ-calculus represents and reads its own programs

Scope: quoting, self-interpreters, higher-order syntax, opening and closing binders, and reading a function by
running it, set against the net's "view" (see `this_net_measured.md`). Every quote below was found in the linked
source by `scripts/verify-source.sh -q`; the link is the one the script printed. "p. N" is a journal page
(Barendregt, Mogensen); "PDF p. N" is the page of the linked file. Formulas marked "transcribed" were copied by hand from a page whose symbols garble
in text extraction; for those the script checked only the prose next to them.

Words used below:
- **Quote**: a map from a term's syntax to a piece of data (a term that no longer runs) standing for it.
- **Self-interpreter / unquote**: a term that turns the quoted data back into the running term.
- **Normal form**: a term with nothing left to rewrite.
- **β-equal**: two terms that rewrite to the same thing.
- **The view**: the net's five-argument function built by one read (`ρ`).

## 1. What is a quote, why can no λ-term compute it, and what can be computed from a quoted term?

### Takeaway
A quote is done from outside the calculus: it maps a term to a normal form (data) that stands for it. No λ-term
can do it, because a λ-term cannot tell β-equal terms apart while their quotes must differ. The other direction
is definable: Kleene's self-interpreter turns the quote back into the term, and any computable function of the
quote is a λ-term.

### Cited Findings
- A running term is not data: it wants to rewrite. Barendregt on a term seen by a reduction machine: "It 'itches' at many places: all redexes want to be reduced" — [Barendregt 1991, p. 229](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/D16FE1FAB38CD06AD831C845156E10D5/S0956796800020062a.pdf/theoretical-pearls-self-interpretation-in-lambda-calculus.pdf#page=1)
- Barendregt's quote ⌜M⌝ is the Church numeral of M's Gödel number, and it does not run: "being a Church numeral it is in normal form" — [Barendregt 1991, p. 230](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/D16FE1FAB38CD06AD831C845156E10D5/S0956796800020062a.pdf/theoretical-pearls-self-interpretation-in-lambda-calculus.pdf#page=2)
- The quote is defined as a function on syntax, outside the calculus: "Define the quote to be the function" Q with Q(M) = ⌜M⌝ — [Barendregt 1991, p. 230](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/D16FE1FAB38CD06AD831C845156E10D5/S0956796800020062a.pdf/theoretical-pearls-self-interpretation-in-lambda-calculus.pdf#page=2)
- No λ-term computes it: "Such a self-quote does not exist, however". The proof: `I I` and `I` are β-equal, so a quoting term Q would give Q(I I) = Q(I); "Since numerals are in" normal form the two codes would be the same numeral; but `I I` and `I` "are different terms, and so have different codes" — [Barendregt 1991, p. 230](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/D16FE1FAB38CD06AD831C845156E10D5/S0956796800020062a.pdf/theoretical-pearls-self-interpretation-in-lambda-calculus.pdf#page=2)
- The reverse direction exists. Kleene "showed that there is a self-interpreter E for the lambda" calculus: E ⌜M⌝ = M for closed M (transcribed equation). It "cannot hold for open terms containing free variables" — [Barendregt 1991, p. 230](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/D16FE1FAB38CD06AD831C845156E10D5/S0956796800020062a.pdf/theoretical-pearls-self-interpretation-in-lambda-calculus.pdf#page=2)
- Kleene went through combinators to dodge binders: "Kleene avoided this problem by building up closed terms from combinators" — [Barendregt 1991, p. 231](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/D16FE1FAB38CD06AD831C845156E10D5/S0956796800020062a.pdf/theoretical-pearls-self-interpretation-in-lambda-calculus.pdf#page=3)
- "Enumerator" is Barendregt's name for a closed term E that reaches every closed term from some numeral. "The first construction of a self-interpreter is due to Kleene (1936)", and "Such an E is automatically an enumerator" — [Barendregt 1992, p. 233](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/F3145D4DE8AD9734951632E985EAF908/S0956796800000356a.pdf/theoretical-pearls-enumerators-of-lambda-terms-are-reducing.pdf#page=1)
- Anything computable about the quote is a λ-term. The self-interpreter itself is obtained this way: "By the representability of computable functions there is a term" E0 that does the case analysis on the code — [Barendregt 1991, p. 231](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/D16FE1FAB38CD06AD831C845156E10D5/S0956796800020062a.pdf/theoretical-pearls-self-interpretation-in-lambda-calculus.pdf#page=3)
- Why quoting is needed at all: the only way to look at a term "from within the lambda calculus is to apply it to one or more arguments" — [Mogensen 1992, p. 346](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=2)
- Two things are computed from a quote in Mogensen's paper. A self-interpreter: "E applied to the representation of M is equal to M itself". A self-reducer: R applied to the quote "reduces to the representation of M's normal form" — [Mogensen 1992, p. 346](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=2)
- Jay and Palsberg name the two jobs: "self-recognisers that recover the input program from a canonical representation" and self-enactors that run it — [Jay–Palsberg 2011, PDF p. 1](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=1). A self-recogniser is one "that can recognise a term from its quotation, by reversing the quotation process"; "a self-enactor must do actual evaluation" — [Jay–Palsberg 2011, PDF p. 2](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=2)
- A quote works by stopping evaluation: "quotation may add tags that block evaluation, which are then removed by unquote" — [Jay–Palsberg 2011, PDF p. 2](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=2)
- One thing not known to be computable from a higher-order quote in untyped λ-calculus is equality of two quotes: "For such representations, no term equal is known to exist" — [Jay–Palsberg 2011, PDF p. 2](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=2)
- Jay states the obstacle to a quote inside the calculus: "quotation cannot act on reducible terms, since reduction changes the nature of the syntax produced by quotation", and even on normal forms "there is no pure λ-term that can reveal the internal structure of closed normal λ-abstractions" in a uniform way — [Jay 2017, PDF p. 2](https://coalg.org/mfps-calco2017/mfps-papers/4-jay.pdf#page=2)
- His fix is the one the net also uses: identify programs with closed normal forms, "whose reduction begins only when they are applied to arguments" — [Jay 2017, PDF p. 2](https://coalg.org/mfps-calco2017/mfps-papers/4-jay.pdf#page=2)
- What a calculus loses when a primitive can read any term: Wand gives "a very simple model of a reflective facility based on the pure lambda-calculus" and shows its equivalence is trivial: "two terms in the language are contextually equivalent iff they are alpha-congruent" (equal only up to renaming bound variables) — [Wand 1998, abstract on the author's page](https://www.khoury.northeastern.edu/home/wand/pubs.html#Wand98)

### Inferences
- `ρ` is not a quote in Barendregt's sense. A quote takes a term that may still run and makes data of it. `ρ` reads a value whose body never ran. In the net the quoting is done by the evaluation rule, not by `ρ`: bodies are inert until the call, so every function is already its own quote. `DESIGN.md` has the actual quote too: a written-out opened function applied to anything, `(^d. p) u`, "is the code of `p`: a quote".
- In the literature's words `ρ` is an *intensional operator* (Jay's word for one that looks at how a value is built, not only at what it does). The view it returns is the case analysis on the code, the job Barendregt's E0 does on a numeral.
- Barendregt's contradiction does not hit the net directly: `ρ (I I)` runs `I I` first (call by value), so it reads `I`. But `ρ` does tell `λx. I I` from `λx. I`, which are β-equal if rewriting under a binder is allowed. So the net must not count rewriting inside a body as an equation. That is the trade Wand's theorem describes.

### Gaps
- Kleene's 1936 paper was not fetched; the account of it is Barendregt's.
- Wand's paper body was read only from PostScript (see "Not verified").

## 2. Mogensen's representation: how close is the view to one layer of it, is "Mogensen–Scott encoding, one layer at a time, made on demand" accurate, and what speeds did he report?

### Takeaway
One layer of Mogensen's quote and the net's view have the same shape: a function that takes one arm per kind of
node, calls the arm for its own kind with the parts, and keeps the binder as a real function. The name is fair,
with three caveats: Mogensen builds every layer up front and outside the calculus, he has three arms and drops
the unused ones, and "Mogensen–Scott" is a later label. He measured his self-interpreter at 35 times slower than
running directly, and his self-reducer at 50 times.

### Cited Findings
- The older numeral quote is huge: its size "grows (at least) exponentially in the size of the terms" — [Mogensen 1992, p. 347](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=3)
- Mogensen joins two ideas: "we will use a combination of higher order abstract syntax" and a way of writing data constructors as functions. He does not claim the second: it is "so well-known that the author doesn't know where it originated" — [Mogensen 1992, p. 347](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=3)
- The schema (transcribed from p. 348, under the sentence "Combining higher order abstract syntax and the coding of signatures"):
  ⌜x⌝ ≡ λabc. a x;  ⌜M N⌝ ≡ λabc. b ⌜M⌝ ⌜N⌝;  ⌜λx.M⌝ ≡ λabc. c (λx. ⌜M⌝) — [Mogensen 1992, p. 348](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=4)
- The binder stays a real λ: "binding of variables is handled meta-circularly by binding of variables" — [Mogensen 1992, p. 347](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=3)
- Each node is a case written as a function: the coding "allows a switch (case) on constructor names to be implemented" by plain application — [Mogensen 1992, p. 347](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=3)
- Size: "this representation is linear in the size of the represented" term, and "is roughly seven times the size of the original term" — [Mogensen 1992, p. 348](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=4)
- Cost of one look: "Operations like testing, decomposition and building of terms are quite efficient", a few β-steps each — [Mogensen 1992, p. 348](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=4)
- The self-interpreter (transcribed from p. 349): E ≡ Y λe. λm. m (λx. x) (λmn. (e m) (e n)) (λm. λv. e (m v)). Going under a binder is applying it: β-reduction of the binder "is used to perform substitution in the interpreter. Thus no environment is needed" — [Mogensen 1992, p. 348](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=4)
- What the real-λ binder costs: "you cannot inspect the bindings of variables", and "it is not possible to distinguish free and bound variables"; so "The reducer is restricted to closed" terms — [Mogensen 1992, p. 349](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=5)
- His self-reducer opens a binder on a marker: "the bound variable is given a value that builds an application node" when applied, and "any expression that does not reduce to an abstraction must evaluate to a function that builds an application node" — [Mogensen 1992, p. 350](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=6)
- Speed: the self-interpreter and self-reducer were "being about 35 and 50 times slower than direct execution" under call by need — [Mogensen 1992, p. 345](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=1)
- Setup: a call-by-need reducer written "in Chez Scheme (Dybvig, 1987) on a SPARC-station"; the one benchmark is Ackermann's function on the Church numeral 3; Table 1 gives the normalized times 1, 35, 50 — [Mogensen 1992, p. 351](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=7)
- His reading of the numbers: "The slowdown of the interpreter over direct execution is comparable to" interpreted against compiled languages, and "the reducer is not much slower than the interpreter" — [Mogensen 1992, p. 352](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=8)
- Later result: interpreting costs only a constant factor. "We show that linear time self-interpretation of the pure untyped lambda calculus is possible", shown "for reduction to weak head normal form under call-by-name, call-by-value and call-by-need", counting "number of beta-reductions, cost of a substitution-based implementation and cost of an environment-based" one — [Mogensen 2000, abstract on the university's page](https://researchprofiles.ku.dk/da/publications/linear-time-self-interpretation-of-the-pure-lambda-calculus-lectu/#main-content)
- The name in print: "By using a typed Mogensen-Scott encoding [24], we can represent STLC using only functions" (STLC is the simply typed λ-calculus). What it gives is a case: "given case functions for abstraction and application, each producing a result of type r" — [Brown–Palsberg 2017, PDF p. 3](http://web.cs.ucla.edu/~palsberg/paper/popl17-full.pdf#page=3)
- That paper's reference list cites a Wikipedia page for the name: "Mogensen-Scott encoding. https://en.wikipedia.org" — [Brown–Palsberg 2017, PDF p. 14](http://web.cs.ucla.edu/~palsberg/paper/popl17-full.pdf#page=14)
- "Scott" refers to Scott's numerals, where a number is its own case function. "The Scott numerals are distinguished from the Church numerals by" each variable being used at most once and a trivial predecessor. In a typed version the two directions of one layer are named `in` and `out`, and are said to "be the two halves of the isomorphism between M and N" — [Abadi–Cardelli–Plotkin 1993, PDF p. 1](https://lucacardelli.name/Papers/Notes/scott2.pdf#page=1)
- The other choice is a Church encoding, where a term is its own fold (a walk of the whole term) instead of a one-layer case: "We use a typed Higher-Order Abstract Syntax (HOAS) representation based on Church encodings" — [Brown–Palsberg 2016, PDF p. 7](http://web.cs.ucla.edu/~palsberg/paper/popl16-full.pdf#page=7)

### Inferences
- Side by side, for a function. Mogensen: `λabc. c (λx. ⌜M⌝)`. The net: `λk1…k5. k1 k2 k3 k4 k5 F`, F the opened function. Both pick the arm for "function" and hand it something that, applied to a value v, gives the body with v where the variable was. Applications match the same way (`λabc. b ⌜M⌝ ⌜N⌝` against `λk1…k5. k3 k1 k2 k4 k5 f a`).
- Differences:
  1. When and by whom. Mogensen's quote is built for the whole term, before running, by a function outside the calculus (about 7 times the term's size). The net builds one layer (17 agents for a function) when `ρ` meets the node; the parts handed over are raw code, and the next layer exists only if read.
  2. Arms. Three against five. The two extra kinds (opened function, `ρ`) exist because the net reads values of its own running calculus, not only quoted terms.
  3. Unused arms. Mogensen's node drops them for free. The net hands them to the chosen arm, and a program that drops them pays per agent.
  4. Variables. Mogensen wraps every variable use (`λabc. a x`) at quote time. The net's variable arm fires for a variable use holding a captured value.
  5. Children. In Mogensen's quote the children are already quoted. A Scott-style value is its own one-layer case, so the "view" of every node exists before any read. In the net the view is the `out` direction of the `in`/`out` pair, computed on demand.
- So "one layer of a Mogensen–Scott encoding, made on demand" names the view's shape correctly. It should carry the caveats above, and the note that the name is not Mogensen's: he credits the constructor coding to folklore, and the label appears later (Brown–Palsberg 2017, via Wikipedia; Wand 1998 uses "Mogensen-Scott coding", unverified).
- On speed. Mogensen's 35× was measured on a call-by-need reducer, which shares the interpreter's code instead of copying it. The net's self-interpreter costs about 80× per level, "nearly all of it copying and erasing the interpreter's own code" (`DESIGN.md`). The view is 21 of about 2,070 rewrites per read. So the gap to Mogensen's figure is the copying, not the view. Both are constant factors, which is what Mogensen 2000 says is achievable.

### Gaps
- The seconds in Mogensen's Table 1 did not survive text extraction (26.59 s and 38.35 s are legible for the two interpreted runs; the direct run is not). Only the normalized 1 / 35 / 50 are safe to cite.
- Mogensen 2000 was verified from its abstract only; the size of its constant factor is not known here.
- No source was found that states who first wrote the name "Mogensen–Scott".

## 3. Is there prior work on quoting lazily, one layer on demand, or exposing a term through a view / out / expose function?

### Takeaway
I found no paper that quotes a λ-term lazily or names a "lazy quote". The idea exists under other names: Jay's
factorisation operator splits an unquoted program into its parts one node at a time; Wadler's "views" and the
`out` half of a recursive type present a hidden value as a case; and work on codata treats a function as
something known only by its observations. The net's view is these three put together.

### Cited Findings
- Wadler's views are the source of the word: "A view allows any type to be viewed as a free data type" (free data type: one built from constructors that a pattern can match), meeting "the need to view one data type as if it were another". A view is a pair of functions, in and out, that "specify isomorphisms between data types" — [Wadler 1987, PDF p. 1](https://www.cs.tufts.edu/~nr/cs257/archive/views/wadler.pdf#page=1)
- Jay's SF-calculus reads programs without a quote. The usual route "begins by applying a meta-function quote which converts an arbitrary λ-term into a data structure"; instead SF-calculus can query any closed normal form "by using its operator F to reveal its internal structure" — [Jay 2016, PDF p. 3](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=3)
- The rules for F (transcribed from Fig. 1): `F O M N → M` when O is an operator; `F P M N → N P′ P″` when P is a compound of P′ and P″. One step looks at one node and hands its two parts to the second arm — [Jay 2016, PDF p. 3](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=3)
- Jay's slogan: "programs = closed normal forms = data structures", terms that are both functions and data, "ready for analysis and optimisation, and this without any need for quotation" — [Jay 2016, PDF p. 3](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=3)
- From the abstract: analysis happens in the calculus "without requiring any meta-level process of quotation to produce a data structure" — [Jay 2016, PDF p. 2](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=2). And: programs "are already data structures, there is no need for quotation" — [Jay 2016, PDF p. 15](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=15)
- To read a function, λSF turns it into combinators first, only when "this will not break any redexes in the body of the abstraction" — [Jay 2016, PDF p. 3](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=3). It goes a binder at a time: "only one abstraction is eliminated at a time" — [Jay 2016, PDF p. 5](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=5)
- The same operator in the typed combinator setting: F is able "to decompose compounds (e.g. closed normal forms) into their components" — [Jay–Palsberg 2011, PDF p. 1](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=1). They compare it to Lisp: "factorisation is a formalisation of the car and cdr of Lisp" — [Jay–Palsberg 2011, PDF p. 11](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=11)
- With such operators the quote itself becomes a term of the calculus: "Even the quotation function used to access program syntax can be defined and typed" — [Jay 2017, PDF p. 1](https://coalg.org/mfps-calco2017/mfps-papers/4-jay.pdf#page=1)
- Jay's reason for wanting this: "Hiding the body of a λ-abstraction supports the separation of concerns", but "we must be able to look under the λ to analyse a program" — [Jay 2017, PDF p. 14](https://coalg.org/mfps-calco2017/mfps-papers/4-jay.pdf#page=14)
- Observation as the dual of construction: finite data "is defined by constructors we define infinite data by observations" — [Abel–Pientka–Thibodeau–Setzer 2013, PDF p. 1](https://www.cse.chalmers.se/~abela/popl13.pdf#page=1). Observations are "experiments on blackbox infinite objects such as functions, streams and processes", and "Since a function is an infinite object, we define it by observation" (its one observation is being applied) — [Abel–Pientka–Thibodeau–Setzer 2013, PDF p. 2](https://www.cse.chalmers.se/~abela/popl13.pdf#page=2)
- The `out` function as one-layer exposure of a Scott-style value: `in` and `out` are said to "be the two halves of the isomorphism between M and N", where N is one layer of case over M — [Abadi–Cardelli–Plotkin 1993, PDF p. 1](https://lucacardelli.name/Papers/Notes/scott2.pdf#page=1)

### Inferences
- In pure λ-calculus a function has one observation, application (Mogensen's remark in section 1). `ρ` adds a second: look at the top node. The view is what that observation returns, written as a Scott-style case because the net has no constructor agents.
- The nearest prior design is Jay's F. Both read an unquoted, inert program one node per step and hand the parts on. Two differences. F takes its arms as arguments and fires in one step; `ρ` returns a five-argument function that then has to be run, which is the 20 extra rewrites measured (the notes' "a case agent that held the five arms would do the same in 1" is the F design). And λSF must turn a function into combinators before F can split it, a step that walks the body, because its variables are names in a term; in the net a binder is a node wired to its variable, so the read is one rewrite.
- Wadler's sense of "view" fits: a hidden value shown as if it were built from five constructors. Wadler's views have both directions. The net has only `out`: per `DESIGN.md`, "Programs build new code only by opening templates and splicing values into the holes", so there is no general `in` that builds a function node from parts.
- "Lazy quote" is a fair informal gloss, but I could not tie it to a source. The precise names are: one layer of a Mogensen–Scott encoding (shape), `out` / a view (direction), factorisation or an intensional operator (what the primitive does).

### Gaps
- No source found for quoting lazily or one layer on demand in λ-calculus. Two web searches plus the papers above; not exhaustive.
- Reflective Lisps (3-Lisp, Brown) and their "reify" were not covered here; Wand's paper says they have "even higher degrees of introspection" (unverified wording, see "Not verified").
- Jay and Given-Wilson's 2011 paper that introduces F was not fetched; F is cited from Jay 2016 and Jay–Palsberg 2011.

## 4. Higher-order abstract syntax and locally nameless open/close: is "open a function on a marker, close it again" the same operation, and what does opening cost?

### Takeaway
Applying the net's opened function to a marker is the standard operation: it is application in higher-order
abstract syntax, `instantiate` in McBride–McKinna, "variable opening" in Charguéraud, `unbind` in the Unbound
library. On first-order terms it walks the body every time. The net's "close" is not the standard closing: it
only puts an unapplied opened function back under its binder; it does not re-bind a marker inside arbitrary code.

### Cited Findings
- Higher-order abstract syntax (HOAS: a binder in the represented program is a real function in the host language): "Higher-order abstract syntax incorporates name binding information in a uniform and language generic way" — [Pfenning–Elliott 1988, PDF p. 1](https://www.cs.cmu.edu/~fp/papers/pldi88.pdf#page=1). In it "bound object language variables actually become variables in the typed lambda calculus" — [Pfenning–Elliott 1988, PDF p. 4](https://www.cs.cmu.edu/~fp/papers/pldi88.pdf#page=4)
- What HOAS buys: "we represent variables and abstractions meta-circularly", so substitution need not be written: "we inherit it from the host language implementation" — [Brown–Palsberg 2016, PDF p. 7](http://web.cs.ucla.edu/~palsberg/paper/popl16-full.pdf#page=7)
- Parametric HOAS (PHOAS) makes the marker explicit: "binders bind variables instead of terms", and the variables are injected "explicitly via the Var constructor". A program that wants to look inside picks what a variable is: the term "can be instantiated to any concrete choice of a variable type" — [Chlipala 2008, PDF p. 3](https://adam.chlipala.net/papers/PhoasICFP08/PhoasICFP08.pdf#page=3). The order is forced: "we must fix a variable type before we can deconstruct syntax recursively" — [Chlipala 2008, PDF p. 6](https://adam.chlipala.net/papers/PhoasICFP08/PhoasICFP08.pdf#page=6)
- Locally nameless (bound variables are numbers, free variables are names). Closing: `abstract` "turns a closed expression into a scope by turning me into B 0" (a scope is a body with one dangling variable) — [McBride–McKinna 2004, PDF p. 4](https://www.e-pig.org/downloads/notanum.pdf#page=4)
- Opening is its inverse: "so we can use instantiate (F me) to invert abstract me". Together they "enable us to navigate under binders and back out again". Both are written as recursive walks of the expression (transcribed: `letmeB` and `what'sB` recurse through applications and scopes; "Observe that the existing bound variables within" stay untouched) — [McBride–McKinna 2004, PDF p. 5](https://www.e-pig.org/downloads/notanum.pdf#page=5)
- Opening needs a marker: "navigating under a binder requires the supply of a Name" — [McBride–McKinna 2004, PDF p. 7](https://www.e-pig.org/downloads/notanum.pdf#page=7)
- Charguéraud's names: "Variable opening turns some bound variables into free variables". "It is used to investigate the body of an abstraction". The reader supplies the marker: "it is our responsibility to provide a fresh name x to open the abstraction". The result "is a copy of t in which all the bound variables referring to the outer" binder are replaced — [Charguéraud 2012, PDF p. 5](https://www.chargueraud.org/research/2009/ln/main.pdf#page=5)
- Cost of opening on terms: "The implementation of variable opening needs to traverse a term recursively" — [Charguéraud 2012, PDF p. 5](https://www.chargueraud.org/research/2009/ln/main.pdf#page=5)
- Closing is a second walk and the inverse: "The implementation of variable closing follows a pattern similar to" opening, and "Variable closing is effectively the inverse function of the variable opening operation" — [Charguéraud 2012, PDF p. 6](https://www.chargueraud.org/research/2009/ln/main.pdf#page=6)
- Opening on a term instead of a name is a call: "opening with a term u is the same as opening with a fresh variable x and then substituting u for x" — [Charguéraud 2012, PDF p. 11](https://www.chargueraud.org/research/2009/ln/main.pdf#page=11)
- As a library: a binder is abstract, and "the monadic unbind operation decomposes the binding" with a fresh name — [Weirich–Yorgey–Sheard 2011, PDF p. 2](https://www.engineering.upenn.edu/~sweirich/papers/icfp11.pdf#page=2). On `open`: "we may use it to open up a binder in order to recurse through its subcomponents" — [Weirich–Yorgey–Sheard 2011, PDF p. 5](https://www.engineering.upenn.edu/~sweirich/papers/icfp11.pdf#page=5)
- As a language feature: in FreshML binders "can be deconstructed by matching against patterns naming bound variables explicitly", which may have the effect of "swapping bound names with freshly generated ones" — [Shinwell–Pitts–Gabbay 2003, PDF p. 1](https://www.cl.cam.ac.uk/~amp12/papers/frepbm/frepbm.pdf#page=1)
- Mogensen's interpreter opens by application (section 2), and reports the HOAS limit: "you cannot inspect the bindings of variables" — [Mogensen 1992, p. 349](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=5)

### Inferences
- Name map. The net's step → the literature's name:
  - `R ⋈ lam` (function becomes opened function): taking the scope out of its binder. McBride–McKinna's `Scope`, PHOAS's function under `Abs`, Mogensen's `λx. ⌜M⌝` handed to arm c.
  - opened function applied to a marker (`O ⋈ Lam`): HOAS application; `instantiate (F me)`; variable opening; `unbind`; a FreshML pattern match.
  - the marker: a fresh name; PHOAS's chosen variable type; Mogensen's application-building value.
  - `R ⋈ Lam` (opened function closed again): putting the scope back under its binder. This is a relabel, not `abstract` / variable closing.
- The standard closing takes any code with a free name in it and binds that name, by walking the code. The net has no such step: once a marker is spliced in, nothing turns it back into a bound variable. That is the HOAS position (Mogensen: bindings cannot be inspected), not the locally nameless one.
- Cost. On first-order terms opening and closing each walk the body, once per binder crossed, so a full traversal of a deep term re-walks inner bodies. In HOAS opening is one call in the host, and the host pays for substitution. In the net it is one rewrite that joins two wires (measured), because the binder node is already wired to the variable's use; the body is not walked. If the variable is used several times the marker is copied to each use by the body's own sharing nodes, so the cost depends on the marker's size and the number of uses, not on the body's size.

### Gaps
- No source was found that measures open/close on a graph or net where variables are wires. The comparison above rests on Charguéraud's statement for terms and this repo's measurement for the net.
- Whether the net could offer a real `abstract` (re-binding a marker) was not looked at.

## 5. Reading a function without keeping its source: normalization by evaluation

### Takeaway
A function can be turned back into text with no source kept: apply it to a fresh variable, and read back what
comes out, guided by the type. What comes back is only the normal form. Two functions with the same normal form
give the same text, the function must have a type and must terminate, and the cost is that of running it.

### Cited Findings
- The problem: when a term of function type is evaluated, the procedure is not shown, only "a message that the result is some procedural object" — [Berger–Schwichtenberg 1991, PDF p. 1](https://www.math.lmu.de/~schwicht/papers/lics91/paper.pdf#page=1)
- The goal: "we do not need the procedure itself but a term in normal form evaluating to it", so define a function p→e (procedure to expression) "inverting the evaluation functional and returning normal forms only" — [Berger–Schwichtenberg 1991, PDF p. 1](https://www.math.lmu.de/~schwicht/papers/lics91/paper.pdf#page=1)
- The definition (transcribed from pp. 1–2): p→e at type ρ→σ of a is `λz. p→e_σ (a (mse_ρ z))`, and `mse_{ρ→σ} r b = mse_σ (r (p→e_ρ b))`. So a function is read by applying it to a fresh variable z. "But how can we find a fresh z?" Their first answer: "We simply used the SCHEME procedure gensym creating a new symbol" — [Berger–Schwichtenberg 1991, PDF p. 2](https://www.math.lmu.de/~schwicht/papers/lics91/paper.pdf#page=2)
- What comes back: norm(r) := p→e(|r|) "is the long β–normal form of r" (long: also η-expanded, every function written with all its arguments) — [Berger–Schwichtenberg 1991, PDF p. 4](https://www.math.lmu.de/~schwicht/papers/lics91/paper.pdf#page=4)
- It reads behaviour, not source: "If two λ–terms have the same value in some model" of the stated kind, "then they are provably equal in the" βη-calculus — [Berger–Schwichtenberg 1991, PDF p. 1](https://www.math.lmu.de/~schwicht/papers/lics91/paper.pdf#page=1)
- Cost is the cost of running: "this normalization procedure will be as efficient as the compiler is" — [Berger–Schwichtenberg 1991, PDF p. 10](https://www.math.lmu.de/~schwicht/papers/lics91/paper.pdf#page=10)
- Danvy's version, for compiled Scheme: "given a compiled normal form and its type, we can recover its text". It "is completely directed by type information". Programs "must be closed and monomorphically typable". Its "restriction to the simply typed lambda-calculus coincides with Berger and Schwichtenberg" and it "essentially achieves lambda-calculus normalization"; "Residualization also exhibits a normalization" effect — [Danvy 1996, PDF p. 1](https://tidsskrift.dk/daimipb/article/download/7022/5983/22798#page=1)
- Its limits in one sentence: "we can map a compiled program back into its text" under the restrictions that "this program terminates, and (2) it has a type" — [Danvy 1996, PDF p. 4](https://tidsskrift.dk/daimipb/article/download/7022/5983/22798#page=4)
- Mogensen saw his self-reducer as the untyped cousin. Berger–Schwichtenberg's procedure "is controlled by the type of the term in question, so it is not applicable to our problem", but "The idea of letting the underlying evaluation mechanism handle the reduction" is the same — [Mogensen 1992, p. 349](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/4727ED102FD8F5C5950EBBFE776E8C69/S0956796800000423a.pdf/efficient_selfinterpretation_in_lambda_calculus.pdf#page=5)
- Without types the output is not η-long: "Our nbe is not type directed, so it does not produce η-long normal forms" — [Brown–Palsberg 2017, PDF p. 10](http://web.cs.ucla.edu/~palsberg/paper/popl17-full.pdf#page=10)

### Inferences
- What can be read back this way: the normal form of what the function does, and nothing else. It cannot tell apart `λx. (λy. y) x` and `λx. x`, or any two programs that compute the same thing in different ways. It says nothing of the size, the sharing or the steps of the original. It fails to return if the function loops on the marker.
- The net's read and this one start with the same move, applying the function to a fresh thing. They differ in what happens next. In the net the body is inert, so the application hands back the body's source with the marker in it, in one rewrite. In normalization by evaluation the body runs on the marker, and the result is rebuilt from how the marker was used. The first reads the program, the second reads the behaviour.
- The net can get the second from the first (interpret the opened body with the marker as an unknown); the reverse is not possible.

### Gaps
- The typed-versus-untyped boundary was not studied further; one sentence from Brown–Palsberg 2017 is the only source here for the untyped variant.

## 6. Typed self-interpreters: representation used and reported cost

### Takeaway
Three designs, three representations: blocked combinators read by factorisation (Jay–Palsberg 2011), a
fold-style higher-order encoding (Brown–Palsberg 2016), and a one-layer-case Mogensen–Scott encoding
(Brown–Palsberg 2017). The one that evaluates step by step is the one with the one-layer case. None of the three
reports a running-time figure.

### Cited Findings
- Jay–Palsberg 2011. The language is a combinator calculus with a factorisation operator; "we add a constructor B to block evaluation" — [PDF p. 1](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=1). Quotation blocks each operator with B and keeps applications (transcribed). Then "quoted terms are always normal forms, whose internal structure can be examined by factorising" — [PDF p. 4](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=4)
- Jay–Palsberg on cost: a general remark, "The process of quotation tends to cause code expansion" — [PDF p. 2](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=2); and efficiency left as a question: "Does the calculus admit an efficient implementation" — [PDF p. 11](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=11)
- Brown–Palsberg 2016 (System Fω, a typed λ-calculus where every program terminates). Representation: typed HOAS "based on Church encodings" — [PDF p. 7](http://web.cs.ucla.edu/~palsberg/paper/popl16-full.pdf#page=7). The simplest version reads nothing: "the program representation is shallow and supports no other operations" than unquote — [PDF p. 1](http://web.cs.ucla.edu/~palsberg/paper/popl16-full.pdf#page=1); because "the polymorphic identity function is the only possible argument to a representation" — [PDF p. 4](http://web.cs.ucla.edu/~palsberg/paper/popl16-full.pdf#page=4)
- Their fuller version supports five programs over the quote, among them "an intensional predicate for testing whether a closed term is an abstraction or an application", "a size measure, and a normal-form checker" — [PDF p. 2](http://web.cs.ucla.edu/~palsberg/paper/popl16-full.pdf#page=2). They require that "a representation is required to be a normal form" — [PDF p. 12](http://web.cs.ucla.edu/~palsberg/paper/popl16-full.pdf#page=12)
- Brown–Palsberg 2016 on cost: tests of correctness only. "Each operation has been programmed, type checked, and tested" — [PDF p. 11](http://web.cs.ucla.edu/~palsberg/paper/popl16-full.pdf#page=11)
- Brown–Palsberg 2017. The calculus adds to Fω: it comes "with recursive types and intensional type functions and has decidable type checking". The new thing is a self-evaluator, which "evaluates the represented program and produces a representation of the result" — [PDF p. 1](http://web.cs.ucla.edu/~palsberg/paper/popl17-full.pdf#page=1)
- Representation: "we use a typed Mogensen-Scott encoding, though there are several important differences", together with PHOAS — [PDF p. 8](http://web.cs.ucla.edu/~palsberg/paper/popl17-full.pdf#page=8)
- Three evaluators: "one that evaluates terms to weak head normal form, one that performs a single step of left-most reduction", and a normalizer by evaluation — [PDF p. 2](http://web.cs.ucla.edu/~palsberg/paper/popl17-full.pdf#page=2)
- Brown–Palsberg 2017 on cost: again tests only. "we tested that our self-evaluators are self-applicable". The older whole-term programs had to be redone with a fold written on top: "They are defined as folds over the representation" — [PDF p. 11](http://web.cs.ucla.edu/~palsberg/paper/popl17-full.pdf#page=11)

### Inferences
- A fold-style (Church) quote is enough to unquote and to walk a whole term. Taking one evaluation step needs a one-layer case, which is the Mogensen–Scott style plus recursive types. The net's view is of the second kind, untyped.
- All three papers quote from outside the calculus. Of the sources read here only Jay 2017 defines the quote as a term inside it, and only by first making programs inert normal forms.
- Mogensen 1992 is the only source in these notes with a measured slowdown (35× and 50×).

### Gaps
- No time or step counts are reported in any of the three typed papers (searched each full text for timings).
- The 2016 and 2017 papers were read in the authors' full versions, not the published proceedings versions.

## Not verified

These could not be checked with `scripts/verify-source.sh` and are kept out of the findings.

- Wand 1998, body text. Read from the author's PostScript file (`https://www.khoury.northeastern.edu/home/wand/papers/fexprs.ps`) with a rough local text extraction; the script cannot read PostScript and Springer blocks the fetch. Wording as extracted (spacing repaired by hand): "The fexpr construct creates a procedure which, when called, reifies its unevaluated actual parameter as a λ-term in normal form using the Mogensen-Scott coding [9]." Also: "Our result extends an old observation of Albert Meyer [8] that eval and quote render contextual equivalence trivial", and that fully reflective languages such as 3-Lisp "have even higher degrees of introspection than our simple language". If right, this is (a) a 1998 use of the name "Mogensen-Scott" and (b) the closest prior primitive to `ρ`: a reader inside a λ-calculus that hands back a Mogensen–Scott coded term, though of a whole unevaluated argument, not one layer of a value. Only the abstract is verified (section 1).
- Mogensen 2000, "Linear-Time Self-Interpretation of the Pure Lambda Calculus" (DOI 10.1023/A:1010058213619, and the conference version 10.1007/3-540-46562-6_11): full text blocked by Springer. Only the abstract on the university's page is verified.
- Mogensen 1992, Table 1 seconds: the scanned table's digits garble in extraction.
- Formulas transcribed by hand and checked only through neighbouring prose: Mogensen's schema and E; Barendregt's E ⌜M⌝ = M; Berger–Schwichtenberg's p→e and mse; Jay's two F rules; Jay–Palsberg's quotation.
- Kleene 1936, "λ-definability and recursiveness" (Duke Math. J. 2): not fetched.
- Jay and Given-Wilson 2011, "A combinatory account of internal structure" (J. Symbolic Logic 76): not fetched.
- Wadler 1987 was verified from a scanned copy hosted on a Tufts course page, not the author's own page (his copy is PostScript only).
- Sources looked at and not used: Washburn–Weirich 2003 "Boxes go bananas" (fetched, no quote taken); Jacobs–Rutten's coalgebra tutorial (text extraction unreadable).
