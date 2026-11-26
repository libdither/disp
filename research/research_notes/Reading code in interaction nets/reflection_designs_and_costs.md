# Reflection designs outside the pure λ-calculus, what readable code costs, and cheap code access

How to read these notes. Every quote under "Cited Findings" was matched against the fetched source by
`scripts/verify-source.sh -q`; the link is the one the script printed. `p.N` is the PDF page index (what
`#page=N` opens), not the printed page number. Sources that could not be checked this way are only in
"Not verified" at the end. "The net" means the user's system as measured in `this_net_measured.md`: a
λ-calculus run as an interaction net, function bodies kept as inert code, `ρ` reads one node of any
function's code, `T` (thaw) turns code into running applications.

## 1. What is lost when any function's code can be read (Wand, Shutt, Mitchell, Taha)

### Takeaway
If a program can take apart every term, two terms can be swapped for each other only when they are the same
text (up to renaming of bound variables), so no rewrite of a readable term is invisible. Shutt's answer is
that this holds exactly for the things that can be taken apart, and nothing else: keep some things sealed
(closures, environments, running computations) and the theory of those stays rich.

### Cited Findings

Wand's result (the original is PostScript only, see "Not verified"; these are later primary sources stating it):

- Wand proved that in a λ-calculus with a quoting facility, contextual equivalence (two terms can replace
  each other in every program) is the same as α-equivalence (same text up to renaming). "Wand [1998] showed
  the following theorem" followed by "Theorem 2 (Wand)", which reads M ≅ N if and only if M ≡α N —
  [Kavvos 2017, p.20](https://arxiv.org/pdf/1712.09302#page=20)
- What counts as an observation, and how terms are quoted, in Wand's proof: "definition of observation is
  termination at a weak head normal form", and the quoting function "is a Scott-Mogensen encoding" (a
  λ-term that is a case over the kinds of node; the same shape as the net's five-argument view) —
  [Kavvos 2017, p.20](https://arxiv.org/pdf/1712.09302#page=20)
- Kavvos's reading of the consequence: "there can be no semantic study of functional programming languages
  that are so strongly reflective that they can internally define quoting." and "Such a language can
  internally distinguish any two terms" — [Kavvos 2017, p.20](https://arxiv.org/pdf/1712.09302#page=20)
- Wand's own conclusion, quoted word for word by Shutt: "To get non-trivial theories, one needs to find
  weaker reflection principles" — [Shutt 2010, p.355](https://dcreager.net/pdf/Shutt2010.pdf#page=355); and
  "we care not only when two terms have the same behavior" (but also what the behaviour is) —
  [Shutt 2010, p.356](https://dcreager.net/pdf/Shutt2010.pdf#page=356)

Shutt's account of why it happens (PhD thesis, WPI 2010; Kernel is his Lisp, vau-calculus its formal model;
a fexpr is a procedure that receives its operands unevaluated, as data):

- Looking inside a term means using it without running it first. "Object-examination, however, is
  inherently non-applicative"; a program can then tell two terms apart "even if they have identical
  behavior" — [Shutt 2010, p.33](https://dcreager.net/pdf/Shutt2010.pdf#page=33)
- With a quote operator, any term can sit where it is never run, so "only syntactically identical
  expressions can be equated" — [Shutt 2010, p.34](https://dcreager.net/pdf/Shutt2010.pdf#page=34)
- What "trivial" means: "An equational theory that contains only reflexive equations is trivial" —
  [Shutt 2010, p.186](https://dcreager.net/pdf/Shutt2010.pdf#page=186)
- Shutt's general form of Wand's result (his Proposition 15.1): take S to be the objects a fexpr can take
  apart; then the theory of "equivalence of objects in S is trivial" —
  [Shutt 2010, p.354](https://dcreager.net/pdf/Shutt2010.pdf#page=354)
- In Wand's system everything can be taken apart: "all operational equivalences between any computational
  entities whatsoever in the formal system are trivial", and the paper "might defensibly have been titled"
  The Theory of Reflection is Trivial — [Shutt 2010, p.355](https://dcreager.net/pdf/Shutt2010.pdf#page=355)
- Shutt on Wand's paper: "This paper is a useful elaboration of the basic difficulty caused by mixing fexprs
  with implicit evaluation in an equational theory" —
  [Shutt 2010, p.415](https://dcreager.net/pdf/Shutt2010.pdf#page=415)

What Shutt says stays non-trivial, and why:

- His fexprs are a different thing from Wand's: they "are a dramatically different facility", and Wand
  "assumes an isomorphism between source expressions and computational states" while Shutt does not —
  [Shutt 2010, p.354](https://dcreager.net/pdf/Shutt2010.pdf#page=354)
- Kernel seals some objects: "Fexprs here are unable to deconstruct either encapsulated objects" (he lists
  environments, compound operatives, i.e. user-defined fexprs/closures, and computational states) —
  [Shutt 2010, p.355](https://dcreager.net/pdf/Shutt2010.pdf#page=355)
- The condition for losing everything: "If the object language itself has no encapsulated objects" then all
  operational equivalences between objects are trivial —
  [Shutt 2010, p.355](https://dcreager.net/pdf/Shutt2010.pdf#page=355)
- Running computations ("active terms") are not source text, so equations about them survive: "even a fully
  reflective formal system can be modeled by a calculus with a nontrivial formal equational theory" —
  [Shutt 2010, p.356](https://dcreager.net/pdf/Shutt2010.pdf#page=356)
- The design fix is to mark what gets evaluated instead of marking what does not: "The trivialization could
  be prevented by introducing a notation into the calculus that guarantees certain designated subexpressions
  will be evaluated regardless of context" — [Shutt 2010, p.34](https://dcreager.net/pdf/Shutt2010.pdf#page=34)
- In Kernel an ordinary procedure "consists of a wrapper that induces evaluation of operands, around a fexpr
  that acts on the resulting arguments", which removes the clash "that would otherwise trivialize their
  equational theories" — [Shutt 2010, p.2](https://dcreager.net/pdf/Shutt2010.pdf#page=2)

The same loss seen from staged programming (MetaML, a language with code brackets and no way to look inside code):

- Inspection defined: a construct that lets a "program to inspect a piece of code, and possibly change its
  execution" by its structure — [Taha 1999, p.113](https://web.archive.org/web/20161018191554id_/https://www.cs.rice.edu/~taha/publications/thesis/thesis.pdf#page=113)
- Inspection and rewriting inside code cannot both be allowed. Taha's example is a construct that "tests a
  piece of code to see if it is an application"; if an optimizer may β-reduce inside the code, then "we can
  get the result false just as well as we can get the result true"; inspection "is in direct contention with
  allowing the" β-rule on code — [Taha 1999, p.113](https://web.archive.org/web/20161018191554id_/https://www.cs.rice.edu/~taha/publications/thesis/thesis.pdf#page=113)

Abstraction as the thing lost (Mitchell):

- Mitchell compares languages by translations "that preserve observational" equivalence, and concludes "no
  programming language with an abstraction mechanism can be reduced to a language without"; his example:
  "Since Lisp with user-defined special forms does not have an abstraction mechanism", it is not universal
  in his theory even though it computes every computable function — [Mitchell 1991, abstract posted by the
  author](https://seas.upenn.edu/~sweirich/types/archive/1991/msg00029.html)

### Inferences
- The net's `ρ` reads any function value, closures included. In Shutt's terms every function is
  "deconstructible", so by his Proposition 15.1 two functions are interchangeable only when their code (and
  the values captured in it) are the same. The net has no sealed function type.
- The net loses less than Wand's language. Wand's fexpr gets its operand unevaluated, so any term can be
  read. `ρ` gets a value: the argument is run first (call by value). Running applications cannot be read, so
  they are Shutt's "active terms", and equations about them survive (a call and its result are
  interchangeable wherever a value is expected). The theory is trivial on function bodies and non-trivial on
  running computations.
- Nothing the net's evaluator did is lost, because it never rewrote under a binder anyway. What is lost is
  for a compiler or optimizer: any rewrite of a function body can be seen by a program (Taha's `IsApp`
  example applies as is).
- By Mitchell's criterion the net has no information hiding: a caller holding `ρ` can read any callee. Hiding
  would need a sealed kind of value or a restriction on who may hold `ρ`.
- The same fact makes sharing safe: since code identity is already observable and bodies never change,
  storing one copy of identical code (hash-consing, section 6) changes nothing a program can observe.

### Gaps
- Wand's paper itself could not be machine-verified (PostScript only). His wording is in "Not verified".
- Mitchell's full paper (Science of Computer Programming 21, 1993) was not fetched; only his abstract.
- No source was found that works out the equational theory for reflection restricted to values under weak
  call-by-value evaluation with captured values inside the code. Shutt's chapter 15 is the closest.

## 2. Staged and modal designs: only marked code is readable

### Takeaway
These designs freeze a marked expression (`box E`, of type "code of A") and leave ordinary functions opaque,
so the rest of the language keeps its equations. The first systems could only build and run code; reading it
by pattern matching came later (Mœbius) and needs extra typing machinery.

### Cited Findings

Davies–Pfenning (JACM 2001), the modal type □A:

- The frozen unit is typed and abstract: □A "represents code of type A. This type remains abstract in the
  sense that we do not commit ourselves" to an implementation —
  [Davies–Pfenning, p.3](https://www.cs.cmu.edu/~fp/papers/jacm00.pdf#page=3)
- Only closed expressions are frozen: "we should be able to manipulate an arbitrary closed expression as
  code" — [Davies–Pfenning, p.3](https://www.cs.cmu.edu/~fp/papers/jacm00.pdf#page=3); "they only manipulate
  closed code during execution"; "the reason for only treating closed code is that we need to be able to
  evaluate code without danger of encountering unbound variables" —
  [Davies–Pfenning, p.47](https://www.cs.cmu.edu/~fp/papers/jacm00.pdf#page=47)
- Running: the elimination rule means "we execute the code of type A to obtain a value of type A" —
  [Davies–Pfenning, p.4](https://www.cs.cmu.edu/~fp/papers/jacm00.pdf#page=4). In the operational semantics
  "we represent code for E simply by box E", a value that is not evaluated inside —
  [Davies–Pfenning, p.16](https://www.cs.cmu.edu/~fp/papers/jacm00.pdf#page=16)
- No construct reads code, but the door is left open: "our intended implementation of code is intensional"
  (so that code can be optimized); a related system "provides a primitive recursion construct for examining
  terms" of type □A — [Davies–Pfenning, p.47](https://www.cs.cmu.edu/~fp/papers/jacm00.pdf#page=47)
- How it is implemented: boxed expressions "are compiled into generating extensions which emit" machine code
  — [Davies–Pfenning, p.19](https://www.cs.cmu.edu/~fp/papers/jacm00.pdf#page=19)

Contextual modal type theory (Nanevski–Pfenning–Pientka, TOCL 2008):

- Box is a quote: "the box constructor functions like a quotation operator" —
  [Nanevski et al., p.32](https://www.cs.cmu.edu/~fp/papers/tocl07.pdf#page=32)
- The implementation does not keep the source: a boxed value "is actually not maintained as a source
  expression, but compiled to a generator" that produces compiled code at run time —
  [Nanevski et al., p.32](https://www.cs.cmu.edu/~fp/papers/tocl07.pdf#page=32)
- The new part is code with free variables, typed with the context it needs: the calculus "relates contexts
  to intensional expressions and run-time code generation" in the presence of free variables —
  [Nanevski et al., p.2](https://www.cs.cmu.edu/~fp/papers/tocl07.pdf#page=2)

Mœbius (Jang–Gélineau–Monnier–Pientka, POPL 2022; arXiv long version):

- Code can be read, and it is code of the same language: "meta-programs can pattern match on code that
  represents another" program in the same language —
  [Jang et al., p.3](https://arxiv.org/pdf/2111.08099#page=3)
- Reading extends the binding form: `let box` now supports binding "but also inspecting this piece of code
  by pattern matching" — [Jang et al., p.16](https://arxiv.org/pdf/2111.08099#page=16)
- The representation is hidden: "We view code patterns as higher-order abstract syntax trees" (so the
  implementation may pick any encoding) — [Jang et al., p.16](https://arxiv.org/pdf/2111.08099#page=16)
- Open code is held as code plus a pending substitution: "closures consisting of a variable and a delayed
  substitution" — [Jang et al., p.2](https://arxiv.org/pdf/2111.08099#page=2)
- Because code is readable, its exact shape matters: generated code "containing an administrative redex,
  which is particularly undesirable" in Mœbius, "where the program can inspect the code" —
  [Jang et al., p.2](https://arxiv.org/pdf/2111.08099#page=2)

Contrast, a design with no marker at all (Jay's tree calculus):

- Every program is readable with no quote step: "one program can query the internal structure of another",
  without "the quotation of lambda-abstractions or serialisation of programs" —
  [Jay 2021, p.5](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=5)
- "In such a language, programs have a dual nature, as both functions and data structures" —
  [Jay 2021, p.16](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=16)

### Inferences
- Is the net quote-like? Partly. What matches a quote: a function body is frozen text that a program can
  read. What does not: (1) nothing marks the frozen part, every λ body is frozen by the evaluation order;
  (2) there is no operator that stops an arbitrary operand from being evaluated, `ρ` only reads values;
  (3) no whole-term conversion happens, a read looks at one node.
- Closest named designs: "every λ is a box" in the modal reading, with `T` as unbox-and-run and `ρ` as a
  one-level pattern match on code (Mœbius's case on code, depth one). On the Lisp side it is Shutt's
  explicit evaluation (data stays inert until something evaluates it), minus Kernel's sealing of closures.
  On the "no marker" side it is Jay's tree calculus restricted to function values.
- The net's "opened function applied to a value hands back its body with the value spliced in" is the same
  idea as Mœbius's code-plus-delayed-substitution closure. If spliced values were kept as a pending
  substitution next to shared code instead of being written into a fresh copy, the body would not need
  copying (see section 6).
- What the modal designs buy and the net does not have: an opaque function type `A → B` whose values can
  still be optimized freely, next to a readable type □A whose values cannot.
- Wand's and Kavvos's point about the Scott–Mogensen encoding places the net's five-argument view exactly:
  it is one layer of that encoding, built on demand, instead of the whole term encoded up front.

### Gaps
- None of these papers gives run-time cost figures for reading code. They state typing and semantics.
- Earlier work on inspecting code (Despeyroux–Pfenning–Schürmann 1997; Nanevski's names-and-necessity) was
  not fetched; it is only referred to through Davies–Pfenning's related-work section.

## 3. Reflective towers: what reify and reflect hand over, and how the cost of levels is removed

### Takeaway
In the tower designs, reify hands the program its interpreter's state (the call's text, the environment,
the continuation) and reflect installs such data as interpreter state again. The infinite tower is never
run: upper levels hold no information until used, or are replaced by one extra piece of state, or are
compiled away by staging.

### Cited Findings

Smith, 3-Lisp (POPL 1984; the scan's text layer is poor, so only short fragments could be matched):

- The tower is an idea, not a cost: "this apparently infinite architecture can be finitely implemented" —
  [Smith 1984, p.1](https://ics.uci.edu/~jajones/INF102-S18/readings/17_Smith84.pdf#page=1)
- One language at every level: "there is a single dialect (3-Lisp) all the way up" —
  [Smith 1984, p.9](https://ics.uci.edu/~jajones/INF102-S18/readings/17_Smith84.pdf#page=9)
- A reflective procedure receives three things: "the argument structure at the point of call", the
  environment and the continuation —
  [Smith 1984, p.10](https://ics.uci.edu/~jajones/INF102-S18/readings/17_Smith84.pdf#page=10)
- Why it is affordable: the "implied infinite regress is not problematic" and the "architecture can be
  efficiently realised" (only finitely many bottom levels hold any information) —
  [Smith 1984, p.8](https://ics.uci.edu/~jajones/INF102-S18/readings/17_Smith84.pdf#page=8); the
  reflective processor "is fully tail-recursive" —
  [Smith 1984, p.11](https://ics.uci.edu/~jajones/INF102-S18/readings/17_Smith84.pdf#page=11)

Friedman–Wand, "Reification: reflection without metaphysics" (Indiana TR 161, 1984; OCR text):

- The two words: making interpreter data available to the program, and letting the program change it. "We
  refer to these processes as reification and reflection" —
  [Friedman–Wand, p.2](https://web.archive.org/web/20250228134044id_/https://legacy.cs.indiana.edu/ftp/techreports/TR161.pdf#page=2)
- It is the fexpr idea widened: "not only the form and the environment, but also the continuation" are given
  to the procedure body — [Friedman–Wand, p.2](https://web.archive.org/web/20250228134044id_/https://legacy.cs.indiana.edu/ftp/techreports/TR161.pdf#page=2)
- What a reifying procedure gets: "what is passed is a list of three elements" (the unevaluated actual
  parameters, "a representation of the current environment", and of the current continuation) —
  [Friedman–Wand, p.3](https://web.archive.org/web/20250228134044id_/https://legacy.cs.indiana.edu/ftp/techreports/TR161.pdf#page=3)
- Calls must keep their text because "a function may seize, using a reifier, the text of its call" —
  [Friedman–Wand, p.3](https://web.archive.org/web/20250228134044id_/https://legacy.cs.indiana.edu/ftp/techreports/TR161.pdf#page=3)
- No tower is needed for this: the construct does not rely "on the notion of a tower of interpreters" —
  [Friedman–Wand, p.2](https://web.archive.org/web/20250228134044id_/https://legacy.cs.indiana.edu/ftp/techreports/TR161.pdf#page=2)

Wand–Friedman, "The mystery of the tower revealed" (Indiana TR 196, 1986; OCR text):

- The picture to be explained: "This gives rise to a picture of an infinite tower" of interpreters, each
  run by the one above — [Wand–Friedman, p.2](https://web.archive.org/web/20250408204535id_/https://legacy.cs.indiana.edu/ftp/techreports/TR196.pdf#page=2)
- The whole tower above is folded into one value, the metacontinuation, which "represents the state of the
  upper interpreter"; a reflective procedure's body runs where the interpreter "was called in the upper
  interpreter" — [Wand–Friedman, p.3](https://web.archive.org/web/20250408204535id_/https://legacy.cs.indiana.edu/ftp/techreports/TR196.pdf#page=3)
- Result: a component "that abstracts the state of the tower above the current interpreter" —
  [Wand–Friedman, p.11](https://web.archive.org/web/20250408204535id_/https://legacy.cs.indiana.edu/ftp/techreports/TR196.pdf#page=11);
  the account "does not employ reflection to explain reflection" —
  [Wand–Friedman, p.2](https://web.archive.org/web/20250408204535id_/https://legacy.cs.indiana.edu/ftp/techreports/TR196.pdf#page=2)

Amin–Rompf, "Collapsing towers of interpreters" (POPL 2018):

- The aim: "collapse this tower into a compiler that removes all interpretive overhead" —
  [Amin–Rompf, p.1](https://www.cs.purdue.edu/homes/rompf/papers/amin-popl18.pdf#page=1)
- The starting fact: staging an interpreter, "making it generate code whenever it would normally interpret
  an expression", gives a compiler — [Amin–Rompf, p.2](https://www.cs.purdue.edu/homes/rompf/papers/amin-popl18.pdf#page=2)
- The device: each interpreter is written so that "an evaluator either executes source code" or generates
  code — [Amin–Rompf, p.1](https://www.cs.purdue.edu/homes/rompf/papers/amin-popl18.pdf#page=1); "they are
  able to act either as an interpreter or as a translator", and the middle levels "act in a kind of
  pass-through mode" — [Amin–Rompf, p.3](https://www.cs.purdue.edu/homes/rompf/papers/amin-popl18.pdf#page=3)
- The base language "supports staging through a polymorphic Lift operator" —
  [Amin–Rompf, p.3](https://www.cs.purdue.edu/homes/rompf/papers/amin-popl18.pdf#page=3)
- Result: "we can collapse arbitrarily many levels of self-interpretation", also in a tower where "every
  aspect of the semantics can change dynamically" —
  [Amin–Rompf, p.1](https://www.cs.purdue.edu/homes/rompf/papers/amin-popl18.pdf#page=1)

### Inferences
- The towers reify interpreter state: call text, environment, continuation. The net's `ρ` reifies less: the
  code of a function value, one node per read, with captured values found at the variable nodes. It hands
  over no continuation and no separate environment object.
- The net's 80× per level for a self-interpreter is the "naive tower" cost these papers set out to remove.
  But the measured cause differs: the papers remove interpretive dispatch, while 97% of the net's cost is
  copying and erasing the interpreter's own unchanged code. Section 6's techniques attack that part; staging
  (Amin–Rompf) or a built-in "run this code" step (the net's `T`, compare Maude's descent functions in
  section 4) attack the dispatch part.
- Smith's finiteness argument (upper levels hold no information until used) is the same kind of saving as
  sharing unchanged code: do not materialize what is identical to what already exists.

### Gaps
- Smith's paper and the two Indiana reports are scans; longer sentences could not be matched, only the
  fragments quoted above.
- No cost measurements for 3-Lisp or Brown towers were found in these sources. Amin–Rompf report benchmarks
  but those numbers were not extracted.

## 4. Reflection in rewriting logic: Maude's META-LEVEL

### Takeaway
Maude is quote-like in the full sense: `upTerm` turns a term into data of a separate sort, and a universal
theory can run that data. Because simulating one step at the level above costs many steps, Maude ships
built-in "descent functions" (`metaReduce` and others) that drop back down and run the represented term
with the ordinary engine.

### Cited Findings
- The universal theory: "there is a finitely presented rewrite theory" U that can represent any finitely
  presented rewrite theory, itself included, as a term —
  [Maude manual §17.1](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-234001)
- An earlier paper by the same authors "specifies a finitely presented universal theory for" rewriting
  logic — [Clavel–Meseguer 1996, abstract](https://maude.cs.illinois.edu/papers/abstract/tcs4009.html)
- The cost stated: "a naive implementation of reflection can be computationally expensive" —
  [Maude manual ch.17](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-233002); "In a
  naive implementation, each step up the reflective tower comes at considerable computational cost", because
  "simulating a single step of rewriting at one level involves many rewriting steps one level up" —
  [Maude manual §17.1](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-234002)
- The remedy stated: compute at a higher level "only when this is strictly necessary"; the key part of U
  "has been efficiently implemented in the functional module META-LEVEL" —
  [Maude manual §17.1](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-234002)
- Descent functions: `metaReduce`, `metaApply`, `metaRewrite` and others are so called "since they allow us
  to descend levels in the reflective tower" —
  [Maude manual §17.1](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-234003); they are
  built in, and "provide useful and efficient ways of reducing metalevel computations to object-level ones"
  — [Maude manual §17.6](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-241003)
- `metaReduce` takes a represented module and a represented term and "returns the metarepresentation of the
  canonical form of" the term — [Maude manual §17.6.2](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-244006r4)
- Moving up: `upTerm` takes a term and "returns the metarepresentation of its canonical form" (so the term
  is first reduced, then represented; `downTerm` goes back) —
  [Maude manual §17.6.1](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-242077r7)
- The representation is ordinary data: "Terms are metarepresented as elements of the data type Term" —
  [Maude manual §17.2.2](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-23700017.2.2)
- Referring beats converting: with the `[_]` operator a module is not moved up, "it is just a way of
  referring to it, and therefore more efficient" —
  [Maude manual §17.6.1](https://maude.lcc.uma.es/maude-manual/maude-manualch17.html#x114-242066r4)
- The TCS paper exists as cited: "Reflection in conditional rewriting logic", Theoretical Computer Science
  285(2):245–288, 2002 — [Maude manual, bibliography](https://maude.lcc.uma.es/maude-manual/maude-manualli3.html#Xclavel-meseguer-tcs)

### Inferences
- Why descent functions are efficient, as far as the manual says: they are built into the engine and run the
  represented term at the object level, instead of rewriting with U's rules one level up. The manual gives
  no finer mechanism on this page.
- Mapping to the net: `T` (thaw) is a descent function (it runs code natively). `ρ` is a one-node `upTerm`
  with no conversion, since the code is already there. A program in the net pays the tower cost only if it
  re-implements evaluation with `ρ` instead of handing code to `T`.
- The manual's "referring to it, and therefore more efficient" is the same move as section 6: pass a
  reference to unchanged code instead of a converted copy.
- Unlike the net, Maude keeps levels apart by sort: an ordinary term cannot be read; only its
  representation (sort `Term`) can. `upTerm` reduces its argument first, so it shows a canonical form, not
  the text the user wrote.

### Gaps
- Clavel–Meseguer (TCS 2002) itself could not be fetched; the definition of the universal theory and of
  descent functions is taken from the Maude manual. The manual points to another paper (its reference [23])
  for the formal definition of a descent function; that paper was not fetched.
- The Maude manual site's TLS certificate could not be verified; the script fetched it insecurely.
- No measurements of Maude's metalevel overhead were found in the manual chapter.

## 5. Reflection in process calculi: the ρ-calculus (Meredith–Radestock)

### Takeaway
Quoting a process makes a name; dropping a name runs the process inside. Quoting is a constructor with no
stated cost; the price is paid when names are compared, because two names are equal when the processes
inside them are structurally the same, which has to be decided recursively.

### Cited Findings
(The 2005 paper is behind a publisher block; these are a later peer-reviewed paper on the calculus and a
paper co-written by Meredith.)

- Names are code: "names are quoted processes rather than generated by the" ν operator of the π-calculus —
  [Meredith–Stay–Drossopoulou 2013, p.3](https://arxiv.org/pdf/1307.7766#page=3)
- Quote: the lift construct "quotes the process P, thereby creating the name" and sends it —
  [Lybech 2022, p.2](https://arxiv.org/pdf/2209.02356#page=2)
- Drop: the drop construct "removes the quotes of the name to run the process within them" —
  [Lybech 2022, p.2](https://arxiv.org/pdf/2209.02356#page=2)
- What can be compared: names, by name equivalence, which holds "if the processes within quotes have the
  same structure (up to structural congruence)" — [Lybech 2022, p.3](https://arxiv.org/pdf/2209.02356#page=3)
- The comparison is needed at every communication: since names have structure, "we must be able to
  explicitly conclude the equivalence" of the two channel names —
  [Lybech 2022, p.3](https://arxiv.org/pdf/2209.02356#page=3)
- The same from Meredith: an equality on names is used "to judge when two names are equivalent" when
  deciding synchronization and substitution, and it is defined by recursion together with α-equivalence and
  structural equivalence — [Meredith–Stay–Drossopoulou 2013, p.4](https://arxiv.org/pdf/1307.7766#page=4)
- Quoted code is inert under substitution: "substitution does not recur into processes under quotes" —
  [Lybech 2022, p.3](https://arxiv.org/pdf/2209.02356#page=3)

### Inferences
- The ρ-calculus compares code by structure (same text up to reordering parallel parts and renaming), never
  by behaviour. That is the process-calculus form of Wand's result: what is quoted is equal only when it is
  the same text.
- Comparing two names costs a structural comparison of two process terms, recursively through nested quotes.
  With maximal sharing of quoted processes (hash-consing, section 6) this would be a pointer comparison, but
  only after choosing a canonical form for parallel composition; the sources do not discuss this.
- Difference from the net: ρ-calculus quotes are closed to substitution, while the net splices captured
  values into a body's variable nodes.

### Gaps
- Meredith–Radestock (ENTCS 141, 2005) could not be fetched, so nothing here is in their own 2005 wording.
- No source states an implementation cost for quote, drop, or name equivalence.

## 6. Constant-time code access in implementations

### Takeaway
All the standard techniques do the same thing: keep the unchanging code in one place and let a function
value be a small reference to it plus the values it captured. Copying or dropping a function then costs the
size of the reference and the captured values, not the size of the code. An interaction-net evaluator
(HVM2) already does this with reference nodes.

### Cited Findings

Closures (Landin's SECD machine; closure conversion):

- A function value is the λ-expression plus its environment: "bundle of information called a" closure —
  [Landin 1964, p.9](https://www.cs.cmu.edu/~crary/819-f09/Landin64.pdf#page=9), made of the λ-expression
  and "the environment relative to which it was evaluated" —
  [Landin 1964, p.10](https://www.cs.cmu.edu/~crary/819-f09/Landin64.pdf#page=10)
- Closure conversion makes this a program transformation: "Functions with free variables are replaced by
  code abstracted on an extra environment parameter"; a closure is "a data structure containing pure code
  and a representation of its environment" —
  [Minamide–Morrisett–Harper 1996, p.1](https://www.cs.cmu.edu/~rwh/papers/closures/popl96.pdf#page=1)
- One code sequence serves every instance: in the SECD machine the code has access to "an environment which
  contains values for each of the free variables", "thus allowing a single code sequence for each lambda
  abstraction" — [Peyton Jones 1987, p.234](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf#page=234)

Defunctionalization (Reynolds 1972, reprinted 1998; Danvy–Nielsen 2001):

- A function value is fixed by two things: "the function will be determined by two items of information"
  (the λ-expression and the values of its free variables); so it can be a record whose fields "should
  contain values of the global variables" of that λ-expression —
  [Reynolds 1972/1998, p.14](https://homepages.inf.ed.ac.uk/wadler/papers/papers-we-love/reynolds-definitional-interpreters-1998.pdf#page=14)
- These records are closures: "They are equivalent to the objects called FUNARG triplets in LISP and closures
  in the work of Landin" — [Reynolds 1972/1998, p.16](https://homepages.inf.ed.ac.uk/wadler/papers/papers-we-love/reynolds-definitional-interpreters-1998.pdf#page=16)
- The technique in one line: "a whole-program transformation from higher-order to first-order functional
  programs" — [Danvy–Nielsen 2001, p.3](https://www.brics.dk/RS/01/23/BRICS-RS-01-23.pdf#page=3)
- A function becomes a tag naming its λ, and calling becomes a case on the tag: "function types are replaced
  by an enumeration of the function abstractions in this program", "represented with a finite sum together
  with the corresponding injections and case dispatch" —
  [Danvy–Nielsen 2001, p.9](https://www.brics.dk/RS/01/23/BRICS-RS-01-23.pdf#page=9)

Lambda lifting and supercombinators (Johnsson 1985; Hughes 1982; described by Peyton Jones 1987 and Danvy–Schultz 2003):

- Lambda lifting turns free variables into extra parameters: "Lambda-lifting transforms a block-structured
  program into a set of recursive equations"; "Each equation carries extra parameters to account for the
  free variables" — [Danvy–Schultz 2003, p.3](https://www.brics.dk/RS/03/26/BRICS-RS-03-26.pdf#page=3)
- The result is functions with no free variables: "These special lambda abstractions are called
  supercombinators" — [Peyton Jones 1987, p.233](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf#page=233)
- Why that is cheap: "we can compile a fixed code sequence for them", and inner functions "hence do not need
  to be copied when instantiating the supercombinator body" —
  [Peyton Jones 1987, p.236](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf#page=236)
- What it replaces: walking the body node by node, where the instantiator "has to do a case analysis on the
  tag of the node" — [Peyton Jones 1987, p.233](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf#page=233)
- Hughes: with no environment at run time, a super-combinator can be represented "by its body and a count of
  its arguments" — [Hughes 1983, p.51](https://www.cs.ox.ac.uk/files/3309/PRG40.pdf#page=51)

Hash-consing (Filliâtre–Conchon 2006; origins Ershov 1958, Goto 1974):

- Definition: "Hash-consing is a technique to share values that are structurally equal"; the library ends up
  "thus safely ensuring maximal sharing" —
  [Filliâtre–Conchon 2006, p.1](https://usr.lmf.cnrs.fr/~jcf/ftp/publis/hash-consing2.pdf#page=1)
- How: keep a global table and "look for an existing equal value in this table every time we want to create
  a new value" — [Filliâtre–Conchon 2006, p.1](https://usr.lmf.cnrs.fr/~jcf/ftp/publis/hash-consing2.pdf#page=1)
- Equality becomes a pointer test: physical equality "can be substituted for structural equality" —
  [Filliâtre–Conchon 2006, p.1](https://usr.lmf.cnrs.fr/~jcf/ftp/publis/hash-consing2.pdf#page=1)
- Building a node is constant time once its parts are shared: "equality may safely use physical equality on
  sub-terms and thus runs in O(1)"; "The hashing function can also be improved to run in constant time" —
  [Filliâtre–Conchon 2006, p.3](https://usr.lmf.cnrs.fr/~jcf/ftp/publis/hash-consing2.pdf#page=3)
- Dropping: the table must not keep dead terms alive, so it holds weak pointers. "A weak pointer is
  precisely a reference that does not prevent the garbage collector" from reclaiming the value —
  [Filliâtre–Conchon 2006, p.3](https://usr.lmf.cnrs.fr/~jcf/ftp/publis/hash-consing2.pdf#page=3)
- Their running example is λ-terms with nameless variables: a type "for λ-terms with de Bruijn indices" —
  [Filliâtre–Conchon 2006, p.1](https://usr.lmf.cnrs.fr/~jcf/ftp/publis/hash-consing2.pdf#page=1)
- They credit the idea to Ershov, "On programming of arithmetic operations" (1958), and Goto, "Monocopy and
  associative algorithms in extended Lisp" (1974) —
  [Filliâtre–Conchon 2006, p.8](https://usr.lmf.cnrs.fr/~jcf/ftp/publis/hash-consing2.pdf#page=8)

Content-addressed code (Unison documentation):

- A definition's identity is the hash of its tree: "Each Unison definition is identified by a hash of its
  syntax tree" — [Unison, The big idea](https://www.unison-lang.org/docs/the-big-idea/#title)
- The hash is "512-bit SHA3 digest of a term or a type's internal structure, excluding all names" —
  [Unison, Hashes](https://www.unison-lang.org/docs/language-reference/hashes/#title)
- Names are removed before hashing: "all named arguments are replaced by positionally-numbered variable
  references", dependencies "are replaced by their hashes", and "names are just separately stored metadata"
  — [Unison, The big idea](https://www.unison-lang.org/docs/the-big-idea/#title)
- Code never changes under its address: "the contents of each address are forever unchanging" —
  [Unison, The big idea](https://www.unison-lang.org/docs/the-big-idea/#title)
- Code moves as references, fetched only when missing: "the sender ships the bytecode tree to the
  recipient"; "If it already has all the hashes, it can run the computation" —
  [Unison, The big idea](https://www.unison-lang.org/docs/the-big-idea/#simplifying-distributed-programming)

In an interaction-net evaluator (HVM2, Taelin 2024):

- A reference node stands for a whole closed net: "they represent an immutable net that is expanded in a
  single interaction" — [HVM2 paper, p.5](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=5)
- It exists to avoid node-by-node copying: "This operation is key to enable fast function application",
  since without it "one would need to use duplicator nodes for the same purpose, which brings considerable
  overhead" — [HVM2 paper, p.7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- Copying a reference copies one node: "the erase rule actually behaves as a copy operation" for a
  reference, cloning it to both ports; "This allows us to perform fast copy of" function pointers —
  [HVM2 paper, p.7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- The limit: a reference whose net contains duplicators is not copied as a pointer, "it instead is computed
  as a normal call operation" — [HVM2 paper, p.7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- "Cloning large networks is faster through the call interaction" than through node-by-node commutation —
  [HVM2 paper, p.7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)

### Inferences

Summary table. The "made of" column is from the sources above; the cost and "could read" columns are my
reading of what each representation implies, not statements from the sources.

| technique | a function value is made of | copy / drop costs | what a program could read from it |
|---|---|---|---|
| the net today | its whole code as agents, captured values written into the variable nodes | every agent of the code, each time | every node, one per `ρ` |
| closure (Landin; closure conversion) | pointer to shared closed code + environment | the environment only (one pointer if the environment is shared) | nothing in most languages; code and environment if a reader is added |
| defunctionalization | a tag naming the λ + the captured values | the tag and the captured values | which λ it is (the tag) and the captured values; the body through a table indexed by tag |
| lambda lifting / supercombinators | name of a top-level closed function + arguments so far | the name and the arguments | the body through the definition table |
| hash-consing | pointer to the one shared copy of each distinct node | one pointer; reclaiming is the collector's job | every node as before, plus equality of two pieces of code in one step |
| content addressing (Unison) | a hash naming a stored, unchanging definition | one hash | the stored tree, looked up by hash |
| reference node (HVM2) | one node naming a closed net in a static table | one node | not readable in HVM2; a reader could index the table |

- Which techniques would remove the net's copying and erasing cost. The measured 97% is copying and erasing
  code that never changes (each recursive call copies the whole recursive function; each read erases four
  unused arms agent by agent). Any representation where unchanged closed code is held by reference turns
  both into one step per reference:
  1. Reference nodes for closed code (HVM2's REF, the supercombinator idea). A closed function, such as the
     recursive `size` after its free variables are made parameters, becomes one node. Copy and erase of that
     node are one rewrite each. `T` expands it only when it is called; `ρ` reads a node by looking into the
     shared definition. This is the smallest change and already has an interaction-net precedent.
  2. Code plus captured values kept apart (closures, lambda lifting). The net writes captured values into
     the body, so a function that captured something is no longer the same code as its source. Keeping the
     body shared and the captured values in a separate small structure makes copy cost the number of
     captured values. The "opened function applied to a value" case then returns shared code plus a pending
     substitution (Mœbius's closure) instead of a spliced copy.
  3. Hash-consing of inert code, as the design notes already propose. It is the dynamic form of (1): also
     shares code built at run time, and gives reading programs a one-step code equality test. It needs a
     nameless canonical form for binders (de Bruijn indices or Unison's positional variables), because in
     the net variables are wires, and it needs a way to free unused entries (weak table or counts).
  4. Content addressing is (3) with the table kept across runs and machines. It adds nothing for the
     rewrite count inside one run.
- The unused arms of a read are a separate, smaller matter of the same kind: if the four arms are references
  to shared code, erasing them is four rewrites whatever their size.
- Defunctionalization explains the view: the five-argument function built at each read is a case dispatch
  written as a function. A case agent holding the five arms would do it in one rewrite (the measurements say
  21 → 1), but that is 1% of the cost, so it matters only after the copying is fixed.
- HVM2's caveat carries over: a reference can be copied as a pointer only if expanding it later gives the
  same result as copying its expansion. That holds for the net's inert code because bodies never rewrite
  before the call.
- Sharing does not change what programs can observe (section 1): code identity is already observable through
  `ρ`, and Wand's own remark (in "Not verified") is that an implementation may use any optimized form as long
  as the original source can be recovered for the reader.

### Gaps
- No source measures hash-consing or reference nodes inside a reflective interaction-net reducer; the saving
  for the net is an inference from its own measurements, not a cited number.
- The sources do not state copy and drop costs for closures directly; the table's cost column is inferred
  from what each value is made of.
- Johnsson 1985 and Hughes 1982 were not read in the original (see "Not verified"); lambda lifting and
  supercombinators are cited from Danvy–Schultz, Peyton Jones's book, and Hughes's 1983 thesis.
- Whether hash-consing interacts badly with linear use of code in the net (a linear program expects to own
  what it holds) was not found in any source.

## Not verified

Nothing below is used as a cited finding. Each item says why it could not be checked with
`scripts/verify-source.sh`.

- **Wand, "The Theory of Fexprs is Trivial", Lisp and Symbolic Computation 10(3), 1998, pp. 189–199.** The
  author's copy is PostScript only (https://www.ccs.neu.edu/home/wand/papers/fexprs.ps); the script cannot
  extract text from `.ps`, the Springer page (doi 10.1023/A:1007720632734) returns a bot challenge, and no
  open PDF was found. I read the `.ps` locally with a throwaway text extractor, so word spacing is my
  repair. Wording as read:
  - Abstract: "We provide a very simple model of a reflective facility based on the pure λ-calculus, and we
    show that its theory of contextual equivalence is trivial: two terms in the language are contextually
    equivalent iff they are α-congruent."
  - Mechanism: "The fexpr construct creates a procedure which, when called, reifies its unevaluated actual
    parameter as a λ-term in normal form using the Mogensen-Scott coding."
  - Scope: "Since all 'fully reflective' languages (e.g. 3-Lisp, Brown) have even higher degrees of
    introspection than our simple language, we conclude that these languages similarly have trivial
    equational theories."
  - Earlier observation: "Our result extends an old observation of Albert Meyer that eval and quote render
    contextual equivalence trivial".
  - For implementers: "the result means that there are no valid source-to-source optimizations for our
    reflective language. It does not mean there are no optimizations at all: one could still translate a
    term into some other optimizable formalism, so long as the translation allowed one to retrieve the
    original source code for use by fexpr."
  - Types: "Types add complexity to the system, but they do not change the result, so long as expressions
    are reifiable in a form that allows them to be tested for equality."
- **Clavel–Meseguer, "Reflection in conditional rewriting logic", TCS 285(2), 2002.** ScienceDirect blocks
  the fetch (403) and no open copy was found. Only its bibliographic entry in the Maude manual was verified.
- **Meredith–Radestock, "A reflective higher-order calculus", ENTCS 141(5), 2005.** ScienceDirect blocks the
  fetch (403); CORE search also returned 403. Section 5 rests on Lybech 2022 and Meredith–Stay–Drossopoulou
  2013 instead.
- **Johnsson, "Lambda lifting: transforming programs to recursive equations", FPCA 1985 (LNCS 201).**
  Springer only; no open copy found.
- **Hughes, "Super-combinators: a new implementation method for applicative languages", LFP 1982.** No open
  copy found. His 1983 Oxford thesis (PRG-40) was used; it is a scan with a poor text layer and only the one
  phrase quoted in section 6 could be matched.
- **Goto, "Monocopy and associative algorithms in an extended Lisp", TR 74-03, Univ. of Tokyo, 1974.** The
  file resolves (https://www.cs.utexas.edu/~hunt/research/hash-cons/hash-cons-papers/monocopy-goto.pdf#page=2,
  phrase "In monocopy lisp" matched) but the OCR text is too broken to quote any claim from.
  **Ershov, "On programming of arithmetic operations", CACM 1(8), 1958** was not fetched. Both are cited
  only through Filliâtre–Conchon's reference list.
- **Taha, "A sound reduction semantics for untyped CBN multi-stage computation. Or, the theory of MetaML is
  non-trivial", PEPM 2000.** The archived copy
  (https://web.archive.org/web/20170813015418/https://www.cs.rice.edu/~taha/publications/conference/pepm00.pdf)
  has an unreadable text layer; only the title could be read. Its section "Intensional Analysis Conflicts
  with β" appears to be the same material as the thesis section quoted in section 1.
- **Mitchell, "On abstraction and the expressive power of programming languages", Science of Computer
  Programming 21, 1993.** Full paper not found open. Section 1 quotes the abstract from the author's own 1991
  announcement of the preliminary (TACS 1991) version.
- **Smith's sentences in full.** The scan's text is letter-spaced; the fuller sentences as I read them (not
  matched by the script): 3-Lisp is "an infinite tower of procedural self-models"; the regress is harmless
  "since only a finite amount of information is encoded in all but a finite number of the bottom levels";
  the reflective processor "can be executed by a simple finite state machine".
- **How Maude's descent functions are implemented inside the engine** (C++ built-ins, any caching of
  represented modules): not stated in the manual chapter read; no source checked.
