# Trees, "no quotation", and bracket abstraction

Scope: Barry Jay's calculi that read their own programs (SF-calculus, tree calculus, λSF-calculus, pattern
calculus), and bracket abstraction, the compile step that turns a λ-term into a variable-free term.
Every quote below was found in the linked source by `scripts/verify-source.sh`; the link is the one the
script printed. Unverified items are only in the last section.

Names used:
- Normal form: a term with no rewrite left to do.
- Combinator: a term built only from fixed operators (S, K, F, △) and application. No variables, no binders.
- Bracket abstraction `[x]t`: an algorithm, run outside the calculus, that turns a term `t` with a free variable `x`
  into a combinator that behaves like `λx.t`. Star abstraction `λ*x.t` is the same with shortcuts.
- Quote: a step that turns a program into a data structure describing it (its syntax tree).
- Intensional: able to tell apart two programs that compute the same function. Extensional: not able to.

## 1. What do Jay and co-authors claim about quotation? What can be read? What do F and triage do in one rewrite?

### Takeaway
The claim is narrow: a program that is already a normal form can be taken apart by another program of the same
calculus, so no outside quote step is needed. Terms that can still run cannot be read, and Jay's own lazy
self-evaluator quotes them. One rewrite of F, or of the fork rule of tree calculus, looks at one node and hands
over its two parts.

### Cited Findings

The claim, in Jay's words:
- Tree calculus gets self-evaluation "without any of the usual outside machinery, such as the Gödel numbering of
  Turing machines, the quotation of lambda-abstractions or serialisation of programs" — [Jay, *Reflective Programs in Tree Calculus* (2021), preface](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=5)
- "extra machinery such as Gödel numbers or quotation is no longer necessary" — [tree book, preface](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=5)
- On older self-interpreters: "Stability was imposed by first quoting the program to produce a data structure".
  In tree calculus "the programs are already data structures, so that no pre-processing is required", so
  "tree calculus supports honest reflection without make-up" — [tree book, p. 6 (PDF page 18)](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=18)
- On what a quote does: "Traditionally, self-interpreters were allowed to act on the syntax tree of a program, i.e. its
  quotation." Quotation "performs all of the work necessary to discover program structure" and "performs all of the
  intensional computation"; when it lies outside the calculus, "some sort of staging is required" — [tree book, §6.1](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=76)
- For λ-calculus specifically: "quotation is required to convert abstractions into trees". Tree calculus avoids the
  conversion: "the trees will be themselves executables, will be functions as well as data structures" — [tree book, ch. 3](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=36)
- The 2019 survey abstract: in λ-calculus and combinatory logic "it has been necessary to encode or quote the program
  before analysis", but intensional calculi "can analyse higher-order programs within the calculus proper, without
  quotation" — [Jay, "Intensional computation with higher-order functions", TCS 768 (2019), abstract on the UTS record](https://opus.lib.uts.edu.au/handle/10453/135540#s3)
- λSF-calculus (λ-calculus plus S and F): analysis is done "without requiring any meta-level process of quotation to
  produce a data structure" — [Jay, "Programs as Data Structures in λSF-Calculus", ENTCS 325 (2016), abstract](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=2).
  The slogan there is "programs = closed normal forms = data structures" — [same paper, §1](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=3)
- Why S and K alone cannot do it: "there are Turing-computable functions on the combinators themselves that cannot be
  so represented"; "the factoring of a combinator into its constituent operators, is a metamathematical operation" — [Jay & Given-Wilson, "A combinatory account of internal structure", JSL 76(3) (2011), p. 1](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=1)
- Theorem 3.2 there: "Factorisation of SK-combinators is a symbolic computation that is not representable" — [JSL 2011, p. 7](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=7).
  A Turing machine can do it only after someone has written the combinator on the tape: "this expression of
  factorisation does not imply its representation as a combinator" — [JSL 2011, p. 7](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=7)

What can be read (normal forms only):
- SF-calculus "supports all pattern-matching functions whose patterns are in normal form", and such calculi
  "represent all their Turing-computable functions whose domain is limited to normal forms" — [JSL 2011, abstract](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=1)
- "since all normal forms are partially applied operators, F can be used to examine their internal structure" — [JSL 2011, p. 2](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=2)
- F reads only "factorable forms", the partially applied operators: "where the factorable forms are the combinators
  of the form" S, SM, SMN, F, FM, FMN; "their stability ensures that reduction is confluent" — [JSL 2011, p. 1](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=1)
- A term that never settles cannot be read: Ω = (SII)(SII) "does not reduce to a factorable form" — [JSL 2011, §5](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=10)
- An outside reader states the same restriction: "The trick by which disaster is avoided is that of having" (page
  break) "intensional operations act only on normal forms" — [Kavvos, "On the Semantics of Intensionality and Intensional Recursion" (2017), §9.2](https://arxiv.org/pdf/1712.09302#page=198), [continued](https://arxiv.org/pdf/1712.09302#page=199)
- Why the restriction is needed: adding a factorisation operator to λ-calculus without it fails. "Consequently,
  adding this operator breaks confluence" — [Lester, "Control Flow Analysis for SF Combinator Calculus" (2015), p. 6](https://arxiv.org/pdf/1512.03861#page=6)
- Tree calculus: "define the factorable forms to be the terms which are leaves, stems or forks"; a tree whose nodes
  are all factorable is a program — [tree book, §6.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=76)

Jay still uses a quote for terms that are not normal forms:
- The lazy ("root") evaluator must hold unevaluated terms, so they "will be quoted": "root evaluation maps a quotation
  to a factorable form of quotations". "quotation of computations is not definable within tree calculus" — [tree book, §6.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=76)
- "quotation of arbitrary computations is not definable as a program" because "quotation does not preserve
  evaluation; that is, after all, the point". The quote is a tree "which represents arbitrary computations by binary
  trees that have no stems" — [tree book, §6.5](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=80)
- Quoting a program (a normal form) is a program: it "is definable by the program quote", so "In this manner, no
  meta-theory is required" — [tree book, §6.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=77)

What one rewrite does:
- F (SF-calculus), rules as printed: `F O M N → M` if O is S or F; `F (P Q) M N → N P Q` if `P Q` is a factorable form — [JSL 2011, p. 1](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=1).
  In words: "if x is an atom (i.e. an operator) then use y", else hand the two parts of the compound to z — [tree book, §10.1](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=126).
  F only splits: "reduction of F does not allow for duplication of any kind" — [tree book, §10.1](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=126)
- Tree calculus (2021 book) has three rules: `△△yz = y` (K), `△(△x)yz = yz(xz)` (S), `△(△wx)yz = zwx` (F). The
  third is the read: "(F) decomposes the first argument to expose its branches" — [tree book, ch. 3](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=40)
- In the 2021 book triage (the leaf / stem / fork test) is a program built from those rules, not a rule: "This sort of
  testing can be packaged up using triage" — [tree book, §5.7](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=72); "triage performs three-way case analysis on binary trees" — [tree book, glossary](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=174)
- Pattern calculus (Jay's earlier calculus with first-class patterns) reads data but not functions: "it is unable to
  analyse pattern-matching functions themselves". SF-calculus reads any closed normal form, but "it does not provide
  first-class support for λ-abstraction or any other mechanism for binding variables" — [λSF paper, §1](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=3)

### Inferences
- "No quotation" means: no step outside the calculus between the runnable program and the readable program, for
  programs that are normal forms. It does not mean "any term can be read". A running term is still opaque.
- The user's net has the same shape: a function body is inert code, already a normal form, so reading it needs no
  quote. The local measurements say the same ("No whole-term quote happens").
- What the user saw as "quote-like" is the view: a five-way choice written as a five-argument function, built at
  every read (17 agents, 21 rewrites, per the local measurement file). Tree calculus and SF-calculus do not build
  one: the choice is the rewrite rule itself (F picks one of two arms, the tree rules pick by the shape of the
  first argument). So on this one point trees do better, but the reason is "the case is a primitive", not "the
  program is a tree". The local measurement already notes a case agent would do the same in 1 rewrite.
- One read looks at one node in both systems. F hands the two parts to the chosen arm and drops the other arm; the
  user's reader hands over the parts and the arms not chosen.

### Gaps
- The later variant where triage is itself a rewrite rule (used on treecalcul.us and in Jay's "Typed Program Analysis
  without Encodings", PEPM 2025) was not fetched; no verified source for those rules here.
- The full text of the TCS 2019 paper is closed on the UTS repository (only the abstract was verified).

## 2. When a tree-calculus program reads a function that was written with λ, what does it see?

### Takeaway
It sees the combinator that bracket abstraction produced, not the binder. λ is notation for a compile step done
outside the calculus. The step keeps the function's behaviour but not its text: β-equal λ-terms compile to
different trees, and after compiling a binder cannot be told from a hand-written combinator unless tags were added.

### Cited Findings
- Definition: "the bracket abstraction [x]t represents the function that, when applied to x yields t", with four
  clauses (`[x]x = I`, `[x]y = Ky`, `[x]△ = K△`, `[x]uv = d{[x]v}([x]u)`, where d is an S-like tree) — [tree book, §4.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=53)
- λ in the SF papers is the same notation: "Where no confusion is likely, we may write λx.M for λ∗x.M" — [JSL 2011, p. 6](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=6)
- The result is always inert: "A curious property of bracket abstractions is that they are always stable" — [tree book, §4.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=54)
- It changes what a reader can see: "bracket abstraction does not preserve the equality induced by the evaluation
  rules. In other words, it does not preserve the meaning of terms." (Example there: `[x]K x x` does not reduce to
  `[x]x`.) — [tree book, §4.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=54)
- It is not a term former of the calculus: "bracket abstraction is not a “first-class” means of constructing terms, as
  these should preserve the equality based on the evaluation rules"; "no set of additional rules has been found that
  will support arbitrary evaluation under bracket abstraction" — [tree book, §4.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=54)
- Star abstraction is the optimised version: "optimise the bracket abstraction to exploit variable occurrence"; "it is
  safe to treat star abstraction as an independent concept"; "Unlike bracket abstractions, star abstractions may
  require evaluation" — [tree book, §4.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=55)
- The binder is lost: conversions to combinators "lose intensional information, in that an abstraction λx.M becomes
  indistinguishable from a star abstraction λ∗x.M or combinator" — [λSF paper, §9](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=12).
  Keeping it takes tags: the conversion "can be made intensional, too, by adding tags to record the presence of
  abstractions and combinators" — [λSF paper, §9](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=13)
- A calculus with real binders (VA-calculus, Jay's λ-calculus variant in the book) is weaker at reading: "VA-calculus
  is not complete as it cannot define program equality" — [tree book, ch. 9](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=113).
  Even there Jay reuses bracket abstraction: "It may seem strange to do this within a λ-calculus but it will prove
  convenient" — [tree book, §9.3](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=118)
- The 2026 paper says the same of plain combinatory logic: bracket abstractions "are always normal forms; and they are
  quite verbose"; because programs are normal forms "there is no need to encode them as syntax trees before
  analysis" — [Jay & Bader, "Simple Types for Polymorphic Functions" (2026), p. 4](https://arxiv.org/pdf/2604.12194#page=4), [p. 2](https://arxiv.org/pdf/2604.12194#page=2)

### Inferences
- In Jay's calculi the work a quote would do for a λ-term is split in two. Bracket abstraction, at compile time and
  outside the calculus, removes the binders and makes the program a normal form. F or the tree rules, at run time and
  inside the calculus, then read that normal form one node at a time. The first half is the part "no quotation" does
  not mention.
- The user's net keeps the binder: reading a function opens it and hands back the body as code with the variable
  still a wire. A tree program cannot get that back; it gets routing combinators (S, K, and their tree forms).
- So the two designs differ in what is read. Trees: the compiled form, cheap to read, binder gone. The net: the
  source form, binder kept, read through a built view.

### Gaps
- No statement was found, by Jay or others, that calls bracket abstraction itself a "quote". The link is drawn here
  from the quotes above.

## 3. What does bracket abstraction cost?

### Takeaway
With a fixed, finite set of combinators the output cannot be linear: the proven lower bound is n log n, the plain
S/K/I algorithm is cubic in the worst case, and Turner's is quadratic. Linear size is claimed only with an unbounded
family of "bulk" combinators and a size measure that counts variables in unary.

### Cited Findings
- Jay's own figures for the naive algorithm in tree calculus: "However, [x]M is much bigger than M. If M is a
  combinator of size k ... then [x]M has size 5k − 2"; `[x]I` "has size twenty-three instead of K I whose size is
  seven"; "This five-fold expansion is especially problematic when abstractions are nested" — [tree book, §4.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=54)
- The fix needs variable bookkeeping: "it is necessary to keep track of variable occurrences in terms" — [tree book, §4.2](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=54)
- Jay & Palsberg on the η shortcut (`λ*x.t x = t`): it "makes a big difference in the size of the resulting term" — [Jay & Palsberg, "Typed Self-Interpretation by Pattern Matching", ICFP 2011, §4.1](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=4)
- Plain S/K/I algorithm with the "K for a term without x" shortcut (called abf): it "is perhaps the most widely known
  and also one of the simplest bracket abstraction algorithms, but it is not particularly efficient". Without that
  shortcut "the translation size may be exponential", with it O(n³) — [Czajka, "On the equivalence of different presentations of Turner's bracket abstraction algorithm" (2015), p. 3](https://arxiv.org/pdf/1510.03794#page=3)
- The cubic bound is tight: "the asymptotic growth rate of the size of a translated term is Θ(n³) in worst-case"
  (the page prints Θ as "Ø" and the exponent inline) — [Lachowski, "On the Complexity of the Standard Translation of Lambda Calculus into Combinatory Logic", Reports on Mathematical Logic 53 (2018), abstract](https://ejournals.eu/en/journal/reports-on-mathematical-logic/article/on-the-complexity-of-the-standard-translation-of-lambda-calculus-into-combinatory-logic#panel-abstract-en).
  Restated: the naive translation "suffers from a size increase of" Θ(n³) — [Lester, "Understanding the Expressive Power of Unhygienic Substitution in Metaprogramming via Combinatory Logic" (2019), p. 7](https://arxiv.org/pdf/1910.03217#page=7)
- Lower bound for any fixed finite set of combinators: Ω(n log n) "is a lower bound on the translation size", and
  "This bound is attained in [11]" (Kennaway & Sleep 1987; the bound is credited to Joy, Rayward-Smith & Burton 1985) — [Czajka 2015, p. 3](https://arxiv.org/pdf/1510.03794#page=3)
- Schönfinkel's algorithm (adds B and C): "the translation size for S is also" O(n³), with a smaller constant — [Czajka 2015, p. 3](https://arxiv.org/pdf/1510.03794#page=3)
- Turner's algorithm (adds S', B', C'): "The translation size of T is worst-case" O(n²) and average-case O(n^(3/2))
  (credited to Joy et al. 1985 and Hikita 1984) — [Czajka 2015, p. 5](https://arxiv.org/pdf/1510.03794#page=5)
- Turner in 1979 on the plain algorithm: "it tends to produce rather long-winded" code — [Turner, "A new implementation technique for applicative languages", Software: Practice and Experience 9 (1979), p. 34](https://courses.grainger.illinois.edu/cs522/sp2016/ANewImplementationTechniqueForApplicativeLanguages.pdf#page=4).
  His compiler could "get a dozen or so levels deep in abstraction operations" — [p. 38](https://courses.grainger.illinois.edu/cs522/sp2016/ANewImplementationTechniqueForApplicativeLanguages.pdf#page=8),
  and the extra optimisations for nested abstraction "proved essential to maintaining this degree of compactness" — [p. 44](https://courses.grainger.illinois.edu/cs522/sp2016/ANewImplementationTechniqueForApplicativeLanguages.pdf#page=14)
- Abstraction is not compositional: "It must be applied to each lambda in sequence, starting with the innermost. The
  abstraction of outer lambdas modifies the terms generated during translation of inner lambdas" — [Lester 2019, p. 3](https://arxiv.org/pdf/1910.03217#page=3)
- Kiselyov: in earlier approaches "the size of the translation result has to be, in the worst case, at least quadratic
  in the size of the input term", seen on λx1…λxn. xn…x1, because "Distributing one environment variable across an
  application requires one combinator" — [Kiselyov, "λ to SKI, Semantically", FLOPS 2018, §6](https://okmij.org/ftp/tagless-final/ski.pdf#page=14)
- Kiselyov's linear result: "its output and the running time can both be made linear in the size (i.e., the number of
  all constructors) of the input De Bruijn-indexed term" — [Kiselyov 2018, abstract](https://okmij.org/ftp/tagless-final/ski.pdf#page=1).
  It rests on bulk combinators Bn, Cn, Sn, one for every n: "The bulk combinators distribute several variables in
  bulk"; the count holds "If we regard Bn, Cn and Sn as pre-computed" — [§6](https://okmij.org/ftp/tagless-final/ski.pdf#page=14);
  "we take each bulk combinator reference ... to take constant space" — [§7](https://okmij.org/ftp/tagless-final/ski.pdf#page=15).
  Measured: the output "stays within 1.5× the size of the input term" — [§6](https://okmij.org/ftp/tagless-final/ski.pdf#page=14)
- The caveat on that result: "the increase in term size depends on the metric used for the size of the lambda term";
  Kiselyov's choice to count the index sⁿz as size n + 1 "inflates the size of the lambda term, allowing him to claim
  an" O(n) translation — [Lester 2019, p. 7](https://arxiv.org/pdf/1910.03217#page=7)
- Other bounds, as Kiselyov reports them: SKI translation size and time "are worst-case quadratic in the size n of
  the" input; "Hughes supercombinator process has the worst-case size complexity of O(n log n), but is often linear";
  "Noshita proves the O(n log n) upper-bound on the translation size" — [Kiselyov 2018, §7](https://okmij.org/ftp/tagless-final/ski.pdf#page=15)
- Director strings (Kennaway & Sleep), as described by Kiselyov: they "tell for each application node which parameters
  should go left or right or both ways" — [Kiselyov 2018, §7](https://okmij.org/ftp/tagless-final/ski.pdf#page=15).
  Same idea for the plain combinators: "Intuitively, B, C and S serve as directors" — [Lester 2019, p. 3](https://arxiv.org/pdf/1910.03217#page=3)

### Inferences
- The cost is a routing cost. A tree has no pointer from a binder to its uses, so every application node on the way
  from the binder to a use carries a routing combinator (left, right, both). n nested binders over a body of n
  applications gives about n² routing nodes; the best algorithms for a fixed combinator set reach n log n; only an
  unbounded combinator family removes the factor.
- This matches the project's own table (local `DESIGN.md`, "Why trees blow up"): for λx1…xn. k xn…x1 at n = 16 the
  term and the net have 50 nodes, the 9-rule tree 1,134 and the tree-calculus tree 6,533; random 320-node terms
  average 455 agents as nets and about 2,400 to 2,600 tree nodes.
- The net pays none of this because a variable is a wire. That is the saving bracket abstraction cannot give with a
  fixed rule set.
- Bulk combinators are an unbounded family of rules. A calculus with one node △ and a fixed rule set cannot use them
  as primitives. The n log n lower bound is stated for a fixed finite set of combinators; that it carries over to △
  (not a combinator in the classical sense) is likely but was not checked.

### Gaps
- Joy, Rayward-Smith & Burton 1985, Burton 1982, Noshita 1985, Hikita 1984, Kennaway 1982 and Kennaway & Sleep 1987
  were not fetched (paywalled). Their bounds are given above only as Czajka and Kiselyov report them.
- No bound was found for star abstraction into tree calculus specifically, beyond Jay's 5k − 2 for the naive form.

## 4. Is bracket abstraction ever done lazily, at run time? Is it ever treated as quoting?

### Takeaway
Yes to the first, once: in Jay's λSF-calculus the read operator F, applied to a λ-abstraction, performs one step of
star abstraction, on the innermost binder only, each time it is asked. That is lazy bracket abstraction used as a
reader. Nothing was found that calls abstraction elimination a quote; the nearest is Kiselyov treating it as an
evaluator, and Jay & Palsberg stacking a quote on top of it.

### Cited Findings
- The stated aim: "This paper shows how to factorise abstractions in a new calculus, the λSF calculus, by converting
  them to combinators when it is safe to do so, i.e. when this will not break any redexes in the body of the
  abstraction" — [λSF paper, §1](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=3)
- The rule: "F branches according to whether its first argument P is an atom, i.e. an operator, or a compound", and
  some abstractions count as compounds: those "whose decomposition is safe because either M is already an atom or
  compound", or the body is waiting for x — [λSF paper, §2](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=4)
- What F returns for an abstraction: a fixed marker `abs_left` that "will be used as the left component of any term
  that is not an application, especially of any abstraction", and as right component the star abstraction of the body
  (`(λx.M)` has right component `λ*x.M`). "no redexes are broken by taking components of compounds" — [λSF paper, §2.3](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=6)
- One step per read: "The decomposition of an abstraction λx.M will use the star abstraction"; for an application
  "the result uses λx.N instead" of recursing, and for nested binders "it is important that only one abstraction is
  eliminated at a time, namely, the innermost one" — [λSF paper, §2.2](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=5)
- Before any read the function is left alone: λx.λy.y "is already a closed normal form" — [λSF paper, §2.2](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=5).
  Reading all the way down gives a pure combinator: "Further factorisation eliminates the remaining abstractions to
  produce the combinator"; "Of course, it is much bigger than the original term" — [λSF paper, §2.2](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=6)
- The full compile is then a program in the calculus (`to_combinator`), "which eventually converts each abstraction
  λx.M in its argument to" λ*x.M — [λSF paper, §7](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=11).
  It can be undone only for star abstraction as such: "Star abstraction is intensional, with inverse unstar" — [λSF paper, §9](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=13)
- Jay's later summary: the SF results "are compatible with direct support for lambda-abstraction" — [TCS 2019 abstract](https://opus.lib.uts.edu.au/handle/10453/135540#s3)
- Kiselyov treats the translation as an evaluation, not a rewrite of syntax: "Unlike the well-known bracket
  abstraction based on (syntactic) term re-writing", his algorithm is "a non-standard evaluator for lambda-terms" — [Kiselyov 2018, abstract](https://okmij.org/ftp/tagless-final/ski.pdf#page=1).
  Lester applies it to calculi with quote and splice because it "gives meaning to open terms" — [Lester 2019, abstract](https://arxiv.org/pdf/1910.03217#page=1)
- Quote on top of abstraction (Jay & Palsberg): λ is compiled away first ("We de-sugar terms with such constructs
  before executing them" — [ICFP 2011, §4](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=3)); then a
  quote wraps each operator in a blocking operator ("B is used to block evaluation" — [§3](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=3)),
  so that "quoted terms are always normal forms, whose internal structure can be examined by factorising" — [§5.1](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=4)
- Their general remark on quoting: "The process of quotation tends to cause code expansion" — [ICFP 2011, §2](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=2)

### Inferences
- "Is the reader just lazy bracket abstraction?" For the user's net: no. For λSF: yes, literally. The difference is
  what a read does to the body.
  - λSF read of λx.M: rewrites the top node of the body into variable-free form (pushes the λ one level down, or
    turns it into S, K, I). The binder is consumed. Repeated reads end in a combinator larger than the term.
  - The net's read of a function: one rewrite relabels the function as opened; the body is not walked or changed;
    applying the opened function to a marker returns the body with the marker where the variable was. Reading the
    opened function closes it again. The binder survives.
- So the net's reader is closer to "open the binder" (apply to a fresh marker) than to abstraction elimination. It
  needs no routing because the variable is a wire.
- What the two share: both make a function's body readable one node at a time, with no whole-term pass. That is the
  sense in which the user's "lazy" intuition is right.
- How bracket abstraction relates to quoting, from sections 1, 2 and 4:
  - Same job in part: both turn a λ-term, which can run under the binder and so is unstable, into a stable normal
    form another program can take apart.
  - Different result: a quote gives data that must be unquoted or interpreted to run; bracket abstraction gives a
    term that still runs directly. That is Jay's "function and data structure at once".
  - Different loss: a quote keeps the source exactly; bracket abstraction keeps behaviour and drops the binder.
  - Both are outside the calculus when applied to a λ-term; λSF is the one case where the step is a rule.

### Gaps
- Searched for: bracket abstraction or abstraction algorithm "at run time", "on demand", "lazily", "on the fly",
  "during reduction"; bracket abstraction with "quote", "reify", "reflection". Found only λSF for the first and
  nothing for the second. No paper was found that names abstraction elimination a form of quoting.
- Fernández, Mackie & Sinot, "Lambda-calculus with director strings" (2005), keeps routing annotations up to date
  during reduction and is a likely second example of run-time routing; the PDF could not be fetched (see last section).

## 5. The debate: do these calculi do more than λ-calculus with a quote?

### Takeaway
Both sides are right under their own definition. If the program may be encoded first, λ-calculus simulates
SF-calculus (Rowe 2015). If the program must be handed over as itself, it cannot, and F cannot be defined away. The
disagreement is exactly whether the encoding step counts.

### Cited Findings
Jay's side:
- "any model of computability in which equality is not definable, such as the λ-models underpinning higher-order
  programming languages, is not equivalent to the Turing model"; SF-calculus "can define equality of its closed normal
  forms" — [Jay & Vergara, "Confusion in the Church-Turing Thesis" (arXiv 2014, marked draft), abstract](https://arxiv.org/pdf/1410.7103#page=1)
- Mutual simulation is not enough: "this mutual simulation is too weak to be an" equivalence — [same, p. 2](https://arxiv.org/pdf/1410.7103#page=2);
  "simulation yields an equivalence of models only if both re-codings are computable, each in its respective model" — [same, p. 13](https://arxiv.org/pdf/1410.7103#page=13)
- It depends on how a term is handed over: closed λ-terms can be "represented by either themselves or by the Church
  numeral of their" Gödel number, and one claim holds for the Gödel representation "while Corollary 3 is true for the
  inclusion representation"; there are functions of closed normal forms "that are not definable as λ-terms but are
  definable as" λSF-terms — [Jay & Vergara, "Conflicting Accounts of λ-Definability" (2017, Elsevier journal manuscript on the UTS repository), PDF page 2](https://opus.lib.uts.edu.au/bitstream/10453/125511/4/Jay%202017%20Conflicting%20accounts.pdf#page=2), [PDF page 3](https://opus.lib.uts.edu.au/bitstream/10453/125511/4/Jay%202017%20Conflicting%20accounts.pdf#page=3)
- Jay on the Turing-machine argument: "Note that Gödelisation is not relevant here, being solely concerned with the
  representation of" a list by a number; the missing step is writing the combinator out as a list — [JSL 2011, p. 8](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=8)
- No translation back: "There is no meaningful translation from tree calculus to SK-calculus" — [tree book, ch. 8](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=111);
  for λSF "there is no homomorphism of applicative rewriting systems from" λSF-calculus to λ-calculus — [λSF paper, abstract](https://opus.lib.uts.edu.au/bitstream/10453/122938/4/1-s2.0-S1571066116300913-main%20%281%29.pdf#page=2)
- A supporting third party: extensionally equal terms can be told apart, so "Consequently, SF-calculus cannot be
  translated into" λ-calculus — [Lester 2015, p. 6](https://arxiv.org/pdf/1512.03861#page=6)

The other side:
- Rowe gives "a faithful encoding of the Factorisation Calculus into the" λ-calculus, preserving reduction and strong
  normalisation — [Rowe, "Encoding the Factorisation Calculus", EXPRESS/SOS 2015, abstract](https://arxiv.org/pdf/1508.06713#page=1)
- "We show that there does exist a simulation of Factorisation Calculus within" λ-calculus; "our result signifies that
  Factorisation Calculus is no more powerful than" λ-calculus in the classical sense. Before that, Jay and Given-Wilson
  "speculate that the added expressive power may manifest itself in a non-existence result" — [Rowe 2015, §1](https://arxiv.org/pdf/1508.06713#page=2)
- Rowe's conclusion: the two "are of equivalent computational power, according to the classical interpretation of
  computational equivalence" — [Rowe 2015, §6](https://arxiv.org/pdf/1508.06713#page=14)
- Rowe also grants Jay's point. F cannot be macro-defined: "That is, we ask is F eliminable? The answer to this
  question is no", so "Jay and Given-Wilson’s results do justify the claim that" SF-calculus is more expressive in
  Felleisen's sense. On finer notions: "Our result shows that they are power equivalent", but "they cannot be
  bijectively power equivalent" — [Rowe 2015, §5](https://arxiv.org/pdf/1508.06713#page=13)
- The encoding has a price: it "leads to a large increase in the size of terms" (no figure given) — [Rowe 2015, §6](https://arxiv.org/pdf/1508.06713#page=14)
- The objections Jay reports receiving: "skepticism, tending towards hostility and derision"; "your claims contradict
  the Church-Turing Thesis"; and "The strongest objection is that there is nothing new here, since Barendregt has
  already shown how to add Church's δ" (an equality test on closed normal forms) to λ-calculus. His reply: "Jay is
  merely an observer" of a conflict between Barendregt and Kleene — [Jay & Vergara 2017, PDF page 2](https://opus.lib.uts.edu.au/bitstream/10453/125511/4/Jay%202017%20Conflicting%20accounts.pdf#page=2), [PDF page 4](https://opus.lib.uts.edu.au/bitstream/10453/125511/4/Jay%202017%20Conflicting%20accounts.pdf#page=4)
- Earlier mechanisms Jay himself lists for combinator equality: "meta-level operations [4, p. 245]; or partial
  combinatory algebras (not logics) such as the uniformly reflexive structures [25]; or discriminators [16, 17]" — [JSL 2011, p. 2](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=2)
- Jay's own bridge in the other direction: "there is a meaningful translation from VA-calculus to tree calculus", and
  with tags stored in environments a translation back, so "It follows that equality of translated programs is
  definable" in the λ-like calculus — [tree book, §1.3](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=18)

### Inferences
- Rowe's encoding plays the role a quote plays: the whole SF program is first mapped to a λ-term that represents
  it, and the encoded F then takes encoded terms apart. The encoded term still runs (the encoding preserves
  reduction), so it is a compile step more than an inert syntax tree. So "SF-calculus can be simulated in λ-calculus
  after an encoding step" is proven, with a size blow-up.
- Jay's point is that this mapping is not a function of the calculus on its own programs; with it, the reader is
  not reading the running program. That is a definition of "expressive", not a disputed theorem.
- For the user's question this means: trees do not add power over "λ plus a way to read code". They change where the
  encoding sits (in the compile step and in the rules) and what is lost (the binder).
- Church's δ, discriminators, eqatom, F, triage and the user's ρ are all the same kind of move: add one primitive
  that inspects a stable term.

### Gaps
- No published third-party reply aimed at "Confusion in the Church-Turing Thesis" itself was found. Searched for
  replies, critiques and "simulated" claims about SF-calculus; found Rowe 2015, Lester 2015 and Kavvos 2017 only.
  A Lambda the Ultimate thread exists but is a forum, so it is not cited.
- Jay's TCS 2019 abstract says a "non-standard encoding" is used "to lambda-define a trivial solution of the Halting
  Problem" ([abstract](https://opus.lib.uts.edu.au/handle/10453/135540#s3)); the argument itself is in the closed full text and was not read.

## 6. Typed versions and cost reports

### Takeaway
The typed work (Jay & Palsberg 2011) does use a quote, on top of star abstraction, and reports sizes only: 50
operators for the self-recogniser and 1,185 for the self-enactor. No published speed figure for a self-interpreter in
SF-calculus or tree calculus was found.

### Cited Findings
- Setting: "a combinatory calculus with a factorisation operator", typed with System F types, where F "is able to
  decompose compounds (e.g. closed normal forms) into their components" — [ICFP 2011, abstract and §1](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=1)
- Two steps are named: "The first is a process of quotation that transforms the syntax of a term" into a normal form;
  quotation "may add tags that block evaluation, which are then removed by unquote" — [ICFP 2011, §2](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=2)
- λ is compiled first: "One of the oldest results on computability is that λ-abstraction can be defined by SK-terms" — [ICFP 2011, §4.1](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=4)
- Sizes: the self-recogniser `unquote` is a combinator "which is built from 50 operators"; the self-enactor `enact`
  (it runs the quoted program): "it uses 1185 operators". "We have two implementations of both reduction and
  desugaring" (bondi and Scheme); no timings are given — [ICFP 2011, §8](http://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=9)
- Tree calculus: the `size` program "When applied to itself, the result is (the tree of) the number 508", so `size`
  is a tree of 508 nodes — [tree book, §6.1](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=76)
- The only speed remark in the book is qualitative: "Although branch-first evaluation is quite efficient it does not
  find all results" — [tree book, ch. 6](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=80)
- Jay names the hidden cost of tree rewriting: "evaluation rules for combinations commonly take one step to copy,
  delete or permute combinations", so "a single computation step may take an arbitrarily long time". "The obvious
  approach would be to use pointers, or let-expressions, to allow sharing", left for later because "equality of graphs
  is much harder to prove than that of trees" — [tree book, §11.5](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=138), [continued](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=139)
- Self-evaluation is Jay's test of the design: "Self-evaluation in a calculus provides good evidence for the ability to
  perform program analysis" — [tree book, §6.1](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=76)
- Later typed work: tree calculus supports "a self-interpreter that acts on itself directly, without encoding to a
  syntax tree" — [Jay & Bader 2026, §12.4](https://arxiv.org/pdf/2604.12194#page=23)

### Inferences
- The copy and delete cost Jay names in §11.5 is the same cost the user measured: 99% of a read in the net is copying
  and erasing the reading program's own code, and the view is 1%. Tree calculus counts a copy as one step, so its
  step counts hide this cost; the net counts every agent copied.
- Both designs point at the same fix: share code that never changes (hash-consing in the user's notes, pointers or
  let-expressions in Jay's).
- Jay's 508-node `size` in tree calculus and the 1,185-operator `enact` in SF-calculus are sizes after bracket
  abstraction; neither source gives the size of the λ-source, so the blow-up of those programs is not known.

### Gaps
- No step counts or timings for `bf`, `root`, `rb` (the tree-calculus self-evaluators) or for `enact` in a primary
  source.
- "Typed Program Analysis without Encodings" (Jay, PEPM 2025) was not fetched; it may hold typed tree-calculus
  figures.

## Not verified

- Kennaway & Sleep, "Director strings as combinators", TOPLAS 10(4), 1988, doi:10.1145/48022.48026. The script
  confirmed title, authors, year and venue from the DOI, but the ACM page returned 403 and no open copy was found
  (UEA lists it without full text). No quote from it is used; director strings are described above only through
  Kiselyov and Lester.
- Fernández, Mackie & Sinot, "Lambda-Calculus with Director Strings", AAECC 15 (2005). The author-hosted PDF
  (lsv.ens-paris-saclay.fr) timed out and has no archive capture. The claim that directors are kept up to date during
  reduction comes from a search summary only.
- Joy, Rayward-Smith & Burton 1985; Burton 1982; Noshita 1985; Hikita 1984; Kennaway 1982; Kennaway & Sleep 1987
  ("Variable abstraction in O(n log n) space"). Not fetched. Their bounds are quoted only as reported by Czajka 2015
  and Kiselyov 2018.
- Turner, "Another algorithm for bracket abstraction", JSL 44(2), 1979. Not fetched (JSTOR/Cambridge).
- Full text of Jay, "Intensional computation with higher-order functions", TCS 768 (2019), doi:10.1016/j.tcs.2019.02.016.
  Closed on the UTS repository and blocked at the publisher. Only the abstract on the repository record was verified.
- Lachowski 2018 full text. Only the abstract on the journal page was verified.
- The triage-as-a-rule variant of tree calculus (five rules), and a `size size = 168` figure seen in a search summary
  of a tree-calculus demo site. No primary source checked.
- Project figures quoted in the Inferences (17 agents and 21 rewrites per view, 1% / 99% split, the n = 16 table, 80×
  per interpreter level) come from the local files `this_net_measured.md` and `research/linear-ic-net/nets/DESIGN.md`,
  not from the web.
