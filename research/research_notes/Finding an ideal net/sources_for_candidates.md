# Sources for the candidates

Exact quotes behind the claims in this folder. Every link below printed `MATCH:` from `scripts/verify-source.sh` (2026-11-12). Page numbers are PDF pages, not printed page numbers.

## A. Barry Jay's definitions

Book: Jay, *Reflective Programs in Tree Calculus* (public PDF on GitHub; a local copy sits in `research/barry-jay/`).

**1. Intensional completeness (book §11.3).** Jay offers it as an informal sketch ("perhaps"): program analysis by pattern matching, where a pattern is any term that becomes a program once its variables are filled in with programs.
- ["Intensional completeness is perhaps the ability to perform arbitrary program analyses"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=138)
- ["as supported by pattern-matching functions"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=138)
- ["if all variables in the pattern are replaced by programs then the result is a program"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=138)
- ["tree calculus and SF-calculus are intensionally complete"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=138) ... ["but the traditional models and V A-calculus are not"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=138) (the PDF text layer spells it "V A-calculus")

**2. Program completeness (book §11.2).** This is a formal definition: the same functions of binary trees as tree calculus, plus an invertible map from programs to trees. VA-calculus fails it because it cannot decide program equality.
- ["Program Completeness"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=9) (contents entry for §11.2)
- ["a theory is program-complete if it supports the same class of functions of binary trees as tree calculus"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=137)
- ["supports an invertible function from programs to binary natural trees"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=137)
- ["VA-calculus is not program complete, since it cannot define equality of programs"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=137)

**3. Intensional behaviour (book §5.1).** It is whatever a query can see about a program without applying it.
- ["The intensional behaviour of a program p is all behaviour which is revealed by making it the argument of some query"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=66)
- ["without trying to apply p to arguments"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=66)

**4. No equality term in SK, and programs already being data.** An equality term `eq` reduces `eq M M x y` to `x` and `eq M N x y` to `y` when M and N differ. SK cannot define one, because it cannot tell apart combinators that behave the same, such as SKK and SKS. In tree calculus, programs are normal-form trees, so no quotation step is needed.
- ["Define an equality term in SK-calculus"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=106)
- ["equality_of_programs_is_not_definable_in_SK"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=109) (Theorem 49, which reads "There is no equality term in SK-calculus."; that phrase first [matches on p. 107](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=107), where the proof's goal is stated)
- ["cannot distinguish combinators that have the same functional behaviour"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=107)
- ["the programs are already data structures, so that no pre-processing is required"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=18)
- ["Gödel numbers or quotation is no longer necessary"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=5)

**5. Structure completeness (Jay and Given-Wilson, JSL 2011; open preprint in the UTS repository).** This is the formal one. A confluent combinatory calculus is structure complete when every case `P → M` with P in normal form is a term. The paper shows this holds exactly when the calculus has S, K, F (factorisation) and eqatom (atom equality).
- ["The resulting SF-calculus is structure complete"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=1), in that it ["supports all pattern-matching functions whose patterns are in normal form"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=1), including ["a function that decides structural equality of arbitrary normal forms"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=1)
- Definition (§8): ["A confluent combinatory calculus is structure complete if, for every normal term P and term M"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=15), the case G(P) = M ["is represented by some term P → M"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=15). Next: ["Such a calculus is combinatorially complete since"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=15) λx.M is x → M.
- Characterisation (Theorem 8.2): ["is structure complete if and only if it supports S, K, F and eqatom"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=16)
- ["A general characterisation of the structure complete, confluent combinatory calculi is given"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=1)

**6. λSF-calculus (MFPS 2016).** Not verified; see below.

## B. Interaction nets and sharing

**7. One principal port, interaction only between principal ports (Lafont 1997, *Interaction Combinators*).**
- ["Such a cell has one principal port and n auxiliary ports"](https://dcreager.net/pdf/Lafont1997.pdf#page=3)
- ["cells can only interact pairwise, through their principal ports"](https://dcreager.net/pdf/Lafont1997.pdf#page=5)

**8. Several principal ports bring nondeterminism and lose confluence (Mazza's PhD thesis, Part II "Multiport Interaction Nets and Concurrency").** The thesis credits Alexiev with introducing multiport cells and says Khalil encodes them with McCarthy's amb.
- ["simply by allowing cells to have more than one principal port"](https://lipn.fr/~mazza/papers/Thesis-1.0.pdf#page=25)
- ["one cell may be involved in several active pairs, as many as its principal ports"](https://lipn.fr/~mazza/papers/Thesis-1.0.pdf#page=25). ["The choice of which one is reduced is non-deterministic"](https://lipn.fr/~mazza/papers/Thesis-1.0.pdf#page=25), with an example of ["how this implies the presence of non-confluent nets"](https://lipn.fr/~mazza/papers/Thesis-1.0.pdf#page=25)
- ["already been introduced by Vladimir Alexiev in his Ph.D. thesis"](https://lipn.fr/~mazza/papers/Thesis-1.0.pdf#page=25); ["McCarthy's amb, which is represented by a cell with two principal ports"](https://lipn.fr/~mazza/papers/Thesis-1.0.pdf#page=25)

**9. A translated cell carries the code of the rules (Lafont 1997).** The universality proof is in §2 (§2.4–2.7); §3 is the reversible-machine semantics. The translation [σᵢ] of a symbol is a package (a closed net) built from menus and selectors, holding the tree of every rule it might take part in. Because duplicators cannot copy duplicators, the general case swaps that tree for a copyable "code" which acts as a genetic code.
- [σᵢ] ["can be defined inductively for each symbol"](https://dcreager.net/pdf/Lafont1997.pdf#page=19) (restricted, recursion-free case; Fig. 6 "Translation of a cell")
- ["represents the finite tree of all possible futures of a cell"](https://dcreager.net/pdf/Lafont1997.pdf#page=19); ["it suffices to use something like a genetic code"](https://dcreager.net/pdf/Lafont1997.pdf#page=19)
- a code is a package ["which can be duplicated, and from which"](https://dcreager.net/pdf/Lafont1997.pdf#page=20) the original package can be extracted; general case: ["Translation of a cell (general case)"](https://dcreager.net/pdf/Lafont1997.pdf#page=21), where one package ["plays the role of a genetic code"](https://dcreager.net/pdf/Lafont1997.pdf#page=22)

**10. Optimal (sharing-graph) reduction duplicates one node at a time (Asperti 2017, arXiv 1701.04240).**
- ["duplicate the abstraction node without jointly duplicating the whole body of the function"](https://arxiv.org/pdf/1701.04240#page=2)
- ["duplication is requested at the principal port of the node"](https://arxiv.org/pdf/1701.04240#page=3) (for λ nodes; for application nodes it is requested at an auxiliary port)
- Lamping-style sharing graphs ["performs the minimum amount of duplication required by the computation"](https://arxiv.org/pdf/1701.04240#page=5)

## C. Reversible computing

**11. Compute, copy, uncompute (Bennett 1989, *Time/Space Trade-Offs for Reversible Computation*, Lemma 1).** Cost: time O(T), space O(S + T).
- any Turing machine running in time T and space S ["can be simulated by a reversible input-saving machine running in time O(T) and space"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Tradeoffs.pdf#page=2) O(S + T)
- ["This history-taking renders the computation reversible"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Tradeoffs.pdf#page=2); after the output is copied, ["The third stage exactly reverses the work of the first stage"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Tradeoffs.pdf#page=2)

**12. Heat leaves through the surface (Frank, *Reversibility for Efficient Computing*, MIT PhD 1999, expanded manuscript; the live link is dead, so these use a Wayback copy).** Irreversible machines scale with surface area. Frank's reversible gain is Θ(√d) more operations per second at diameter d, which is less than a full use of the volume.
- ["the information that is thrown away when performing an irreversible operation must be somehow exported from the system"](https://web.archive.org/web/20230405014547id_/https://web1.eng.famu.fsu.edu/~mpf/Frank-99-PhD-bookmarked.pdf#page=115); processors grow as O(n³) ["whereas the surface area only increases by"](https://web.archive.org/web/20230405014547id_/https://web1.eng.famu.fsu.edu/~mpf/Frank-99-PhD-bookmarked.pdf#page=115) O(n²)
- irreversible computers are ["limited to processing rates that are only proportional to their surface area"](https://web.archive.org/web/20230405014547id_/https://web1.eng.famu.fsu.edu/~mpf/Frank-99-PhD-bookmarked.pdf#page=27)

**13. Programs as isomorphisms; erasing and creating information as effects (James and Sabry, *Information Effects*, POPL 2012, author copy).** Π has no conditional primitive. Branching comes from distributivity, (b1+b2)×b3 ↔ (b1×b3)+(b2×b3), so a branch is itself an isomorphism.
- ["we derive a typed, universal, and reversible computational model"](https://homes.luddy.indiana.edu/sabry/files/information-effects.pdf#page=1) where ["information is treated as a linear resource that can neither be duplicated nor erased"](https://homes.luddy.indiana.edu/sabry/files/information-effects.pdf#page=1)
- ["We turn the above isomorphisms into a programming language"](https://homes.luddy.indiana.edu/sabry/files/information-effects.pdf#page=3); conditionals ["they are expressible using the distributivity laws"](https://homes.luddy.indiana.edu/sabry/files/information-effects.pdf#page=4)
- the arrow metalanguage ["exposes creation and erasure of information as explicit effect operations"](https://homes.luddy.indiana.edu/sabry/files/information-effects.pdf#page=1); create and erase ["These operators are not isomorphisms and hence cannot have"](https://homes.luddy.indiana.edu/sabry/files/information-effects.pdf#page=7) ↔ types

**14. Token machines trade time for space; Lafont's reversible 2-stack machines.** Source: Accattoli, Dal Lago and Vanoni, *The (In)Efficiency of Interaction* (arXiv 2010.12988), which summarises the Danos–Regnier interaction abstract machine (IAM). It is secondary, but it states the point directly.
- ["it can be exponentially slower than environment machines"](https://arxiv.org/pdf/2010.12988#page=2) (example: t₁ = I, tₙ₊₁ = tₙ I)
- the IAM ["sacrifices time in order to be space-efficient"](https://arxiv.org/pdf/2010.12988#page=2)
- Lafont 1997: ["reversible 2-stack machines which seems to throw a bridge between distributed and sequential computation"](https://dcreager.net/pdf/Lafont1997.pdf#page=3); §3: ["we interpret the nets of combinators as reversible 2-stack machines"](https://dcreager.net/pdf/Lafont1997.pdf#page=22)

**15. Rotary element: a shared module with state and no garbage (Morita 2008 survey, Hiroshima repository via Wayback).** A particle that enters an RE either passes straight through or turns and rotates the bar. It changes the stored state and leaves.
- a particle from the orthogonal direction makes a right turn and ["rotates the bar by 90 degrees counterclockwise"](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=10)
- ["reversible sequential machine can be built very simply only from REs as a completely garbageless circuit"](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=7)
- Theorem 2.5: ["For any RSM, there is a completely garbageless circuit composed only of REs that simulates the RSM"](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=12)

## Not verified

- **6, Jay's λSF paper** ("Programs as Data Structures in λSF-Calculus", ENTCS 325, doi:10.1016/j.entcs.2016.09.040): ScienceDirect returned 403 and the CyberLeninka copy timed out, so no quote was checked.
- **7, Lafont 1990** (*Interaction Nets*, POPL): not fetched. Lafont 1997 is used for the one-principal-port rule instead.
- **8, primary copies** of Alexiev's thesis (1999) and Fernández–Khalil's amb paper: not fetched. Both are cited only through Mazza's thesis.
- **10, Lamping 1990 and the Asperti–Guerrini book:** not fetched. Asperti 2017 is used instead.
- **11, Bennett 1973:** not fetched. Bennett 1989 is used instead.
- **14, Danos and Regnier's own IAM paper:** not fetched. The slowdown quote comes from Accattoli et al., a secondary source. No primary sentence says the token machine shares no work.
- **15, Morita arXiv 1309.1264:** not used. The 2008 survey covers the claim.
- **4, scope note:** the book's §11.3 calls intensional completeness "perhaps" and gives no formal definition. Only program completeness (book) and structure completeness (JSL paper) are formal definitions.

Checked: 61 quotes printed MATCH, and every one is linked above. The book version of "the traditional models and VA-calculus are not" missed only because of the PDF's "V A" spacing; the matching form is used above.
