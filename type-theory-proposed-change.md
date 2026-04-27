● Proposed Type n design for the doc 
   
  Committing to Option (a): registered base types at rank 0, structural descent for composites, cumulativity by construction. Here's the full picture with change mapping.   
   
  The design in one page 
   
  Type is a function Nat -> Predicate:   
   
  let Type : Nat -> Predicate  
 = fix ({self, n} -> {t} ->
  // (1) universe-in-universe: t is mkUniverse k for k < n   
  or (and (is_universe t) (lt (universe_rank t) n))  
 
  // (2) compound: Pi / Sigma / RecType with components at rank ≤ n  
 (or (and (is_pi t)   
  (and (self n (piA t))  
   { let a = fresh_hyp (piA t)   
 self n ((piB t) a) }))  
 (or (and (is_rectype t) (all_fields_at_rank n self t))  
 (or (and (is_sigma t) (sigma_at_rank n self t)) 
 
  // (3) registered base type (trusted — user asserted at declaration)   
 (is_registered_base_type t))))))
 
  Universe encoding: every rank gets a distinct tagged tree.  
 
  mkUniverse : Nat -> Tree   
  mkUniverse k = tagged KU (encode_nat k)  
 
  Type n (the rank-n universe term, as a value you can pass around) is mkUniverse n. By the recursion, mkUniverse n : Type (n+1) via case (1) — universe_rank (mkUniverse n) = n < n+1.  
   
  Registry of trusted base types: a list of hash-cons identities, threaded through elaboration state alongside metas. Grown by every let X : Type 0 = body declaration — the elaborator adds fast_eq-identity-of-X to the
  registry. is_registered_base_type t does a right-fold of fast_eq t over the registry.   
 
  Rank inference for user declarations:  
   
  - let Nat : Type 0 = body — explicit. Trust body; add Nat to registry. 
  - let Maybe : Type -> Type = body — rank inferred from signature. Type -> Type is Pi Type (_. Type); checking this against Type n structurally gives n ≥ 1 (because inner Type means Type 0, which means outer Pi's A is Type
  0, requiring the whole thing at Type 1). Not registered; composite.
  - let id : {A : Type} -> A -> A = body — implicit rank meta. Signature becomes Pi (Type ?ρ) ({A} -> Pi A ({_} -> A)). Elaboration solves ?ρ per use site.   
 
  What changes in TYPE_THEORY.typ
 
  §3 — fresh-hypothesis primitive
  
  Small addition: note that fresh_hyp A produces a hypothesis whose stored type A has its own rank. The hypothesis inherits that rank's validity — i.e., when A : Type k, the fresh a : A is itself usable as a rank-k entity
  (passes Type m for m ≥ k via case 2's descent). 
 
  §4 — evaluator interface
   
  - H-hypothesis rule: tighten the wording. Currently says "the candidate is the same hypothesis." Correct version: when apply(h, v) where h is a KH-tagged hypothesis with stored type A, return TT iff quote v's stored type is
   hash-cons-equal to h's stored type. (Phrased: "predicate is a hypothesis of type A, candidate is known to inhabit A".)
  - New primitive universe_rank: extract the rank from a mkUniverse k term. Add to the interface.
  - Structural inspection primitives: is_pi, piA, piB, is_rectype, is_universe, and friends. These have to be in the interface because Type n uses them. Currently the doc treats them implicitly; make explicit.
 
  §5.1 — Type (rewrite completely)   
 
  Replace the current §5 Type section with:  
   
  - Preamble: why stratify. Girard's paradox in one sentence. The hierarchy is the fix.  
  - Definition: the full Type : Nat -> Predicate above, with pseudocode.  
  - Universe terms: mkUniverse encoding, relationship to Type n-the-predicate vs Type n-the-term.
  - Three admissible shapes explained: universe-in-universe, compound, registered base.  
  - Base-type registry: what it is, how it's threaded, user declarations extend it.  
  - Cumulativity: shown to fall out of the definition.   
  - Universe polymorphism: implicit rank meta, solved at use.
  - Examples: Nat : Type 0 by registry; Type 0 : Type 1 by case 1; {A : Type 0} -> A -> A : Type 1 by case 2.
 
  §5.2 — Pi  
 
  Current Pi def is rank-agnostic. Add: "Pi A B has rank max(rank A, rank (B a)) for fresh a : A. When the elaborator needs to check Pi A B : Type n, it reduces to the two sub-checks Type n A and Type n (B a)."   
   
  §5.3 — RecType / Sigma 
  
  Same story: rank is max of component ranks. Concrete formulas. 
  
  §5.4 — Eq (fix the missing lambda) 
  
  let Eq : {A : Type} -> A -> A -> Type  
 = {A} -> {a} -> {b} -> {_} -> fast_eq a b   
  The {_} -> makes Eq A a b a predicate awaiting a (irrelevant) witness. 
 
  §6 — worked examples   
 
  Update example 2 and 3 to show the hypothesis's stored type participating in the check, not just "the same hypothesis." Add a new example: Type 0 : Type 1 — the smallest meaningful cumulativity trace.   
 
  §7 — soundness 
  
  - Invariant III, Type bullet: rewrite. Instead of "reduces to boolean on fresh hypothesis," say "Type n t is TT iff t is registered at rank ≤ n, or t = mkUniverse k for k < n, or t is a compound type constructor whose  
  components all satisfy Type n."
  - Breakage table: add "Registry corrupted" — if a non-data predicate is admitted as rank-0, rank-1 types that depend on it inherit the inconsistency.  
  - What soundness does not include: "Girard / logical inconsistency" is no longer in this list — the hierarchy provides it. Replace with: "Strong normalization is still assumed; without it, fix-pointed user predicates can   
  diverge and the budget-as-error rule still applies."   
 
  §8 — open questions
  
  - Remove "universe stratification" as an open item — it's the design.  
  - Keep extensional equality.
  - Add: "Registry management — should rank-0 declarations be restricted to definitions using only base-vocabulary primitives (fast_eq, is_fork, first, second, and, or, fix, TT, FF), or purely user-trusted? The   
  trust-by-assertion approach is simpler but risks a malformed rank-0 declaration corrupting the hierarchy." 
  - Add: "Rank-polymorphic definitions — supporting let id : {n : Nat} -> {A : Type n} -> A -> A = ... requires ranks as first-class values, which means Nat is pre-installed in the registry before any user declarations.  
  Bootstrap ordering matters."   
  
  Glossary additions 
  
  - mkUniverse k — object-language term for the rank-k universe. 
  - universe_rank t — evaluator primitive, extracts k from mkUniverse k.  
  - is_registered_base_type t — evaluator predicate, checks the registry.
  - rank — a Nat associated with every type; smallest n such that Type n t = TT. 
  - cumulativity — property that Type n t implies Type (n+1) t.  
  - Registry — the list of hash-cons identities of trusted rank-0 base types.
 
  Things the design requires that aren't in the doc yet  
 
  Elaborator duties (new):   
  - Solve implicit rank metas at Type use sites.  
  - Infer ranks for composite type signatures (walk the signature, derive).  
  - Maintain the base-type registry; add entries on let X : Type 0 = .... 
  - Reject let X : Type 0 = body where body is structurally not a data predicate. (Or — per the open question — don't reject, trust.)
  - Insert mkUniverse k terms where the source writes Type k.
 
  Evaluator primitives (new):
  - is_universe, universe_rank — reflection on universe terms.   
  - is_pi, piA, piB — reflection on Pi-predicate form.   
  - is_rectype, is_sigma, and field/component accessors — reflection on record/sigma predicate form.  
  - is_registered_base_type with registry argument (threaded via evaluator state).   
 
  Kernel object-language definitions (new):  
  - KU tag for universes.
  - encode_nat for the rank payload (fork-chain or Church numeral — pick one).   
  - The full fix-pointed Type predicate above, with all helpers. 
 
  What I want to flag as still open / thin   
 
  (1) The "composite rank" algorithm needs to handle a polymorphic Pi cleanly. Pi Type 0 ({A} -> Pi A ({_} -> A)) — outer Pi quantifies over Type 0, requires rank ≥ 1 for the whole thing. But what's the rank of the inner 
  body? Under a fresh A : Type 0, the inner body is Pi A ({_} -> A) : Type 0. So max(rank Type_0, rank inner_body) = max(0, 0) = 0? That would say the whole Pi is Type 0, which is wrong — it quantifies over types.
 
  The fix: "Pi Type k (...)" is NOT rank k; it's rank (k+1) because quantifying over Type k consumes a universe level. The rule is more subtle than max: 
   
  rank (Pi A B) = max(rank A, rank(B a))   where a fresh 
IF rank A < k for some k where Type k accepts A  
THEN the Pi lives at k, not at rank A
 
  Actually in Coq/Agda: Pi (A : Type u) (B : Type v) has rank max(u, v). If A = Type u, then rank A = u+1 (because Type u lives in Type (u+1)). So Pi Type 0 ({A} -> Pi A ({_} -> A)):   
  - rank Type 0 = 1 (because Type 0 : Type 1)
  - rank (Pi A ({_} -> A)) for fresh A : Type 0 = max(0, 0) = 0  
  - Overall: max(1, 0) = 1. ✓ 
 
  So the formula rank (Pi A B) = max(rank A, rank (B a)) works — but rank of a type-valued thing is the rank of the universe containing it. rank Type 0 = 1; rank Nat = 0; rank A (fresh hyp of type Type 0) = rank Type 0 = 1?
  Or does the hypothesis inherit its stored type's rank? 
  
  Hmm. If A = fresh_hyp (Type 0), then A's value is a hypothesis, but A is a type (it was asserted to be so via Type 0). Its rank as a type is 0 (it's a rank-0 type because Type 0 accepts it). When we ask rank (Pi A B) we're 
  asking the rank of the Pi-type, given A is a Type 0-inhabitant. So rank A = 0, not 1.   
 
  So there are TWO distinct notions: 
  - rank-of-a-type: the smallest n such that Type n t = TT. Nat has rank 0. Type 0 has rank 1.
  - type-of-something: what type it inhabits. A : Type 0 means A's type is Type 0.   
  
  The Pi formula uses rank-of-a-type, not type-of-something. A : Type 0 has rank 0 because Type 0 A = TT (A is a valid rank-0 type). So rank A = 0.  
 
  Got it. Then rank Type 0 = 1 because Type 0 Type 0 = FF (case 1 fails: 0 < 0 is false), Type 1 Type 0 = TT (case 1: 0 < 1). So smallest n is 1.
 
  OK the formula holds. Worth including this worked out in the doc.  
  
  (2) The H-hypothesis rule with stratification. When a fresh A = fresh_hyp (Type 0) is in scope and we check something against Type 1, how does the H-rule interact?
  
  Specifically: checking Pi A (_. A) : Type 1. Case 2: is_pi t = TT. Recurse on piA = A. self 1 A — applying Type 1 to a fresh hypothesis A. 
  
  A is KH-tagged with stored type Type 0. Does Type 1 accept it? Case 3 (registered) — no, A isn't registered. Case 1 (universe) — no, A isn't mkUniverse k. Case 2 (compound) — no, A isn't a Pi/rectype/sigma structurally.
  
  So Type 1 A = FF? But we know A : Type 0 (it's a hypothesis of type Type 0), and by cumulativity should also be Type 1.
  
  This is where the H-hypothesis rule extension I hinted at in the review (Option X) might be needed: when applying a universe predicate Type n to a hypothesis with stored type Type k, return TT iff k < n. This encodes the   
  universe hierarchy into the H-rule. 
 
  But then universes become magic — the H-rule special-cases them.   
   
  Alternative (Option Y from review): the Pi-check descent carries rank context. When descending into Pi (Type 0) B, the fresh A = fresh_hyp (Type 0) is marked as rank-0. Checking B's body : Type 1 under this descent: the
  descent sees A and knows it's rank-0 (from the fresh_hyp introduction), so treats it as Type 0-valid. No re-check against Type 1's predicate.   
 
  Option Y is cleaner but couples fresh_hyp to rank tracking. Doable.
   
  I lean toward Option Y — the descent already carries binder depth; carrying rank-of-each-hypothesis is a natural extension.
  
  (3) Bootstrap ordering. Before any user declarations, the registry must contain at least Nat (so ranks can be written as Nat values). And Type itself — but Type is a function, not a registered base type. Circular?  
  
  Resolve by: Nat is pre-installed at rank 0 (hardcoded into the initial registry). mkUniverse is a primitive that takes a Nat and produces a universe term. Type n the predicate uses Nat via universe_rank, lt, etc. — these   
  are built-in.   
 
  So the initial state has: Registry = [Nat, Bool, Unit] (or similar). Users extend. 
   
  (4) The Type name clash. Type is used for (a) the predicate Type : Nat -> Predicate, (b) the universe term Type n = mkUniverse n, (c) the surface syntax Type with implicit rank meta. Three different things, one name. Doc   
  needs to disambiguate — probably by context, but clean notation helps: Type (surface) vs TypePred (the predicate function) vs mkUniverse n (the term).  
 
  (5) Does the kernel see rank? Or just the elaborator? The evaluator needs to compute universe_rank t, so it's a runtime concept. The kernel sees rank. That's fine but makes the evaluator interface bigger.   
   
  My ask before I touch the doc  
  
  Three design choices to pin down:  
  
  1. Option X or Option Y for hypothesis-meets-universe? (H-rule extension vs descent rank-tracking.) I lean Y.  
  2. Rank-0 declarations: trust-by-assertion or syntactic-restriction? Earlier you said trusted. Confirming — Type 0 accepts any user-declared predicate at rank 0, no structural validation, because if users break it they
  break their own code. The open question stays for future tightening.   
  3. Bootstrap registry: what's pre-installed? Nat, Bool, Unit seems minimal. Anything else a prelude needs?  
 
  With those answered I can do the doc sweep in one pass.