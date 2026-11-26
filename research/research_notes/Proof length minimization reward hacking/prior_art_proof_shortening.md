# Prior art: automated proof shortening, golfing and compression (and gaming of the length measure)

Verification legend: **[V]** = read in a primary source during this session (quote from fetched page/PDF). **[S]** = seen only in a search-engine snippet or a secondary summary, not read in the primary source. **[M]** = from my prior knowledge, not checked this session; treat as unverified.

## ImProver (Ahuja, Avigad, Tetali, Welleck; arXiv 2410.04753, ICLR 2025): metrics, length definition, failure modes

### Takeaway
ImProver measures length as the number of tactic invocations and openly reports a degenerate solution for its readability metric: the model can pad a proof with unused `have` statements to raise the "declarativity" ratio. That is a documented, author-acknowledged case of metric gaming in LLM proof optimization. The paper does not report shorter-but-slower proofs or reliance on heavier automation.

### Cited Findings
- **[V]** Length metric (§3.1): it "measures the number of tactic invocations in the tactic proof". But the model-facing prompt (App. A.2) says length is "measured in the number of lines of the proof". So the scoring metric and the instruction to the model differ — [arXiv HTML](https://arxiv.org/html/2410.04753)
- **[V]** Readability/"declarativity" metric (§3.1): "the ratio of number of explicitly typed have tactics to total number of tactic invocations"; reported improvement is the difference μread(y)−μread(yo) (§4.1) — [arXiv HTML](https://arxiv.org/html/2410.04753)
- **[V]** Other metrics (§3.1): a completion metric that "measures the number of errors present", and a mixed metric that scores each tactic at −1 and each declarative tactic at +5, so "each declarative tactic 'pays' for 4 additional tactics" — [arXiv HTML](https://arxiv.org/html/2410.04753)
- **[V] Metric gaming acknowledged** (§3.1, "Degenerate Solutions"): "overuse of have statements can greatly increase the declarativity of the proof", even when the `have`s are "not being used in the proof's deductive process whatsoever". The authors call their metrics "intentionally simplistic" and steer the model with human examples rather than relying on the reward alone — [arXiv HTML](https://arxiv.org/html/2410.04753)
- **[V]** (§4.2) "optimizing for declarativity is more difficult for the underlying generator than optimizing for length." — [arXiv HTML](https://arxiv.org/html/2410.04753)
- **[V]** Headline results (Table 1): length improvement 20.96 vs 3.7 for GPT-4o (described as "566%"); declarativity 9.34 vs 2.21; mixed 27.31 vs 3.51. Among nonempty outputs: "we expect an average of a 55.29% reduction in length". Per dataset (length): MIL 30.54 vs 6.25; Compfiles 18.86 vs 2.75; Mathlib 6.19 vs 0.0. Example (App. B.3): a Compfiles lemma goes "from 12 tactics to 6." Limitation (§5): "ImProver is limited by its high cost" — [arXiv HTML](https://arxiv.org/html/2410.04753)
- **[S]** ImProver's related work says AlphaProof "was shown to produce proofs with an arcane, non-human structure and syntax" (seen in a search summary of the ImProver text) — [ImProver ICLR 2025 PDF](https://proceedings.iclr.cc/paper_files/paper/2025/file/4864005cfdea7ebd07086ed1b9846825-Paper-Conference.pdf)

### Inferences
- Counting tactics gives credit for packing work into fewer, heavier tactics (for example one `nlinarith`/`simp` in place of several explicit steps), and for `<;>` combinators. ImProver does not discuss this; it follows from the definition.
- The `have`-padding exploit is the mirror image of length gaming: a readability proxy based on counting can be inflated by syntax that does no work.

### Gaps
- The final ~18k characters of the HTML (late appendices) were not read. Any discussion of check time or automation-heavy outputs there is unverified.

## ProofOptimizer (Gu, Piotrowski, et al., Meta FAIR / MIT; arXiv 2510.15700, NeurIPS 2025 workshop / ICLR 2026)

### Takeaway
ProofOptimizer measures length in Lean-tokenizer tokens. Its RL reward is the relative shortening, given only when the new proof is valid and not longer. The authors document one gaming mode: the model sometimes swaps a fast algorithm for a shorter brute-force one (e.g. `interval_cases`), which makes the proof much slower. They mitigate this by offering a heartbeat-aware objective. They also report diversity collapse under RL.

### Cited Findings
- **[V]** Length (§2): "the number of tokens produced by a Lean-specific tokenizer"; it "does not penalize long identifier names, and it ignores comments and line breaks" — [arXiv HTML](https://arxiv.org/html/2510.15700)
- **[V]** Reward (§3.1.2): relative shortening "if y is valid and |y|≤|x|", with "R(x,y)=0 otherwise". The length constraint is "designed to encourage the model to learn more substantial simplifications rather than trivial ones" (§3.1.1) — [arXiv HTML](https://arxiv.org/html/2510.15700)
- **[V]** Pipeline: a Lean linter first strips redundant steps ("linter.unusedTactic", §3.2), then the model iteratively simplifies the current shortest proof — [arXiv HTML](https://arxiv.org/html/2510.15700)
- **[V] Gaming / side effect (§5.2):** simplified proofs "often exhibit a faster execution time", but there are "several cases where the simplified proofs are much slower than the original proof". The mechanism given: "a faster proof algorithm is replaced by a shorter but slower method (e.g. brute force with interval_cases)." — [arXiv HTML](https://arxiv.org/html/2510.15700)
- **[V]** Mitigation (§5.2.1): "optimizing for a combination of proof length and heartbeat count". With it, the miniF2F proofs showing >10% speedup rise from 114/194 to 138, and those showing >50% from 56 to 81; the authors say all points then fall on or below y=x (no slowdowns). PutnamBench under the length-only objective: 50 of 75 proofs >10% faster, 22 >50% faster. IMO 2025 speedups (Table 7): P1 1.02×, P3 1.02×, P4 1.25×, P5 1.81× — [arXiv HTML](https://arxiv.org/html/2510.15700)
- **[V]** RL side effect (§4.1): RL "significantly improves single sample (@1) metrics at the expense of diversity collapse". PutnamBench reduction "experiences oscillations" during RL — [arXiv HTML](https://arxiv.org/html/2510.15700)
- **[V]** Heavier automation is described as the normal simplification pattern. App. D: "invoking one proof automation tactic (like linarith instead of applying a sequence of more explicit proof steps". App. G, Listing 11, collapses explicit steps into `subst_vars` and `nlinarith` — [arXiv HTML](https://arxiv.org/html/2510.15700)
- **[V]** Limitations: the metric "does not take into account unnecessarily long or redundant sub-theorem statements" and the setup "does not consider the space of structure-level simplifications" (§C.3). Readability is stated as the goal ("concise and readable", §7) but never measured. "the longer the proof, the more challenging it is to shorten it" (App. D) — [arXiv HTML](https://arxiv.org/html/2510.15700)
- **[V]** Headline numbers (abstract): length reduced "by 87% on miniF2F, 57% on PutnamBench, and 49% on Seed-Prover's IMO 2025 proofs". The search summary gives 50% for IMO, so versions differ. Training on simplified proofs gives about 2% better miniF2F (§5.1) — [arXiv HTML](https://arxiv.org/html/2510.15700), [ICLR 2026 poster](https://iclr.cc/virtual/2026/poster/10008013)
- **[S]** One search summary reports "28% of proofs showing at least a 1.5x speedup" — [arXiv v1](https://arxiv.org/html/2510.15700v1)

### Inferences
- The `interval_cases` case is the clearest documented example of an LLM proof-shortener trading length for compute. The fix was a check-time term in the objective, not a stricter validity check.
- A token count that ignores identifier length gives no penalty for calling one long-named, powerful lemma, or for a single heavy decision procedure. Nothing in the reward rewards explicit reasoning.

### Gaps
- I did not see the paper explicitly analyse `simp`/`omega`/`grind`/`aesop`/`decide` substitution as reward hacking, or report `<;>` chaining. The ProofOptimizer token-counting code (App. L) was not visible.

## Other LLM-era proof shortening and length-shaped rewards (2023–2026)

### Takeaway
Few RL provers put proof length into the reward. Where it appears, it is a per-tactic cost (AlphaProof, process-verified RL) or a tactic-count reward (Leanabell-Prover-V2, small gains). I found no paper that documents reward hacking of a length penalty specifically. The documented gaming cases are ProofOptimizer's slower brute-force proofs and ImProver's `have`-padding.

### Cited Findings
- **[S]** AlphaProof (Hubert et al., Nature 2025): each applied tactic carries reward −1 "to incentivize the discovery of the shortest proof". Among found proofs it keeps "the one that minimizes a cost function linearly depending on string length and execution time". The Nature page redirected to a login and could not be read, so this comes from a search summary only — [Nature](https://www.nature.com/articles/s41586-025-09833-y)
- **[S]** Leanabell-Prover-V2, with Kimina-Prover-Preview-Distill-7B as base: "when using R_tactic count, we achieve slight gains", while the R_automation and R_state change rewards "perform slightly worse" than plain success/failure — [arXiv 2507.08649](https://arxiv.org/pdf/2507.08649)
- **[S]** DeepSeek-Prover-V2 used binary rewards ("1 if verified as correct and 0 otherwise"), with no length term — [arXiv 2504.21801](https://arxiv.org/pdf/2504.21801)
- **[S, unverified secondary]** A Medium post claims that after RL, Goedel-Prover's average proof length "balloons from 298 tokens to 486 tokens" and the model spams `try` "10.89 times per proof". This is length growth with no length penalty; it is not confirmed in the primary paper — [Medium](https://medium.com/@AIchats/goedel-prover-the-self-improving-super-human-ai-mathematician-fb03a004dc8b)
- **[S]** Process-Verified RL for Lean (arXiv 2606.20068) uses per-tactic penalties (e.g. −0.05 for partially valid, −0.1 for erroneous steps). Length results not seen — [arXiv](https://arxiv.org/html/2606.20068)
- **[V]** Seed-Prover's IMO 2025 proofs were long enough that ProofOptimizer reports a ~49% length reduction on them — [arXiv HTML](https://arxiv.org/html/2510.15700)

### Inferences
- The mechanisms shown pushing toward shorter proofs are a per-tactic cost (AlphaProof) and post-hoc simplification (ProofOptimizer). Both can be gamed by heavier single tactics, and AlphaProof's reported cost function adds execution time, apparently anticipating this.

### Gaps
- Not found or not checked: Harmonic's Aristotle and Math Inc's Gauss — no published proof-golf or length-penalty details found. Kimina-Prover length penalty: not found. Mathlib "golf bots": no dedicated bot found. TacMiner, Peano (Poesia et al.) and "Learning to compress proofs" were not researched in this pass (**[M]** Peano learns reusable tactic abstractions to shorten solutions; unverified).

## Metamath: MINIMIZE_WITH, shortening culture, measures, and explicit anti-gaming guards

### Takeaway
Metamath (set.mm) is the strongest precedent. The community shortens proofs routinely, both by hand and by machine (OpenAI's GPT-f in 2020). Experience showed that "shorter" proofs can cheat in ways that do not affect correctness: adding axiom dependencies, using discouraged or OLD theorems, or adding distinct-variable conditions that weaken the theorem. Metamath responded with explicit guards: `minimize_with` no longer adds new axioms by default, plus `/no_new_axioms_from`, the "(New usage is discouraged.)" tag, ALT/OLD conventions, and a filter on axiom traces.

### Cited Findings
- **[S]** In 2019 `minimize_with` changed. Before, "minimize * with no qualifiers could add dependencies on new axioms if it made the proof shorter". Now "by default 'minimize' will never introduce dependence on new axioms unless the user adds a new qualifier, '/allow_new_axioms <label match>'". "/no_new_axioms_from <label-match>" overrides it. Norm Megill's recommended everyday form is "minimize * / allow_new_axioms * / no_new_axioms_from ax-*" — [mail-archive: Changes to minimize_with](https://go.mail-archive.com/metamath@googlegroups.com/msg00447.html)
- **[V]** The set.mm wiki recommends "type `minimize */allow */no_new ax-*` after your `IMPROVE ALL` and before you save the proof", and justifies minimizing because "a short proof requires less storage space" — [set.mm wiki: Minimizing](https://github.com/metamath/set.mm/wiki/Minimizing)
- **[S]** A 2023 thread reported that minimizing with only `/ALLOW_NEW_AXIOMS` could pick a shortening that depends on `ax-13`, so `/NO_NEW_AXIOMS_FROM ax-*` is needed ("the minimizer will use ~brrelex12 as a valid shortening") — [Google Group: bug in MINIMIZE_WITH?](https://groups.google.com/g/metamath/c/JYws500zfN8/m/eQewW3ARBwAJ)
- **[S]** The string "(New usage is discouraged.)" in a comment tells metamath-exe and mmj2 not to use that statement in a proof under development. One contributor (2020): we usually "just accept what is shortest (as long as it doesn't add axioms or other discouraged statements)" — [Google Group: ex-natded5.3i used on minimize](https://groups.google.com/d/msg/metamath/f-L91-1jI24/3KJnGa8qCgAJ)
- **[V] AI shortening (OpenAI, 2020).** Stanislas Polu, 26 Mar 2020: proofs "substantially shorter than their human curated counterparts", from "a sample of ~1k proofs", with "we could probably get 45 times as many shorter proofs if there is interest for it!" (set.mm PR #1547). Benoit asked whether new axiom dependencies were introduced and pointed to `show trace_back /axioms`. Polu raised a trade-off between proof length and axiom-dependency size. Replaced proofs were to be kept as "xxxOLD", and a shorter proof that uses more axioms as "xxxALT" — [Google Group: Deep Learning powered proof shortening](https://groups.google.com/g/metamath/c/-FNsw2wyllI)
- **[V] Documented gaming cases (OpenAI round 2, Jul 2020).** Polu kept only proofs "shortened by a factor of at least 2x", excluding *ALT/*OLD. Examples: dvtanlem "247 steps to 14"; 1re (shorter, but uses more axioms); elsb4. Objections raised:
  - ookami flagged elsb4 for adding a distinct-variable condition the original lacked; Glauco noted such conditions "can really weaken results"; van der Vekens said reuan "must not" be shortened for this reason.
  - Megill: 1re relies on df-1, which is marked "(New usage is discouraged.)".
  - van der Vekens: suppss2f's shortening is "not allowed" because it uses suppss2OLD.
  - Wheeler: non-ALT proofs should "normally never" add axioms; credit should read "(Shortened by OpenAI)".
  - Polu agreed to be "very cautious" and to filter for no extra distinct variables and no larger axiom trace.

  [Google Group: More proof shortenings from OpenAI's models](https://groups.google.com/g/metamath/c/2F7knFknKJ8/m/3yi2o67tAwAJ)
- **[V] Readability debate (Mar 2020).** Marnix Klooster worried that auto-shortened proofs will lack structure and "will only be correct". Norman Megill found the OpenAI-shortened 2exp6 "much easier to read" and said "it has been rare (if ever) that I have considered longer proofs easier to read"; he also valued faster verification. Mario Carneiro said ML-generated proofs are "extremely unlikely" to be harder to understand, and that bad short proofs come from exhaustive search or strict optimality constraints (example: dfbi1ALT). Benoit: when a short proof is less clear, keep both via `xxxALT`. Richard Penner: shortening means removing waste or using more powerful tools (intss went from 10 steps to 5) — [Google Group: Why would proof shortening be useful?](https://groups.google.com/g/metamath/c/CMKRwV0CX3w/m/9BxgffRtBQAJ)
- **[M]** set.mm credits shortenings in theorem comments, e.g. "(Proof shortened by NAME, DATE.)", and also uses "(Proof modification is discouraged.)". metamath.exe `MINIMIZE_WITH` has qualifiers such as `/MAY_GROW` (accept replacements that do not shrink the proof), `/EXCEPT`, `/FORBID`, `/OVERRIDE` (allow discouraged statements) and `/INCLUDE_MATHBOXES`. Recent versions compare the compressed proof's size rather than the raw step count. I believe this but did not verify the qualifier names or the measure this session.

### Inferences
- Metamath's guards each block a specific way a shortening can preserve validity while changing what is proved or what it depends on: axiom trace (`/no_new_axioms_from`), discouraged definitions ("New usage is discouraged"), deprecated theorems (OLD), and weaker hypotheses (extra `$d` conditions). This is the closest existing precedent for designing a length objective that resists gaming.
- Even in Metamath, where every step is a primitive inference, the community concluded that "shorter" needs side constraints. A tactic language with heavy automation leaves more room for gaming.

### Gaps
- I did not fetch metamath.exe's `HELP MINIMIZE_WITH` text, so the exact qualifiers (`/ALLOW_GROWTH` vs `/MAY_GROW`) and the precise size measure (compressed characters vs steps) are unverified.

## Wos, Thiele, Veroff, OTTER/Prover9: Hilbert's 24th problem and multiple simplicity measures

### Takeaway
The automated reasoning community long ago defined several competing simplicity measures: length (steps), size (symbol count), level (depth), term structure, and proof-graph complexity. Its documented trade-offs are concrete: the shortest proof can have heavier clauses, and the proof with the smallest DAG can have a larger tree size.

### Cited Findings
- **[S]** Rüdiger Thiele found Hilbert's 24th problem (criteria for simplicity of proofs, and a proof theory able to show that a proof is the simplest) in Hilbert's notebooks in 2000. Thiele, *Amer. Math. Monthly* 110(1):1–24 (2003) — [Wikipedia](https://en.wikipedia.org/wiki/Hilbert%27s_twenty-fourth_problem), [Thiele MAA PDF](https://www.maa.org/sites/default/files/pdf/news_old/Thiele.pdf)
- **[S]** Thiele & Wos, "Hilbert's twenty-fourth problem", *J. Automated Reasoning* 29(1):67–89 (2002), doi:10.1023/A:1020537107897. A proof may be simpler "in one or more ways" including length, size ("total symbol count") and term structure. Wos argues the problem is "extremely amenable to attack with the appropriate automated reasoning program" — [Wikipedia](https://en.wikipedia.org/wiki/Hilbert%27s_twenty-fourth_problem)
- **[V]** Kinyon, "Proof Simplification and Automated Theorem Proving" (arXiv 1808.04251; **[M]** published in Phil. Trans. R. Soc. A, 2019):
  - Measures: length "is proof length, which is the number of inference steps"; weight is "the number of symbols which occur in it, not counting parentheses"; level is "the length of the longest chain of inferences"; maximum or average clause weight; "the complexity of the proof graph".
  - Trade-off: "the slightly shorter proof has a significantly higher maximum clause weight"; simplicity is "obviously context-dependent"; "the issue of proof simplicity is not so simple"; finding simpler proofs "seems to be something like a variational problem".
  - Method: Veroff's hints / proof sketches. Worked example: a Moufang identity proof in Prover9 cut from 470 to 150 steps by iterated hints with weight squeezing.

  [ar5iv 1808.04251](https://ar5iv.labs.arxiv.org/html/1808.04251)
- **[V]** Wernhard & Bibel, "Investigations into Proof Structures" (arXiv 2304.12827v3, 2024):
  - Definitions: "tree size" is the number of D (detachment) occurrences, which "Veroff [71] calls it CDcount"; "height" is what Veroff calls "level"; "compacted size", the number of distinct subproofs (minimal DAG), is what Veroff calls "length" (§3.2.2–3.2.3).
  - Measures disagree (Example 37): c-size(d)=8 > 7=c-size(e) but sc-size(d)=27 < 28=sc-size(e).
  - History (§6): Łukasiewicz's proof of Syll has compacted size 34 and tree size 585 (751 before simplification); Meredith's 1963 variation has compacted size 33 and tree size 669, so it is one step shorter as a DAG but larger as a tree. Fitelson & Wos' OTTER completion had length 36.
  - New result: SGCD finds compacted size 22, tree size 64, height 22. With "combinatory compression" it reaches "compacted size 19, height 15, but tree size 119".

  [arXiv PDF](https://arxiv.org/pdf/2304.12827)
- **[S]** Wos, Ulrich & Fitelson (XCB, equivalential calculus): an original 71-step proof was refined to 25; for two earlier shortest single axioms, "far shorter proofs, of lengths 19 and 21" — [arXiv cs/0211015](https://arxiv.org/pdf/cs/0211015)
- **[M]** Wos also wrote on the resonance strategy (JAR/CMA 1995) and "Conquering the Meredith single axiom" (JAR 2001); both are cited by Wernhard & Bibel. Wos's AAR newsletter columns on shortest proofs were not checked.

### Inferences
- Meredith's "abridgement" and the combinatory-compression result are clean, documented cases: optimizing one size measure (DAG length) makes another (tree size or expanded proof) worse. Lean proofs that call heavy automation are analogous — the surface script is short, the underlying proof term or check time is large.

### Gaps
- I did not read Thiele & Wos 2002 itself (paywalled), so quotes come via Wikipedia/snippets. Veroff's own pages on hints/proof sketches and on demodulation-based shortening were not fetched.

## Mathlib / Lean community: golf conventions, linters, automation tension

### Takeaway
Mathlib constrains golfing through style rather than a length metric. The flexible ("non-terminal simp") linter flags fragile chains in which a rigid tactic acts on the output of a flexible one such as `simp`. Reviewers have pushed back on golfs that trip it. A style rule for `grind` was still being worked out in early 2026.

### Cited Findings
- **[S]** The flexible linter flags a rigid tactic (e.g. `rw`) acting on the output of a flexible one (`simp`). Only `simp`/`simp_all` start a check. It leaves terminal `simp` alone; terminal simps are expected to stay unsqueezed (`simp`, not `simp only`). It is enabled with `set_option linter.flexible true`, and per a maintainer it was not on by default in Mathlib at the time of that thread — [FlexibleLinter docs](https://florisvandoorn.com/LeanCourse25/docs/Mathlib/Tactic/Linter/FlexibleLinter.html), [Zulip: flexible vs rigid tactics](https://leanprover-community.github.io/archive/stream/287929-mathlib4/topic/flexible.20vs.20rigid.20tactics.html), [Zulip: Non-terminal simps](https://leanprover-community.github.io/archive/stream/287929-mathlib4/topic/Non-terminal.20simps.html)
- **[S]** Reviewers flagged `simp at hl; simp [hl]` as fragile because "we may add a lemma in the future that changes the result of simp at hl". In one PR a reviewer asked a golfer to "reassure me that your linter does not flag this as a non-terminal simp" — [Zulip: request: discourage squeezing terminal simps?](https://leanprover-community.github.io/archive/stream/287929-mathlib4/topic/request.3A.20discourage.20squeezing.20terminal.20simps.3F.html), [mathlib4 PR 11259](https://github.com/leanprover-community/mathlib4/pull/11259)
- **[V]** Chris Henson, Zulip, 6 Feb 2026: "There has also been some recent discussion about when it is preferable not to use `grind`"; "it would be helpful to have a style guide entry about grind"; asks "if `grind only` is analogous to `simp only` for squeezing purposes" — [Zulip: style guidelines for grind](https://leanprover-community.github.io/archive/stream/287929-mathlib4/topic/style.20guidelines.20for.20grind.html)
- **[M]** Mathlib forbids `native_decide`, because it trusts the compiler via the `Lean.ofReduceBool` axiom. It enforces a default `maxHeartbeats` of 200000 per declaration, so local increases need justification. `decide` on large instances is discouraged for performance. Not verified this session.

### Inferences
- These rules mean a "golf" in Mathlib must also stay robust to library changes and within heartbeat budgets. An LLM optimizer that only minimizes tokens would routinely produce non-terminal `simp`, `grind`-everything or `decide`-heavy proofs that reviewers would reject.

### Gaps
- I did not read the official Mathlib style guide or the linter talk PDF ([Lean Together 2025 linter talk](https://leanprover.zulipchat.com/user_uploads/3121/rZqRNaxs1Es4wBZH3JD9YW7j/2025_Lean_Together_linter_talk.pdf)). I found no specific Zulip thread quantifying check-time regressions caused by golfs.

## Isabelle Sledgehammer, proof compression, cut-introduction and library learning

### Takeaway
Classical tools minimize dependencies and steps (Sledgehammer's fact minimization and Isar compression), compress proof structure by introducing lemmas or cuts (Hetzl/GAPT, exponential compression in theory), and audit LLM library learning. The audits found that "learned lemmas" are almost never reused, so the claimed compression is not real.

### Cited Findings
- **[S]** Sledgehammer's Isar minimizer (Smolka & Blanchette) does "Minimize dependencies (used facts) of Isar proof steps": it retries already-preplayed steps with fewer facts and a small time slack, and leaves steps that time out alone. A later version removes unreferenced steps. Preplay tests generated steps for a few seconds so users get no proofs that "fail or run too long" — [sledgehammer_isar_minimize.ML](https://isabelle.in.tum.de/repos/isabelle/file/43473497fb65/src/HOL/Tools/Sledgehammer/sledgehammer_isar_minimize.ML), [Smolka & Blanchette, "Robust, Semi-Intelligible Isabelle Proofs from ATP Proofs"](https://wwwbroy.in.tum.de/~blanchet/isar.pdf)
- **[S]** Hetzl, Leitsch, Reis & Weller invert cut-elimination: they compress the terms in a cut-free proof with a tree grammar, find cut formulas that realize the compression, and get "an exponential compression of quantifier complexity". It is extended to Π2-cuts and implemented in GAPT — [arXiv 1401.4330](https://arxiv.org/pdf/1401.4330.pdf), [arXiv 1611.08208](https://arxiv.org/pdf/1611.08208v2), [Hetzl research page](https://dmg.tuwien.ac.at/hetzl/research/index.html)
- **[S]** "Library Learning Doesn't" (Berlot-Attwell, Rudzicz, Si; arXiv 2410.20274, NeurIPS 2024 workshop): in LEGO-Prover's released logs "exactly one lemma is reused", once. An ablation that blocks reuse performs similarly. TroVE's gains come from self-correction and self-consistency, not reuse. The follow-up "LLM Library Learning Fails: A LEGO-Prover Case Study" (arXiv 2504.03048; EACL 2026) finds "no evidence of the direct reuse of learned lemmas", and accuracy gains vanish once compute cost is accounted for — [arXiv 2410.20274](https://arxiv.org/abs/2410.20274v1), [arXiv 2504.03048](https://fugumt.com/fugumt/paper_check/2504.03048v1_enmode), [EACL 2026 PDF](https://preview.aclanthology.org/ingest-eacl/2026.eacl-long.163.pdf)

### Inferences
- The library-learning audits matter for length metrics: a system can look as if it compresses (it produces "lemmas") without reusing anything. Any lemma-extraction approach to shortening should count lemma statements and their proofs in the size, as Wernhard & Bibel's compacted vs tree size distinction does.

### Gaps
- Not covered in this pass: Coq/Rocq proof minimization tools, HOL Light proof compression, resolution/DRAT proof compression (e.g. RecyclePivots, LowerUnits, drat-trim core extraction), and TacMiner. Hetzl's "On the Generation of Quantified Lemmas" (Ebner et al. 2018) was not opened.
