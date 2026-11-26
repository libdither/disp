# How a net or graph reads a net or graph: mechanisms in the interaction-net and graph-rewriting literature

How to read these notes.

- Every item under "Cited Findings" has: the claim in plain words, an exact quote, and the link printed by
  `scripts/verify-source.sh -q`. Every quote below was found by the script (137 quotes checked, 0 misses;
  not all are used). Page numbers in link labels are the paper's own where known, otherwise "PDF p.".
- Anything about the user's own net is under "Inferences" and refers to
  `research/research_notes/Reading code in interaction nets/this_net_measured.md` ("the measured notes").
- Sources the script could not check are listed only in the last section, "Not verified".
- Words used throughout. *Agent*: a node of an interaction net; it has one *principal port* and some
  auxiliary ports. *Active pair*: two agents joined by their principal ports; the only place a rewrite
  happens. *Interaction combinators*: Lafont's three-agent system (constructor, duplicator, eraser).
  *Package*: a reduced net with one free port. *Box*: a marked-off sub-net that is copied, erased or opened
  as a unit (from linear logic). *Read-back*: turning a net back into a term. *Token*: a marker that walks
  a term or graph.

## 1. Has anyone built reflection, quoting, intensional operations or a self-interpreter inside interaction nets?

### Takeaway
No. Eleven searches found no paper or system in which a net inspects another net's code and gets the answer
back as data it can branch on. What exists are four partial pieces: (a) Lafont's "code" of a net, which can be
copied and opened but not inspected; (b) his "menu and selector", a case built from combinators; (c) one
read-back done by agents inside the net (Salikhmetov), whose result is text held in an agent's name; (d)
agents that carry a whole net as an opaque value (Fernández–Mackie–Pinto), where letting the outer net and
the carried net mix is left as future work.

### Cited Findings

**Lafont's translation: menus (a case) and codes (a quote that can only be opened)**

- Any interaction system can be compiled into the three combinators. This is a translation done before
  running, not an interpreter that takes a net as data. "Theorem 1. Any interaction system can be translated
  into the system of interaction combinators." —
  [Lafont 1997, p. 82](https://dcreager.net/pdf/Lafont1997.pdf#page=14)
- "Universal" means exactly that: "a universal interaction system has the property that any other
  interaction system can be translated into it" — [Lafont 1997, p. 70](https://dcreager.net/pdf/Lafont1997.pdf#page=2)
- What a package is: "A reduced net with only one free port is called a package" —
  [Lafont 1997, p. 80](https://dcreager.net/pdf/Lafont1997.pdf#page=12)
- Menu and selector = an n-way case in pure combinators. n packages are bundled into one, and a selector
  net pulls one out: a menu is a package "from which the original ones can be extracted by means of principal
  nets" (the selectors) — [Lafont 1997, p. 84](https://dcreager.net/pdf/Lafont1997.pdf#page=16)
- The packages not chosen are erased, cell by cell, by eraser agents: "One uses the fact any package … can
  be erased as follows" — [Lafont 1997, p. 84](https://dcreager.net/pdf/Lafont1997.pdf#page=16)
- In the translation, each cell carries a package that "represents the finite tree of all possible futures
  of a cell" — [Lafont 1997, p. 87](https://dcreager.net/pdf/Lafont1997.pdf#page=19)
- A duplicator cannot copy a net that contains duplicators, so arbitrary packages cannot be copied: "Now to
  cope with this impossibility of duplicating arbitrary packages" —
  [Lafont 1997, p. 88](https://dcreager.net/pdf/Lafont1997.pdf#page=20)
- The fix is a code: for any package π there is a package !π (the code of π) "which can be duplicated, and
  from which … can be extracted" by a fixed decoder net —
  [Lafont 1997, p. 88](https://dcreager.net/pdf/Lafont1997.pdf#page=20)
- The code is what lets a cell rebuild its own rules again and again: it "plays the role of a genetic code"
  — [Lafont 1997, p. 90](https://dcreager.net/pdf/Lafont1997.pdf#page=22)
- Mazza restates the construction for the symmetric combinators. A net "can be recovered from its Lafont
  code by means of a" universal decoder (one decoder for all nets) —
  [Mazza 2009, p. 15](https://arxiv.org/pdf/0906.0380#page=15)
- The code is inert and copyable: "the code of a net is cut-free and does not contain" duplicator cells, so
  "it can be freely duplicated by means of trees of" duplicator cells — [Mazza 2009, p. 16](https://arxiv.org/pdf/0906.0380#page=16)
- Without a code, only nets with no active pairs can be copied: "we are only able to duplicate cut-free
  nets" — [Mazza 2009, p. 15](https://arxiv.org/pdf/0906.0380#page=15)

**Read-back done by agents inside the net (the one known case)**

- Salikhmetov adds a read agent and "atom" agents to an optimal-reduction net. Normally read-back is done
  from outside: "only facilities external with respect to interaction nets are available" —
  [Salikhmetov 2016, p. 52](https://arxiv.org/pdf/1609.03644#page=8)
- "Here we embed the read-back mechanism into interaction nets themselves" —
  [Salikhmetov 2016, p. 52](https://arxiv.org/pdf/1609.03644#page=8)
- What the read produces is text, not a net: "we dissolve application and abstraction agents into textual
  representation" of the λ-term — [Salikhmetov 2016, p. 52](https://arxiv.org/pdf/1609.03644#page=8)
- The text lives in agent names, so the set of agents is infinite: "there has to be an agent for every"
  λ-term and every context — [Salikhmetov 2016, p. 52](https://arxiv.org/pdf/1609.03644#page=8)
- It is three rules: "Our read-back mechanism mainly consists of the three interaction rules" —
  [Salikhmetov 2016, p. 53](https://arxiv.org/pdf/1609.03644#page=9)
- The author knows of no other: "we are not aware of any other existing implementations of read-back
  embedded into interaction nets" — [Salikhmetov 2016, p. 54](https://arxiv.org/pdf/1609.03644#page=10)

**Agents that hold a net as a value**

- Fernández, Mackie and Pinto let an agent carry a program in an outside language, and take the outside
  language to be interaction nets: "GIN(IN) uses interaction nets as external language" —
  [Fernández–Mackie–Pinto 2001, p. 11](https://web.archive.org/web/20251016073740id_/https://core.ac.uk/download/pdf/55602454.pdf#page=11)
- "think of agents as boxes containing interaction nets" —
  [Fernández–Mackie–Pinto 2001, p. 11](https://web.archive.org/web/20251016073740id_/https://core.ac.uk/download/pdf/55602454.pdf#page=11)
- Rules cannot look inside the carried value: "we cannot interact directly with the contents of the agents"
  — [Fernández–Mackie–Pinto 2001, p. 8](https://web.archive.org/web/20251016073740id_/https://core.ac.uk/download/pdf/55602454.pdf#page=8)
- Mixing the two levels (an outside program that returns a net) is named but not built: a "Higher-Order
  Interaction Net System, which is the subject of future work" ("which is the subject of future work") —
  [Fernández–Mackie–Pinto 2001, p. 11](https://web.archive.org/web/20251016073740id_/https://core.ac.uk/download/pdf/55602454.pdf#page=11)

**An agent that can meet any agent**

- Jiresch's generic rules let one named agent interact with any partner: "Generic rules allow one concrete
  agent to interact with an arbitrary agent" — [Jiresch 2012, p. 15](https://arxiv.org/pdf/1211.4102#page=4)
- He calls this higher-order: "This arbitrary, generic agent corresponds to a function variable, adding a
  higher-order character to interaction nets" — [Jiresch 2012, p. 15](https://arxiv.org/pdf/1211.4102#page=4)
- A specific rule wins over the generic one, so "any agent" works as a default arm: "ordinary rules always
  have priority over generic rules" — [Jiresch 2012, p. 17](https://arxiv.org/pdf/1211.4102#page=6)

### Inferences

- The user's `ρ` (reader agent `R` plus a five-way view) has no published counterpart inside interaction
  nets. The nearest relatives: Lafont's decoder opens a code but returns the net itself, never a description
  of it; Salikhmetov's reader walks a net but returns text in an agent name that no rule can take apart.
- The view "λk1…k5. ki …" is Lafont's menu-and-selector idea written with λ agents: the program supplies
  five arms, one is selected, the others are erased. Lafont's version pays the same erase cost (each
  unchosen package is erased cell by cell), which matches the measured "Erasing the arms not chosen is paid
  for each agent in them" (measured notes, section "What one read costs").
- Lafont's code !π and the user's inert code agents solve the same problem (keep a body from reducing and
  make it copyable) in opposite ways. A code is a different net that must be decoded before it runs; the
  user's code is the function body itself, turned into running agents by the thaw agent. Neither makes
  copying cheaper than one rewrite per agent.
- A self-interpreter in pure interaction combinators would have to work on codes (the only copyable form
  of an arbitrary net) and would need a way to branch on the structure of a code. Lafont's decoder gives no
  such branch. This is the missing piece that `ρ` supplies in the user's system.

#### All mechanisms found, at a glance

| mechanism | what reads what | cost of one read | what it can and cannot see | done inside an interaction net? |
|---|---|---|---|---|
| ordinary rule against a constructor or code agent (§3) | a function/case agent meets the top agent | 1 rewrite | the top symbol and its wires; nothing deeper | yes, the standard way (Inpla, HVM1, PIN) |
| nested pattern (§3) | one written rule matches several levels | 1 rewrite per level after compiling | a fixed finite depth | yes (PIN, Inpla) |
| generic / wildcard rule (§1, §3) | one agent meets any agent | 1 rewrite | can copy, erase or pass the partner on; learns its name only if a specific rule exists | yes (inets, Inpla) |
| menu + selector (§1) | selector picks 1 of n packages | selection, then erasing n−1 packages cell by cell | picks by position; sees nothing inside | yes, pure combinators |
| code + decoder (§1) | decoder opens a copyable code | proportional to the net (not stated as a count) | copy, erase, open; no inspection | yes, pure combinators |
| box / package around a function body (§2) | copier or eraser meets the box | still one rewrite per agent inside | only whether it is closed and in normal form | yes (YALE, Mackie–Pinto) |
| evaluation token (§2) | token agent walks inert term agents | 1 rewrite per visited node | turns code into running agents; reports nothing | yes (Sinot; Almeida et al.) |
| embedded read-back (§1, §5) | read agent walks a normal form | 1 rewrite per node | builds text inside an agent name; the net cannot take it apart | yes, once (Salikhmetov) |
| agent holding an outside value (§3) | a rule calls outside code on the value | varies with the outside code | only what the outside functions return | yes (GIN(L), Ivy, Inpla, HVM2 numbers) |
| reference to a shared definition (§4) | a pointer agent stands for a whole net | copy 1, erase 1, expand 1 rewrite (expand copies the body in one pass) | static closed code only; no inspection | yes (HVM2, Ivy) |
| read-back from outside (§5) | the implementer walks the final graph | believed linear in the size of the term read, unproved | everything, but the program cannot use it | no |
| token machine (§5) | a token walks a graph that never changes | no rewrites; time can be exponentially worse | computes the answer, never changes or copies the net | no (the token is machine state) |
| observation by a context (§5) | a net plugged into another net | n/a | behaviour only, never syntax | n/a (theory of what can be told apart) |
| rules that are graphs (§6) | a rule matches a sub-graph, rules can match rules | sub-graph matching, not constant time | whatever the pattern names | no (port graphs) |
| hierarchical graphs, frame variables (§6) | a rule variable stands for a whole nested graph | 1 rule application | copies/removes without looking inside | no (but "binets" extend interaction nets this way) |
| nets as tokens (§6) | outer net and inner net fire a shared event | 1 synchronised event | only the events the inner net offers | no (Petri nets) |
| reflective Petri nets (§6) | meta-level net reads a copy of the base net kept as tokens | copy refreshed on every base step | whole structure and state | no (Petri nets) |

### Gaps

- No source found for: a quote or reflection primitive, an intensional operator, or a self-interpreter
  written inside interaction nets or interaction combinators. Searches run (WebSearch), none returned a
  relevant primary source:
  1. `reflection OR "self-interpreter" OR metacircular "interaction nets"`
  2. `"interaction combinators" "self-interpreter" OR "quote" OR "reflective" OR "intensional"`
  3. `"interaction nets" OR "interaction combinators" reflection quote "code of a net" OR "self-interpretation" OR "universal net" encode net as data inspect agents` (extended mode)
  4. `"interaction nets" "self-interpreter"`
  5. `"interaction nets" intensional OR "intensionality" OR "quotation" lambda calculus "inspect" code as data net`
  6. `"interaction combinators" interpreter written in interaction combinators "universal" net "encodes" nets as data arXiv`
  7. `HVM OR Bend OR "Interaction Calculus" Taelin reflection OR quote OR "first-class" net inspect "interaction combinators" "self-hosting" OR "metacircular"`
  8. `"interaction nets" "reflective" OR "reflection" net inspects itself agents "meta-level" interaction net rules as nets`
  9. `"higher-order" "interaction nets" nets as data "agents containing nets" OR "nets as values" Fernández Mackie Pinto "higher-order calculus for graph transformation"`
  10. `"interaction nets" "hash-consing" OR "hash consing" OR "maximal sharing" inert code shared copy constant time`
  11. `GP 2 graph programs "reflection" OR "meta-programming" OR "self-interpreter" OR "rules as graphs" Plump`
- "Not found" is a search result, not a proof of absence. Theses and workshop papers that are not indexed
  could exist.
- How exactly Lafont wires a menu into the translation of a cell is in his Figures 6, 7 and 10, which the
  text extraction does not contain. The description above uses only his prose.
- The higher-order interaction-net system that Fernández, Mackie and Pinto announce as future work
  ("A higher-order calculus for graph transformation") was not located.

## 2. Token-passing nets, closed reduction and boxes: are thaw and reader agents tokens, and how is a body kept from being reduced or copied piecemeal?

### Takeaway
The thaw agent is an evaluation token in exactly the sense of Sinot and of Almeida–Pinto–Vilaça: an agent
that walks inert "syntactic" agents and turns them into "computation" agents, never going under a binder.
The reader agent is not a token; it is a one-shot case. Boxes and closed reduction stop a body from being
copied while it is open or unevaluated, but in every such system the copy itself is still one rewrite per
agent.

### Cited Findings

**Token-passing nets**

- The token walks the term's syntax: "Reduction strategies are encoded through the use of an evaluation
  token that traverses the syntactical representation of the term" —
  [Almeida–Pinto–Vilaça 2007, p. 3](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=3)
- Two kinds of agent for each term constructor, an inert one and a running one. The token turns one into
  the other: "transforming the syntactic agent a into the computational" application agent —
  [Almeida–Pinto–Vilaça 2007, p. 4](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=4)
- What a token rule does to an inert agent: "They may activate a syntactic agent in order to make
  computational steps possible" — [Almeida–Pinto–Vilaça 2007, p. 8](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=8)
- Under weak reduction the token never goes under a binder: activation rules do not put token "agents under
  binding constructors, corresponding to canonical forms" —
  [Almeida–Pinto–Vilaça 2007, p. 9](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=9)
- Why this makes copying safe: "duplication is restricted to nets that correspond to syntactical
  representations of terms" (trees of inert agents) —
  [Almeida–Pinto–Vilaça 2007, p. 3](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=3)
- Copying breaks for open functions: there is a mismatch between copier "agents that occurs when open
  abstractions are copied" — [Almeida–Pinto–Vilaça 2007, p. 6](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=6)
- Results are inert code again: "This guarantees that normal forms do not contain tokens and are thus
  syntactical nets" — [Almeida–Pinto–Vilaça 2007, p. 9](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=9)
- Sinot allows one token only; Almeida et al. allow several: "Sinot imposes a linearity restriction on the
  evaluation token, which forces evaluation to proceed sequentially" —
  [Almeida–Pinto–Vilaça 2007, p. 2](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=2)
- Sinot's own abstracts: the token is "an explicit syntactic object to represent the flow of evaluation"
  ("introducing an explicit syntactic object to represent the flow of evaluation") —
  [Sinot 2006, ENTCS abstract](https://web.archive.org/web/20240416005138/https://www.sciencedirect.com/science/article/pii/S1571066106000934#aep-abstract-sec-id6)
- No boxes are needed: "these encodings do not need any notion of box" —
  [Sinot 2005, TLCA abstract](https://web.archive.org/web/20231210100328/https://link.springer.com/chapter/10.1007/11417170_28#Abs1-content);
  "no artifact to represent boxes is needed" —
  [Sinot 2006, ENTCS abstract](https://web.archive.org/web/20240416005138/https://www.sciencedirect.com/science/article/pii/S1571066106000934#aep-abstract-sec-id6);
  "Another benefit of this approach is that no artifact is needed to represent boxes" —
  [Sinot 2006, MSCS abstract](https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/abs/callbyneed-in-tokenpassing-nets/D5113CA58AD7B5E9D434288357A83870#sec0)
- Strategies can be put in the rules themselves: "Interaction nets were found as capable to encode reduction
  strategies directly" — [Salikhmetov 2016, p. 45](https://arxiv.org/pdf/1609.03644#page=1)

**Boxes and closed reduction**

- YALE (Mackie's evaluator) keeps a function body in a box: "the body of the abstraction is encapsulated in
  a box structure" ("is encapsulated in a box structure") —
  [Mackie 1998, p. 121](https://web.archive.org/web/20240724225823id_/https://dl.acm.org/doi/pdf/10.1145/291251.289434#page=5)
- The box keeps a list of the body's free variables: "the coding contains a pointer to all the free
  variables of the abstraction" —
  [Mackie 1998, p. 121](https://web.archive.org/web/20240724225823id_/https://dl.acm.org/doi/pdf/10.1145/291251.289434#page=5)
- Copying is allowed only for closed values: "Terms can only be copied when there are no free variables",
  and only in weak normal form —
  [Mackie 1998, p. 120](https://web.archive.org/web/20240724225823id_/https://dl.acm.org/doi/pdf/10.1145/291251.289434#page=4)
- Pinto's summary of YALE: "it captures locally the notion of an exponential box" by boundary agents, and
  "The encoding requires garbage collection of the boundary agents when a box is opened" —
  [Pinto 2003, PDF p. 13](https://web.archive.org/web/20240725040603id_/https://core.ac.uk/download/pdf/55603027.pdf#page=13)
- Closed reduction (the calculus behind it): "only closed substitutions can be moved through certain
  constructs" — [Fernández–Mackie–Sinot 2005, abstract](https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/abs/closed-reduction-explicit-substitutions-without-alphaconversion/340CB747EE4CFF8D58F0B3E79F53EDDA#sec0)
- Its interaction-net implementation exists: "Closed reduction is a very efficient reduction strategy for the
  lambda calculus" — [Mackie 2008, abstract](https://web.archive.org/web/20210506225947/http://link.springer.com/chapter/10.1007/978-3-642-24452-0_3#Abs1).
  Mackie's 2004 evaluator claims "we obtain one of the most efficient implementations of this kind to date"
  — [Mackie 2004, abstract](https://web.archive.org/web/20220308033900/https://link.springer.com/chapter/10.1007/978-3-540-25979-4_11#Abs1)
- In pure combinators a box is a package with three abilities: "Such a net must have the possibility to be
  duplicated, erased and opened" —
  [Mackie–Pinto 2002, p. 15](https://repositorium.uminho.pt/server/api/core/bitstreams/f3f5ad5e-9466-4ddf-8869-14d34ea18782/content#page=15)
- For call by value the function is what gets boxed: "it is the function which is packaged, thus erasing and
  copying are not possible under an abstraction" —
  [Mackie–Pinto 2002, p. 37](https://repositorium.uminho.pt/server/api/core/bitstreams/f3f5ad5e-9466-4ddf-8869-14d34ea18782/content#page=37)
- Copying and erasing agent by agent is the declared design of interaction nets, not an accident:
  "weakening should gradually erase a proof and contraction should gradually copy a proof" —
  [Mackie–Pinto 2002, p. 2](https://repositorium.uminho.pt/server/api/core/bitstreams/f3f5ad5e-9466-4ddf-8869-14d34ea18782/content#page=2)
- and "external machinery to copy or erase nets is absent from the system" —
  [Mackie–Pinto 2002, p. 2](https://repositorium.uminho.pt/server/api/core/bitstreams/f3f5ad5e-9466-4ddf-8869-14d34ea18782/content#page=2)
- A system where one agent stands for a whole proof and one interaction does everything is possible but
  rejected, because it hides the cost: "each proof is represented by an agent and an interaction with this
  agent will replace it with another agent"; "if additional (external) work is required, then this will
  obscure this measure" —
  [Mackie–Pinto 2002, p. 3](https://repositorium.uminho.pt/server/api/core/bitstreams/f3f5ad5e-9466-4ddf-8869-14d34ea18782/content#page=3)
- Weak reduction as a strategy on the net as a whole: "This strategy never triggers reductions in
  disconnected parts of the net" —
  [Pinto 2003, PDF p. 1](https://web.archive.org/web/20240725040603id_/https://core.ac.uk/download/pdf/55603027.pdf#page=1)
- Inpla has the same as a switch (`-w`): "only connections that have interface names (thus, live and occur
  once) are evaluated" —
  [Inpla, Gentle introduction](https://raw.githubusercontent.com/inpla/inpla/HEAD/Gentle_introduction_Inpla.md#weak-reduction-strategy)

### Inferences

- Thaw = evaluation token. The measured notes describe `T ⋈ app` making a running application `O` with a
  `T` on each part, and `T ⋈ lam` leaving the function untouched. That is Almeida et al.'s activation rule
  for application and canonical rule for abstraction, with `app`/`lam`/`var` as syntactic symbols and `O` as
  the computation symbol. Several thaw agents at once is their variant, not Sinot's single token.
- The user's design note that weak evaluation is "what makes one copier enough" is the same argument as "duplication is
  restricted to … syntactical representations of terms": only inert trees are ever copied.
- The reader `R` is not a token. A token walks on and re-emits itself; `R` fires once at the top agent and
  is consumed. It is a destructor in the ordinary interaction-net sense (§3), with five rules, one per kind
  of code agent.
- The user's design needs no box for the same reason Sinot's does not: inert agents cannot react until a
  token reaches them, so the body is protected by being inert, not by a boundary.
- None of the box or closed-reduction systems reduce the 97% copy/erase share. They decide *when* a body
  may be copied (closed, in normal form), and then copy it one agent at a time. The literature treats that
  as the honest cost measure.

### Gaps

- The bodies of Sinot's two 2005/2006 papers, of Fernández–Mackie–Sinot 2005 and of Mackie 2004 could not
  be fetched (only abstracts). Rule-level details of those systems are taken from Almeida et al., Pinto and
  the YALE paper.
- No rewrite counts were found that split a token-passing run into "token moves" versus "copy/erase", so
  there is no published number to set beside the user's 97%.

## 3. Richer matching instead of a built view: nested patterns, constructor and case agents, external values

### Takeaway
In interaction-net languages a case on a constructor is one rewrite: the function agent meets the
constructor agent and the pair of names picks the rule. Nested patterns are compiled back to one rewrite per
level. Agents can carry outside values that rules cannot look into. Even with a native case, the arm not
taken is still erased agent by agent, unless the arms are references (§4).

### Cited Findings

- The base rule: only two agents meet at a time. "Interaction nets have a very primitive notion of pattern
  matching since only two agents can interact at a time" —
  [Hassan–Jiresch–Sato 2010, p. 13](https://arxiv.org/pdf/1003.4562#page=1)
- So deeper matches need helper agents: "many auxiliary agents and rules are needed to implement more
  sophisticated matches" — [Hassan–Jiresch–Sato 2010, p. 13](https://arxiv.org/pdf/1003.4562#page=1)
- Nested patterns are an add-on that changes nothing underneath: "a conservative extension of interaction
  rules that allows nested pattern matching" — [Hassan–Jiresch–Sato 2010, p. 14](https://arxiv.org/pdf/1003.4562#page=2)
- They are compiled away into ordinary rules (ORN = ordinary rules): "These rules are compiled into the set
  of ORN rules given in Figure 1" — [Hassan–Jiresch–Sato 2010, p. 17](https://arxiv.org/pdf/1003.4562#page=5)
- Sato's 2024 version adds conditions on values and keeps the same property: "by introducing a translation
  to non-nested matching, this extension is conservative in rewriting" —
  [Sato 2024, p. 1](https://arxiv.org/pdf/2410.00540#page=1)
- Agents can hold integers: "Attribute values are integers and are written in parentheses following agent
  symbols" — [Sato 2024, p. 4](https://arxiv.org/pdf/2410.00540#page=4)
- Inpla (Sato's interpreter) lets a rule match "anything else": "we can place a variable as a default match
  on the left or right side of an active pair" —
  [Inpla, Gentle introduction](https://raw.githubusercontent.com/inpla/inpla/HEAD/Gentle_introduction_Inpla.md#wildcard-agent-in-rule-definitions)
- Lookup order is user rules, built-in rules, then "Finally, rules containing the wildcards" —
  [Inpla, Gentle introduction](https://raw.githubusercontent.com/inpla/inpla/HEAD/Gentle_introduction_Inpla.md#wildcard-agent-in-rule-definitions)
- Inpla's built-in eraser and copier work on any agent, step by step: the eraser "disposes any nets
  gradually" — [Inpla](https://raw.githubusercontent.com/inpla/inpla/HEAD/Gentle_introduction_Inpla.md#eraser);
  the copier "duplicates any agents" —
  [Inpla](https://raw.githubusercontent.com/inpla/inpla/HEAD/Gentle_introduction_Inpla.md#duplicator)
- Jiresch's variadic agents make "any agent" precise: "They correspond to any agent with any number of
  ports" — [Jiresch 2012, p. 15](https://arxiv.org/pdf/1211.4102#page=4). Erasing with one such rule still
  goes agent by agent: the eraser propagates, "deleting connected agents in subsequent steps" —
  [Jiresch 2012, p. 16](https://arxiv.org/pdf/1211.4102#page=5)
- HVM1 had constructors as first-class nodes, taken apart by rules written by the user: "constructors
  (eliminated by user-defined rewrite rules)" —
  [HVM1, HOW.md](https://raw.githubusercontent.com/HigherOrderCO/HVM1/master/guide/HOW.md#hvms-core-language-overview);
  "User-defined rules are generated from equations in a file" —
  [HVM1, HOW.md](https://raw.githubusercontent.com/HigherOrderCO/HVM1/master/guide/HOW.md#user-defined-rules)
- HVM2 has no named data constructors. Its node kinds are fixed: "The first three node types (ERA, REF,
  NUM) are nullary" and "the last 4 types (CON, DUP, OPE, SWI) are binary" —
  [Taelin 2024, p. 3](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=3).
  Matching on data goes through the plain annihilation rule (data is encoded as functions): annihilation
  "is used to implement beta-reduction and" pattern-matching —
  [Taelin 2024, p. 7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- HVM2 keeps one native case, on numbers, as a single rule: "If n is 0, we return the zero case, and erase
  the succ case" — [Taelin 2024, p. 7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- Why numbers are native: "Emulating these operations with IC constructs analogous to Church or Scott
  Numerals would be very inefficient" —
  [Taelin 2024, p. 5](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=5)
- Ivy (the net language under Vine) has outside values and a native two-way branch on them: "extrinsic
  values are nilary agents that represent external entities"; "extrinsic branches are ternary agents that
  represent a boolean query on an external entity" —
  [Ivy docs, Interaction System](https://vine.dev/docs/ivy-ivm/interaction-system) (quotes found by the
  script; the nearest page anchor is the sidebar, so the link is to the page)
- The Ivy branch erases the arm not taken with an eraser: "one of the first two auxiliary wires is linked
  to the third, and the other is connected to an eraser" —
  [Ivy docs, Branch rule](https://vine.dev/docs/ivy-ivm/interaction-system#call) (the Branch heading has no anchor; `#call` is the heading just before it)
- Outside values in general (Fernández–Mackie–Pinto): the cost of a rewrite stops being constant,
  "interaction steps have variable cost, depending on the external code to be executed" —
  [Fernández–Mackie–Pinto 2001, p. 11](https://web.archive.org/web/20251016073740id_/https://core.ac.uk/download/pdf/55602454.pdf#page=11);
  and rules cannot look inside, "we cannot interact directly with the contents of the agents" —
  [same, p. 8](https://web.archive.org/web/20251016073740id_/https://core.ac.uk/download/pdf/55602454.pdf#page=8)

### Inferences

- A case on a constructor costs one rewrite in PIN, Inpla, HVM1 and any language with named agents, because
  an interaction rule is indexed by the two names that meet. This follows from "only two agents can interact
  at a time"; none of the sources states "one rewrite" as a sentence.
- The user's `R ⋈ lam`, `R ⋈ app`, `R ⋈ var`, `R ⋈ Lam`, `R ⋈ rho` already are such a case: the code agents
  act as constructors and `R` as the function agent. What is unusual is the right-hand side. It does not
  continue into a chosen arm; it builds a 17-agent λ-encoded value that then takes 20 more rewrites to run
  (measured notes). A case agent holding the five arms would be the standard interaction-net form and one
  rewrite, as the measured notes already say.
- A native case does not remove the erase cost. HVM2's switch and Ivy's branch both hand the unchosen arm
  to an eraser, which then works through it agent by agent, unless the arm is a single reference agent (§4).
  The measured 4 to 16 erases per read for four small arms, and far more for large ones, would stay.
- Nested patterns would let one written rule read, say, an application whose function part is a function.
  After compiling it is still one rewrite per level, so it saves writing, not rewrites.
- Generic rules and wildcards give a default arm ("any other agent"). They cannot report which agent was
  met, so they cannot replace the five specific `R` rules.

### Gaps

- The original nested-pattern paper (Hassan–Sato 2008) could not be fetched; its content is taken from the
  2010 follow-up by the same authors.
- How Bend and Vine compile algebraic data types and `match` (Scott-style functions versus labelled
  combinators) was not verified: the Bend documentation of the HVM2 era is no longer at its old address
  (the repository now holds a different language, "Bend 2"), and the Vine docs page read covers only Ivy.
- No published count of rewrites for a case whose arms are λ-encoded versus a case agent was found; the
  user's 21 versus 1 is the only number.

## 4. Shared immutable code: references to definitions

### Takeaway
HVM2 and Ivy both have a zero-port agent that stands for a whole named net. It is copied in one rewrite and
erased in one rewrite without touching the definition, and expanded in one rewrite when something needs to
look at it. The catch in both: the definition is static, closed code written before the run, and the cheap
copy is allowed only under a no-interference condition. No interaction-net system was found that shares
run-time code by hashing.

### Cited Findings

**HVM2 (`REF` nodes and the "book" of definitions)**

- A reference is a new kind of agent: REF nodes "represent an immutable net that is expanded in a single
  interaction" ("they represent an immutable net that is expanded in a single interaction") —
  [Taelin 2024, p. 5](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=5)
- The call rule "Expands a REF, replacing it with its definition", copying the stored net in one go: "The
  definition is essentially copied from the static Book to the global memory" — [Taelin 2024, p. 7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- The stated reason is the cost of copier agents: "This operation is key to enable fast function
  application", since otherwise duplicator nodes would be needed —
  [Taelin 2024, p. 7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- One pass beats agent-by-agent copying: "Cloning large networks is faster through the call interaction, as
  it can be done in a single pass" —
  [Taelin 2024, p. 7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- A reference is a zero-port node: "The first three node types (ERA, REF, NUM) are nullary" —
  [Taelin 2024, p. 3](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=3)
- Erasing a reference is one rewrite and does not expand it. The void rule "Erases two nullary nodes
  connected to each other" — [Taelin 2024, p. 7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- Copying a reference is one rewrite: "the erase rule actually behaves as a copy operation, cloning the NUM
  or REF" — [Taelin 2024, p. 7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- The condition: only if the definition has no copier nodes inside. "when a copy operation is applied to a
  REF which contains DUP nodes, it instead is computed as a normal call operation" —
  [Taelin 2024, p. 7](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=7)
- The condition is a stored flag: a duplicator meeting a reference "will just copy the REF port, rather than
  expanding the definition, when it is safe" —
  [Taelin 2024, p. 16](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=16)
- References are how case arms are made cheap to drop: "Since REFs unfold lazily, the program above will
  properly erase the foo_S branch" —
  [Taelin 2024, p. 23](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=23)
- A general limit of HVM2 on copying functions: "A higher-order lambda that clones its variable can not be
  cloned" — [Taelin 2024, p. 22](https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf#page=22)

**Ivy / IVM (global nets)**

- Same idea: "A global agent expands into the corresponding global net when necessary during interaction"
  — [Ivy docs, Interaction System](https://vine.dev/docs/ivy-ivm/interaction-system) (found by the script;
  nearest anchor is the sidebar)
- Expanding leaves the other agent alone: "The global agent is simply replaced with the corresponding
  global net" — [Ivy docs, Expand rule](https://vine.dev/docs/ivy-ivm/interaction-system#expand)
- Copy without expanding, with a sharper condition than HVM2's: the copy rule applies when the other agent
  is "a combinator of a label that does not appear in the global net" (or in any global net it refers to)
  — [Ivy docs, Copy rule](https://vine.dev/docs/ivy-ivm/interaction-system) (nearest anchor is the sidebar)
- Erase without expanding: "When two nilary agents interact, they are erased, unless one is an extrinsic
  value and the other is a global agent" —
  [Ivy docs, Erase rule](https://vine.dev/docs/ivy-ivm/interaction-system) (nearest anchor is the sidebar)

**References versus copies, outside interaction nets**

- Valk's nets-as-tokens names the same choice: the reference reading of a token, where copies "are nothing
  but references to the same object" —
  [Valk 1998, PDF p. 18](https://www2.informatik.uni-hamburg.de/tgi/publikationen/public/data/1998/Valk98/Valk98.pdf#page=18)

### Inferences

- Costs in HVM2, read off the quotes: copy a reference 1 rewrite, erase 1 rewrite, expand 1 rewrite whose
  work grows with the size of the definition. For inert code this is exactly "copying and erasing code O(1)"
  from the user's `DESIGN.md`, obtained by naming code instead of hashing it.
- Why the no-copier ("safe") condition exists: a definition's own copier agents would get tangled with the
  copier that is copying it. Ivy states the precise form: the two must use different labels. In the user's
  net there is one copier and code is inert, so inert code contains no active copier at all; the condition
  would hold for every function body. This is an inference from the measured notes, not something tested.
- What does not carry over directly: HVM2 and Ivy definitions are closed and fixed before the run. The
  user's functions capture values ("a variable use holding a captured value"), and each recursive call
  copies "the whole recursive function (with everything it captured)". A reference can stand only for the
  closed part. A function with captured values would be a reference plus its captured values, and only the
  values would be copied agent by agent.
- A read (`R` meeting a reference) would have to expand the definition, or at least its top agent, first.
  HVM2 expands the whole definition on any contact; neither system offers a way to look at the top agent of
  a definition without copying it out.
- No system found does hash-consing (storing each distinct piece of code once and finding it by its hash)
  on nets built at run time. Search 10 in §1 returned nothing relevant.

### Gaps

- Whether Bend lifts every closed function body into its own definition automatically (which is what would
  make references cover most code) was not verified; the old Bend docs are gone from their address.
- No measurement was found of what share of HVM2's or Ivy's rewrites are copy/erase with and without
  references.
- Other evaluators (BOHM, Inpla, inets, PIN) were not found to have a by-reference agent; this was checked
  only for Inpla's introduction, which shows none.

## 5. Reading a net from outside or by traversal: read-back, token machines, and what an observer can tell apart

### Takeaway
Outside the net there are two ways to read: walk the finished graph and rebuild the term (read-back), or
never rewrite at all and send a token through a fixed graph (geometry of interaction). Both are done by the
implementer, not by the program. Inside the theory, a net can only be observed by plugging it into another
net, and what can then be told apart is behaviour, never syntax.

### Cited Findings

**Read-back in optimal reduction**

- In Lamping's graphs a piece of graph means different terms depending on how it was reached; reading needs
  a context carried along the path: "The context combines with the structure of a piece of graph to
  determine which lambda expression is represented" —
  [Lamping 1990, p. 20](https://web.archive.org/web/20240822152938id_/https://dl.acm.org/doi/pdf/10.1145/96709.96711#page=5)
- That context is the reader's bookkeeping, not part of the running system: "notion of context is not a
  part of the algorithm" —
  [Lamping 1990, p. 20](https://web.archive.org/web/20240822152938id_/https://dl.acm.org/doi/pdf/10.1145/96709.96711#page=5)
- Asperti (2017) on inspecting a result: it is done "either by traveling in the resulting graph, computing
  paths in it, or via a readback procedure that reconstructs" the λ-term —
  [Asperti 2017, p. 6](https://arxiv.org/pdf/1701.04240#page=6)
- Cost is not known, only conjectured: "the readback procedure is just linear in the size of the resulting
  term", "but there is no proof of this fact" — [Asperti 2017, p. 6](https://arxiv.org/pdf/1701.04240#page=6)
- He also notes that ordinary languages give no such read at all: "you cannot read back a closure" —
  [Asperti 2017, p. 6](https://arxiv.org/pdf/1701.04240#page=6)
- Other interaction-net evaluators rely on a strategy from outside, namely ones "that require avoiding
  interactions in disconnected parts of a net" — [Salikhmetov 2016, p. 45](https://arxiv.org/pdf/1609.03644#page=1)

**Token machines (geometry of interaction): reading without rewriting**

- The interaction abstract machine (IAM) treats the term as a fixed automaton: "A run then consists in
  entering the automaton with a word at some given entry node" and travelling through it —
  [Danos–Regnier 1999, PDF p. 4](https://www.pure.ed.ac.uk/ws/files/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4)
- Nothing is copied, so nothing has to be erased: "It needs no external garbage-collection" —
  [Danos–Regnier 1999, PDF p. 4](https://www.pure.ed.ac.uk/ws/files/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4)
- Muroya and Ghica put it most plainly: "The token passed around a graph simulates graph rewriting without
  actually rewriting" — [Muroya–Ghica 2019, p. 2](https://arxiv.org/pdf/1803.00427#page=2)
- The graph is read-only: token passing "keeps the underlying graph fixed and uses the data stored in the
  token to route it" — [Muroya–Ghica 2019, p. 2](https://arxiv.org/pdf/1803.00427#page=2)
- The price is time: "It therefore favours space efficiency at the cost of time efficiency" —
  [Muroya–Ghica 2019, p. 2](https://arxiv.org/pdf/1803.00427#page=2)
- How bad: the IAM "can be exponentially slower than environment machines" ("it can be exponentially slower
  than environment machines") — [Accattoli–Dal Lago–Vanoni 2020, p. 2](https://arxiv.org/pdf/2010.12988#page=2)
- The picture is "a token that travels around the syntax tree of the program under evaluation" —
  [Accattoli–Dal Lago–Vanoni 2020, p. 6](https://arxiv.org/pdf/2010.12988#page=6)
- A middle design (the Dynamic GoI Machine) lets the token trigger rewrites where it stands: "the token
  holds control over graph rewriting, by visiting redexes and triggering the rewrite transitions" —
  [Muroya–Ghica 2019, p. 3](https://arxiv.org/pdf/1803.00427#page=3)
- and rewriting pays back walking: "the DGoIM can reduce sub-graphs visited by the token, avoiding repeated
  token actions" — [Muroya–Ghica 2019, p. 3](https://arxiv.org/pdf/1803.00427#page=3)

**What an observer can tell apart in interaction combinators**

- The symmetric combinators are a fair stand-in for all interaction nets: "any interaction net system can
  be translated in the symmetric interaction combinators" — [Mazza 2009, p. 2](https://arxiv.org/pdf/0906.0380#page=2)
- An observer is a context, and a context is just a net: "it is simply another net C whose interface is
  big enough" for the observed net to be plugged in — [Mazza 2009, p. 2](https://arxiv.org/pdf/0906.0380#page=2)
- Whether a net stops is not a usable test: "termination is not an interesting observable in interaction
  nets, because of vicious circles" — [Mazza 2009, p. 17](https://arxiv.org/pdf/0906.0380#page=17)
- What is observed instead is a kind of wire that shows up during reduction ("observable axiom"): "we are
  led to take the presence of observable axioms as the key phenomenon to observe" —
  [Mazza 2009, p. 3](https://arxiv.org/pdf/0906.0380#page=3)
- Telling two nets apart from inside is weaker than in the λ-calculus: separation "cannot be realized using
  two arbitrary nets" — [Mazza 2009, p. 3](https://arxiv.org/pdf/0906.0380#page=3)

### Inferences

- In a pure interaction system, the only thing one agent learns about another is which rule fired when
  they met. There is no "what are you?" besides interaction. A reflection primitive therefore has to be a
  family of rules, one per kind of agent read, which is how `R` is built. Mazza's observers have the same
  limit, and it is why his equivalences are about behaviour.
- The user's code agents are inert until thawed, so syntax stays visible to `R` in a way that Mazza's nets
  do not allow: two functions with the same behaviour and different code are told apart by `ρ`. That is the
  intended break of extensionality and has no counterpart in his theory.
- A token machine is the extreme answer to the measured cost (about 97% of rewrites are copies and erases
  of code that never changes): never copy code; keep one fixed graph and move a token that remembers where it is. The
  literature's verdict is that this trades the copy cost for repeated walking, which can be exponentially
  slower. The Dynamic GoI Machine is the published middle ground (walk, and rewrite only where it pays).
- Read-back shows the size of the gap: what the implementer does from outside with a context stack has, in
  the literature, been moved inside a net only once (Salikhmetov), and there the result is text.

### Gaps

- Mackie's "The geometry of interaction machine" (POPL 1995), Gonthier–Abadi–Lévy (POPL 1992) and the
  Asperti–Guerrini book could not be quoted (see "Not verified"). The read-back and token-machine points
  rest on Lamping, Asperti 2017, Danos–Regnier, Muroya–Ghica and Accattoli–Dal Lago–Vanoni.
- No source gives the cost of read-back as a proved bound.
- No source was found that runs a token machine as agents inside an interaction net (the token is always
  machine state outside the graph).

## 6. Graphs that contain or describe graphs (one short entry each)

### Takeaway
Outside interaction nets, three ideas recur: rules are themselves graphs and can be matched or rewritten;
a node can contain a graph and be copied or deleted as a unit without looking inside; and a system keeps a
description of itself as data that a second level reads and edits. All of them pay with non-local or
non-constant-time steps, which interaction nets rule out.

### Cited Findings

- **Port-graph rewriting (PORGY).** A rule is a graph with a special arrow node joining its two sides:
  "a port graph rewrite rule is a port graph consisting of two port graphs L and R" —
  [Fernández–Kirchner–Pinaud 2014, p. 17](https://arxiv.org/pdf/1407.7929#page=3). A read is a sub-graph
  match of the rule's left side.
- **Rules as first-class port graphs.** In Andrei and Kirchner's calculus, "Rules and strategies are
  themselves port graphs, i.e. first-class objects of the calculus"; "As a consequence, they can be rewritten
  as well" — [Andrei–Kirchner 2009, HAL abstract](https://inria.hal.science/inria-00418560#authors-detailed).
  A rule can therefore read and change another rule.
- **Higher-order port graphs.** A pattern may contain a variable node that stands for a whole sub-graph; a
  match maps "higher-order nodes to port-graphs, and edges to edges" —
  [Fernández–Maulat 2012, p. 30](https://arxiv.org/pdf/1211.4098#page=6). Cost: the authors warn of a
  blow-up in matching because "higher-order variables are matched to sub-graphs" —
  [Fernández–Maulat 2012, p. 33](https://arxiv.org/pdf/1211.4098#page=9)
- **Hierarchical graph transformation.** Special edges ("frames") hold graphs: "distinguished hyperedges
  contain graphs that can be hierarchical again" —
  [Drewes–Hoffmann–Plump 2002, p. 249](https://user.informatik.uni-bremen.de/hof/papers/JCSS02.pdf#page=1).
  Rule variables stand for whole frames: "These variables can be instantiated with frames containing
  hierarchical graphs" and "can be used to copy or remove frames without looking at" their contents —
  [Drewes–Hoffmann–Plump 2002, p. 250](https://user.informatik.uni-bremen.de/hof/papers/JCSS02.pdf#page=2).
  So copy and delete of a nested graph are one rule application. Contents are private by default: "The
  contents of frames may be considered as hidden data" —
  [p. 270](https://user.informatik.uni-bremen.de/hof/papers/JCSS02.pdf#page=22). The nesting can be
  removed: "Flattening recursively replaces each frame in a hierarchical graph by its contents" —
  [p. 251](https://user.informatik.uni-bremen.de/hof/papers/JCSS02.pdf#page=3)
- **Bigraphical nets ("binets").** The same nesting brought to interaction nets: agents may contain nets.
  "Interaction nets can be seen as a particular kind of bigraphs without nesting" —
  [Fernández–Mackie–Walker 2013, p. 76](https://arxiv.org/pdf/1302.6339#page=3); "edges can connect ports
  of agents situated at different nesting levels" — [same](https://arxiv.org/pdf/1302.6339#page=3). The
  price is that a rule can touch everything nested inside an agent: it "involves a rewriting that affects
  all of the nested nets within M, hence a reduction strategy is required" —
  [p. 79](https://arxiv.org/pdf/1302.6339#page=6)
- **Nets within nets (Valk).** A Petri net's tokens are themselves Petri nets: a system net and "Object
  Nets which can be seen as token objects of the system net" —
  [Valk 1998, PDF p. 2](https://www2.informatik.uni-hamburg.de/tgi/publikationen/public/data/1998/Valk98/Valk98.pdf#page=2).
  The outer net does not inspect the inner one; they fire a shared step: "both the system and the object
  participate in the same event" —
  [Valk 1998, PDF p. 12](https://www2.informatik.uni-hamburg.de/tgi/publikationen/public/data/1998/Valk98/Valk98.pdf#page=12)
- **Reflective Petri nets (Capra–Cazzola).** A fixed meta-level net holds a copy of the base net as
  coloured tokens: it reifies the base-level net "into the meta-level as colored marking of a subset of
  places" — [Capra–Cazzola 2007, p. 2008](https://cazzola.di.unimi.it/pubs/jucs07-www.pdf#page=7). The
  meta-program uses it "to observe (introspection) and manipulate (intercession) the base-level PN" —
  [same](https://cazzola.di.unimi.it/pubs/jucs07-www.pdf#page=7). Cost of keeping it readable: "The
  base-level reification is updated every time the base-level PN enters in a new state" —
  [same](https://cazzola.di.unimi.it/pubs/jucs07-www.pdf#page=7); changes flow back because "the base-level
  PN and its reification are causally connected" — [same](https://cazzola.di.unimi.it/pubs/jucs07-www.pdf#page=7)
- **GP 2 (graph programs).** No reflection found (search 11 in §1). A program reads the graph it runs on
  only through rule matching plus three tests: "The new functions indeg and outdeg access the indegree resp.
  outdegree of a left-hand node in the host graph" — [Plump 2012, p. 3](https://arxiv.org/pdf/1204.5541#page=3);
  and a test for "whether there exists an edge with a particular label between two matched nodes" —
  [Plump 2012, p. 1](https://arxiv.org/pdf/1204.5541#page=1). Rules are not graphs the program can hold.

### Inferences

- Frame variables are the graph-rewriting answer to the user's main cost: copy or delete a whole nested
  graph in one step "without looking at" it. In interaction-net terms that is one agent standing for a net,
  which is what HVM2 references and Ivy globals are (§4), restricted to static closed code so that the step
  stays constant-time.
- Reflective Petri nets keep a second copy of the system as data and pay to keep it in step on every state
  change. The user's net avoids that: the code is the only copy, and a read looks at it directly, one node
  per read (measured notes: "No whole-term quote happens").
- Nets-within-nets and GIN(IN) (§1) are the same shape: an outer agent or token that holds a net and can
  only fire shared events with it or call functions on it. Neither gives the outer level a case on the
  inner net's structure.
- Rules-as-graphs systems can read rules because their one primitive, sub-graph matching, already reads
  arbitrary structure. Interaction nets have no such primitive; a read has to be an agent meeting an agent.

### Gaps

- Only the HAL abstract of Andrei–Kirchner was checked, not the paper body.
- Read costs for these systems are not given as numbers in the sources; "sub-graph matching" is the stated
  mechanism and its cost depends on the matcher.
- Maude-style reflection in term rewriting (terms that describe terms and rules) was out of scope and not
  searched.

## Not verified

These could not be checked with `scripts/verify-source.sh`, so nothing above depends on them.

- **Mackie, "The geometry of interaction machine", POPL 1995** (doi 10.1145/199448.199483). ACM returns 403,
  no open copy and no Wayback capture. The script confirmed only the metadata.
- **Gonthier, Abadi, Lévy, "The geometry of optimal lambda reduction", POPL 1992** (doi 10.1145/143165.143172).
  A Wayback scan was fetched, but it is a two-column scan and the OCR interleaves the columns; no clean
  quote could be matched. From memory only: it gives the context semantics used to prove read-back correct.
- **Asperti and Guerrini, "The Optimal Implementation of Functional Programming Languages", CUP 1998.** No
  legitimate open copy. The publisher's contents list has a chapter 7 "Read-back" (seen in a bookseller
  listing, not a primary source).
- **Hassan and Sato, "Interaction nets with nested pattern matching", ENTCS 203, 2008**
  (doi 10.1016/j.entcs.2008.03.035). ScienceDirect returns 403 and no PDF capture exists. Its content is
  cited through the authors' own 2010 follow-up.
- **Bodies of four papers whose abstracts were verified:** Sinot, TLCA 2005 (doi 10.1007/11417170_28);
  Sinot, ENTCS 135(3), 2006 (doi 10.1016/j.entcs.2005.09.027); Fernández–Mackie–Sinot, MSCS 15(2), 2005
  (doi 10.1017/S0960129504004633); Mackie, RTA 2004 (doi 10.1007/978-3-540-25979-4_11) and Mackie, IFL 2008
  (doi 10.1007/978-3-642-24452-0_3). Publisher pages are paywalled or blocked, so how Mackie's 2004 and
  2008 encodings differ from YALE at the level of rules is not covered here.
- **Lafont's own copy of "Interaction Combinators"**
  (https://www.i2m.univ-amu.fr/perso/yves.lafont/pub/combinators.ps). It is PostScript, which the script
  does not extract. The quotes above were verified against a PDF of the published article hosted on a
  third party's site (dcreager.net), since ScienceDirect blocks fetches; the text is the journal version
  (Information and Computation 137, pp. 69–101).
- **Lafont's Figures 6–10** (how menus, selectors, codes and the decoder are wired). Figures are not in the
  extracted text.
- **Bend documentation of the HVM2 era** (lazy definitions, how `match` and data types compile). The
  addresses under `HigherOrderCO/Bend/docs/` return 404; the repository now redirects to a different
  language.
- **Vine language docs on `match` and enums, HVM3/HVM4, BOHM, inets, PIN/amineLight.** Not fetched.
- **Fernández, Mackie, Pinto, "A higher-order calculus for graph transformation", ENTCS 72(1).** Cited by
  Jiresch as a different approach to higher-order interaction nets; not located.
- **Andrei and Kirchner, "A Higher-Order Graph Calculus for Autonomic Computing"** (HAL inria-00328554).
  The record was fetched but its abstract did not contain the checked wording; only the companion record
  inria-00418560 is cited.
- **Lippi, "Encoding left reduction in the λ-calculus with interaction nets"** (the work Salikhmetov cites
  for strategies encoded in nets). Not fetched.
