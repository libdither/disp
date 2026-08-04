# Agda + Cubical Agda

**Repo:** https://github.com/agda/agda (2,905★, pushed 2026-08-03)
**Relevance:** the reference implementation of the two things disp lists as
"designed but not built" — cubical path types — and of *reflection done in a
dependently-typed language*.

## Why disp should care: two mechanisms

### 1. Cubical type theory = the heavyweight answer to A4

FOUNDATIONS §7 lists three responses to the intensional-equality problem, and
cubical (CCHM 2016, Cubical Agda JFP 2021) is the one that *fully computes*:
computational univalence, higher inductive types, function extensionality as a
theorem rather than an axiom. If two implementations are equivalent, univalence
lets you transport everything proved about one to the other — which is exactly the
"replace a program with a differently-written but behaviorally-identical one"
capability disp's optimizer needs.

disp's own note is the correct one: cubical is **heavyweight** — interval
variables and Kan operations carry real performance cost. For a system whose
central selling point is O(1) conversion via hash-consed structural identity,
adopting cubical wholesale would eat the main advantage. The interesting question
disp has not answered: can path types be added *selectively*, as library-level
cells on the telescope walker (§5), so the cost is paid only where transport is
actually used? disp's one-negative-former design is unusually well suited to
trying this — a path is another cell kind.

### 2. Agda's reflection = what A1 looks like without an intensional substrate

Agda has a full reflection API: `quoteTerm`, `unquote`, `Term`/`Name` as inspectable
data, macros that run in `TC`. This is the mainstream way to get programs-as-data —
and it is precisely the **quotation-based** approach disp's substrate avoids.

The comparison is instructive. In Agda, to inspect a program you *quote* it,
obtaining a different object (a `Term`) that represents it; you then work in a
monad, and unquoting is a separate, partial operation. In disp, `shape_of` applies
directly to any program because programs *are* trees:

```
let shape_of := t (t "leaf" ({u} -> "stem")) ({u, v} -> "fork")
test shape_of (t t t) = "fork"
```

No quote, no monad, no representation layer. That is the concrete payoff of Jay's
tree calculus, and Agda is the cleanest system to measure it against — because
Agda's reflection is genuinely good, and it *still* requires the quotation dance.

The cost disp pays for this, which Agda does not: intensionality **breaks
parametricity and extensionality** (FOUNDATIONS §1: "simultaneously its selling
point and a soundness hazard"). Agda keeps both. disp's walker must actively
*police* reflection — refusing to answer questions about a hypothesis's raw shape
(`test param_apply (Pi Nat ({_} -> Bool)) ({x} -> is_fork x) = Err`) — to recover
what Agda gets for free.

## Scorecard

| Axis | Agda/Cubical | Note |
|---|---|---|
| A1 Reflection | ◐ | Full reflection API, quotation-based. The best non-intensional A1. |
| A2 Spec power | **✅** | Full dependent types + HITs + univalence. Strictly stronger than disp's current type theory. |
| A3 Kernel | ◐ | No LCF kernel; the typechecker is the TCB and it is large. Weaker than disp's design here. |
| A4 Equality | **✅** | Cubical: computational univalence. The answer disp cites and has not integrated. |
| A5 Perf | ✗ | GHC/JS backends, research-grade. No systems performance, no cost model. |
| A6 Search | ✗ | Auto/agda-mode search is trivial. No synthesis. |

## What disp could steal

- **Path types as telescope cells.** The single highest-value experiment: does
  disp's one-negative-former design (§5) let cubical paths enter as a cell op
  rather than a kernel change? If yes, disp gets a principled A4 story *without*
  the wholesale performance cost, because non-cubical code never touches the
  interval machinery. NEGATIVE_TYPES.md already argues new formers are library
  code — this is the test case that matters.
- **Cubical Agda's performance war stories.** The community has years of
  documented experience about *where* cubical costs bite (transport in large
  records, `hcomp` blowup). That is free reconnaissance for disp's Q6.
- **The `TC` monad's design** as a cautionary contrast — a reminder of what disp
  is buying by not needing it.

## Where disp differs

Agda is the "do the type theory properly, ignore performance" pole. disp is
attempting a type theory *good enough* on a substrate chosen for speed and
self-application. Agda will always have more expressive power; disp's bet is that
O(1) conversion plus native reflection plus a tiny kernel buys automation that
Agda's expressiveness cannot.

## Verdict

**The reference for disp's unbuilt A4 work and the fairest benchmark for what
native intensionality actually saves over quotation-based reflection.**

**Distance from disp's goals: ahead on A2/A4, absent on A5/A6, opposite approach
on A1.**
