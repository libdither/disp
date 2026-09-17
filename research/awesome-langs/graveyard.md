# The graveyard — projects that aimed near disp and stopped

FOUNDATIONS is organized around "graveyard reasons." This file is the empirical
version: projects that attempted some part of disp's combination and are now dead
or dormant, with dates verified via the GitHub API on 2026-08-03.

## Magmide — the closest prior attempt, abandoned by its author

**Repo:** https://github.com/magmide/magmide (835★, Rocq) · **Author:** Blaine Hansen

The pitch was disp's pitch: *"a dependently-typed proof language intended to make
provably correct bare metal software possible"* — proofs for systems programmers,
Coq/Iris underneath, aimed at people who write real low-level code.

**Status: dead, explicitly.** The only 2026 commit is **2026-05-14: "clarify dormant
rather than under construction."** The last substantive work was April 2024
("going back to bare bones"). The README says it is "dormant for now as I rethink
what it could/should be," and the files are self-described as "mad scribblings." It
never reached a working compiler — design documents plus Iris experiments.

The author's current public work is `votebase` (a Rust/TS voting rules engine, June
2026) and Postgres tooling. He left the field.

**Why this matters more than any other entry here:** Magmide is the nearest
neighbor to disp in *intent* — same target audience, same "verification should be
for systems programmers" thesis, same solo-builder profile — and it died at the
design stage after years of visible enthusiasm (835 stars is a lot for a project
with no compiler). The failure mode was not a wrong technical bet; it was never
converting vision into a running artifact. disp has already cleared that bar (a
working pipeline, a kernel, ~1,200 tests, five evaluator backends), which is the
single strongest thing to be said for disp's execution to date.

## Vale — generational references, then silence

**Repo:** https://github.com/ValeLang/Vale (2,006★ on the org, `Verdagon/Vale` fork at 37★)

Evan Ovadia's memory-safety-without-borrow-checker language (generational
references, region borrowing). Org repo **last pushed 2024-05-14**. Ovadia then
spent **July 2024 – December 2025 on Modular's Mojo compiler team** (linear types,
associated aliases, CPU/GPU type-checking), left at the end of 2025, and revived
his fork in mid-2026 — June–July 2026 commits completed a full frontend migration
from Scala to Rust ("Delete the legacy Scala Frontend — 569 files, ~90.8K lines").

He also has a new project, **Harmonious / "erw — Embed Rust Well"** (243 commits,
June 2026): a framework letting custom languages compile *alongside* Rust via
rustc query overrides, reusing Rust's type system, generics, monomorphization, and
codegen. That is a genuinely useful piece of infrastructure for anyone building a
new systems language who doesn't want to write a backend.

**Relevance to disp:** Vale never had a verification leg — safety came from
type-system and runtime mechanisms, never proofs. Its trajectory is a reminder that
even a talented, well-followed language builder with a novel memory-safety idea
took ~6 years to not-quite-arrive, and that the pragmatic move (embed in rustc,
inherit the ecosystem) is available.

## Austral — linear types + capabilities, dormant

**Repo:** https://github.com/austral/austral (1,556★) · Last commit **2025-07-28**,
only ~3 commits in Jan–Jul 2025. Systems language with linear types and
capability-based security; explicitly *not* a proof language. Author deprioritized
it. A well-designed language that simply stopped.

## Kind / Kind2 — the proof language HigherOrderCO abandoned, then rebuilt elsewhere

**Repo:** https://github.com/HigherOrderCO/Kind · Last pushed **2025-01-22**.

Taelin's dependently-typed language on the HVM runtime — i.e. *the exact
combination disp wants*: proofs + optimal-reduction substrate. For two years the
team's energy went to Bend 1 and HVM2 instead, which had **no proof system**. Then
Bend 2 (September 2026, `bend2.md`) brought the proofs back — by dropping the
interaction-net runtime instead: an affine type theory compiled to plain C, with
the nets left behind in a dormant HVM4 (`hvm4.md`).

**Relevance to disp:** the clearest evidence that "dependent types on an
interaction-net substrate" is hard enough that the people best positioned to build
it never shipped the two together. First the proofs went, then the nets; each half
shipped alone.

## The pattern, and what disp should take from it

Three distinct failure modes, and disp is exposed to different amounts of each:

1. **Vision without an artifact** (Magmide). Killed the closest competitor. disp has
   defended against this — the foundation runs and is tested.
2. **The verification leg gets dropped for the performance leg** (Kind → Bend 1;
   arguably Vale, Austral; Bend 2 is the mirror image, keeping the proofs and dropping
   the exotic runtime, so the two halves still never shipped together). This is the
   most common outcome for anyone building on
   an exotic runtime, because performance work yields visible wins and proof work
   yields none until it's finished. disp's ordering — build the type system first,
   optimizer second — is the opposite of the sequence that killed these, which is
   a point in disp's favor.
3. **Technically fine, socially ignored** (Austral, ATS, Cedille). The risk disp
   inherits if the optimizer never lands: a beautiful substrate nobody has a reason
   to adopt. FOUNDATIONS Part V's framing — "settled modest foundation + unbuilt
   everything-or-nothing endgame" — is precisely a statement that disp's fate is
   avoiding this third mode.
