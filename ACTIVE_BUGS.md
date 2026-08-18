# Active bugs

This file tracks the kernel (`lib/kernel/`, the provenance-based checker that
was promoted from lib/standalone/ on 2026-08-17). The previous kernel's ledger
is frozen at `archive/live-kernel/BUGS.md`; its defense model is different, so
items do not transfer between the two.

Here hypotheses are ordinary trees that anything can construct, and
legitimacy comes from provenance: a session ledger of root hypotheses, plus replaying a
derived hypothesis's recorded history through the shared respond face. Forgery and replay
are refused because a fabricated hypothesis roots nowhere, and that is pinned on both
walking tiers.

Pins live in `lib/kernel/kernel.test.disp`. The 2026-08-01 review list is closed apart
from the two semantic items below, which are the same item seen from two sides and are what
`OEQ_PLAN.md` steps 3 and 4 exist to close.

| # | Gap | Severity |
|---|-----|----------|
| S9 | A dependent codomain that inspects its bound variable is not the type that was written | Live: the certificate is a lie at every concrete point |
| S2 | A function type over a quotient does not mean the function respects the quotient | Latent, and blocked on the ledger being untyped |

### S9. A dependent codomain does not mean what it says

Codomains and motives are instantiated by raw application, and the raw tier's structural
comparison is the native primitive: it answers `false` on a hypothesis instead of going
three-valued the way the walk's does, and the neutrality reader answers honestly for the
same reason. So a codomain that inspects its bound variable is formed at one branch for the
certificate and at the other branch everywhere else.

Measured: a codomain answering `Nat` at the hypothesis and `False` at zero certifies the
identity function, whose value at zero does not inhabit the type that codomain declares
there. The neutrality spelling certifies everything, including candidates of the wrong type.

This is the residue of the barrier step 1 hit. Lazy hypotheses made raw application and raw
elimination of a hypothesis go stuck, which is why an honest dependent codomain works today;
inspection is the piece that was never reached. It is not a new class of defect, it is the
live consequence of the first two entries in step 3's forbidden set, and it means those
entries are load-bearing for type formation now rather than insurance for a future consumer.

The asymmetry that makes it cheap to detect: the eliminator route is already honest, because
a gate instantiates its motive at concrete constructor points, while a Pi codomain is only
ever instantiated at the abstract point. `TwoFace` gives Pi the same treatment, conjoining a
concrete face onto the abstract one; it only ever adds obligations, so it can refuse but
never admit more, and it catches both lying codomains. It is opt-in and it is a finite
battery, so it is a lie detector, not the closure. The closure is the forbidden set.

### S2. A function type over a quotient does not enforce respect

Membership at `Fn Q B` checks that results inhabit `B`. It does not check that the function
maps quotient-equal inputs to equal outputs. Measured: a function that compares its argument
structurally certifies at a parity-quotient function type while returning different results
for two values the quotient identifies, and the quotient itself agrees they are equal.

Not exploitable into a false equation today. Proofs erase to a single canonical value and
the equality type still checks its endpoints, so transport produces that value but no type
accepts it wrongly. Nothing consumes function-type membership as respect evidence either.

This is the setoid-respect gap the archived investigation described, reproduced here by the
quotient feature. Stating respect as a type does not work, and the reason is S9: the
obligation `Eq B (f a) (f b)` is formed raw, so for an inspecting `f` both endpoints reduce
to the same concrete value and the obligation becomes `Eq Nat 1 1`, vacuous for exactly the
class of function it targets. Relatedness therefore has to be a judgment the tier computes.
That judgment is measured working, and it declines structural comparators on its own, but it
cannot ship yet because the ledger stores untyped equations: an assumption recorded at a
quotient is reused at the carrier, so the identity certifies as a parity-respecting map into
the naturals. Typing the ledger entries is the prerequisite. All of it is pinned, including
the unsoundness, so the next attempt starts from the measurement.

## Closed here

- **One stuck term had two representations.** Applying or eliminating a hypothesis built a
  different tree under the walk than raw, and this kernel compares by tree identity nearly
  everywhere, so it held only because every exercised path compared same-route terms. The
  guard tier's eager helper and its collecting shim are deleted, and the respond face's
  projection branch applies the mark instead of minting its own. The one thing the helper
  really added was kept: an observation the type does not declare is an error, not a stuck
  term, and a lazy record cannot represent "no type", so the walk still refuses where the
  observation is made.
- **Type-indexed projection hung on an unknown key.** `at` walked its key list with no base
  case and recurred forever on the exhausted pair. It errors now.
- **Tuple keys deduplicated silently**, so a tuple of two identical types collapsed into a
  scalar. Keys are meant to be distinct, and brands exist for when the underlying types
  collide, so a repeated key is refused at formation.
- **The eliminator layer read declaration slots unguarded.** A type declaring neither a gate
  nor a recursor got an arbitrary arity, and the respond face's defense that an eliminator
  carries the type's own recursor compared two absent slots and passed, so it was vacuous
  for exactly the types with nothing to defend. Guarded at construction, at the arity read,
  and at the respond face.
- **Member lists were only checked for soundness.** Two consumers read the list as the
  complete domain, so a partial one bought a false disequality and a false universal. Full
  exhaustiveness is what a finite battery cannot witness, but the battery catches a probe the
  recognizer accepts and the list omits, which is what truncation looks like. Added to the
  coherence suite as a lie detector, labelled as one.
- **The abstract tier read a stored type key** rather than the derived accessor, which would
  have rejected a re-lifted derived hypothesis. Over-rejection only and unreached, but the
  accessor is the only honest reader after the lazy change.
- **A vestigial hook, deleted.** The guard family's equality hook was left behind when the
  real decision moved elsewhere; both call sites already tested what it re-tested, so its
  rejection branch was unreachable and one site read "if p then X else X".
- **A record type did not check field presence.** Membership read each declared field with
  the plain accessor, which answers the leaf sentinel when the key is absent, and that
  sentinel inhabits most types, so the empty record inhabited every record type. Fixed by
  reading the cell and rejecting an absent field, which is what the live kernel's honest
  lookup does and why it exists.
- **Ex falso leaked at the raw tier.** An elimination with zero obligations must fire on
  arrival; the collector otherwise waited for an argument that never comes and returned a
  partially applied collector as if it were a value. The walk had this case, the marker did
  not.
