<script lang="ts">
  import Aside from '../Aside.svelte'
  import Deep from '../Deep.svelte'
  import Gloss from '../Gloss.svelte'
  import Code from '../Code.svelte'
  import { samples } from '../code-samples'
</script>

<section>
  <h2 id="kernel"><span class="secnum">Section 5</span>The two-operation kernel</h2>
  <p>
    Everything else about the type system — <code>Pi</code>, records, <code>Bool</code>,
    <code>Nat</code>, <code>Eq</code>, the universe <code>Type</code> — is library code you can
    read in <code>lib/kernel/</code>. The kernel proper is <em>two</em> operations and a
    dispatcher. This section is about why those two, and why they must be trusted.
  </p>

  <h3 id="hypothesis-problem">5.1 · The hypothesis problem</h3>
  <p>
    Does <code>quadruple := &#123;n&#125; -> double (double n)</code> have type
    <code>Nat -> Nat</code>? The annotation claims something about <em>infinitely many</em>
    inputs. No amount of running visits them all. Martin-Löf type theory answers with a context
    variable — "assume <span class="math">n : Nat</span>" — managed by judgment rules outside
    the language. Disp has no context object, but it must solve the same problem operationally.
  </p>
  <p>
    The answer is a placeholder with three properties: <em>(i)</em> it carries its declared
    type, so later checks can look that up; <em>(ii)</em> it has a fresh identity, so distinct
    placeholders are distinguishable; <em>(iii)</em> user code, running against it,
    <strong>cannot peek</strong> at any of this. Such a placeholder is called a
    <strong>hypothesis</strong> (or <em>neutral</em>), and this walkthrough will also call it a
    <em>promise</em>: a promise that a Nat will be here.
  </p>
  <p>The kernel mints one as a specific tree shape:</p>
  <p class="math-block">hyp T id &nbsp;=&nbsp; wait (hyp_reduce) (meta)</p>
  <p>
    where
    <Gloss tip="wait f x y reduces to f x y, but wait f x by itself does NOT evaluate f x — it lets a tree carry a handler at its head without that handler firing until another argument arrives">wait</Gloss>
    defers the handler and <code>meta</code> packages the stored type, the fresh identifier, and
    the <Gloss tip="the running log of how the hypothesis has been applied so far — how neutrals stay neutral under further application">spine</Gloss>.
    The tree's <em>signature</em> — kernel-speak for "what function sits at the head", read by
    the sanctioned <code>pair_fst</code> projection — is <code>hyp_reduce</code>. Signatures are
    the kernel's routing keys.
  </p>
  <Code code={samples['hyp-mint'].code} runnable context={samples['hyp-mint'].context} />
  <Aside kind="warn" title="Soundness threat.">
    <p>Two attacks, both live if hypotheses are ordinary trees:</p>
    <p>
      <strong>1 — Forge one.</strong> Fabricate a fork with head signature
      <code>hyp_reduce</code> and attacker-chosen metadata. A forged <code>h</code> with stored
      type <code>False</code> would, by the H-rule (§5.3), have <code>False h</code> answer
      <code>Ok true</code> — a fake proof of contradiction.
    </p>
    <p>
      <strong>2 — Inspect one.</strong> Reflective predicates (<code>is_fork</code>,
      <code>is_neutral</code>) answer differently on a hypothesis than on a concrete value, so a
      type <em>family</em> that inspects its argument could read as <code>Nat</code> during
      checking and as <code>Eq Nat 0 1</code> at a concrete use site — smuggling a false proof
      through an honest check. (These are pinned as attacks 3–5 in
      <code>lib/tests/soundness.test.disp</code>.)
    </p>
  </Aside>

  <h3 id="dispatcher">5.2 · The dispatcher and the walker</h3>
  <p>
    Both attacks are blocked by one mechanism: <code>param_apply</code>, the kernel's
    two-argument dispatcher. Every checked application routes through it, and it runs in one of
    two regimes:
  </p>
  <div class="privilege">
    <div class="priv raw">
      <span class="priv-title">raw mode</span>
      <p>
        The five rules as written, full visibility into hypotheses. Reserved for the handlers
        registered in the fixed set
        <Gloss tip="the kernel's routing table: pinned signatures mapped to registered handlers. param_apply routes a pinned signature to the REGISTERED handler, ignoring any handler embedded in the value — so carrying a kernel signature grants nothing">Σ</Gloss>
        — today, the two kernel operations.
      </p>
    </div>
    <div class="priv walker">
      <span class="priv-title">the walker (policed mode)</span>
      <p>
        Everything else. The five rules with the reflective moves cut: triage
        <em>on a hypothesis</em> is refused, and the stem rule will not assemble a fork whose
        head carries a kernel signature (no forging). A short list of reader carve-outs stays
        legal — <code>pair_fst</code> (signatures), <code>neutral_type</code>,
        <code>I</code>, and <code>tree_eq</code>.
      </p>
    </div>
  </div>
  <p>
    The walker enforces
    <Gloss tip="hypotheses are opaque tokens for universal quantification: user code cannot inspect their structure or synthesize new ones">operational parametricity</Gloss>:
    under it, a hypothesis behaves like an opaque universally-quantified value. That is exactly
    what makes "<code>f h</code> checks for a fresh <code>h</code>" mean the same thing as
    "<code>f</code> is correct on all inputs". A type that tries the illegal question doesn't
    crash the checker — the question itself is refused:
  </p>
  <Code code={samples['walker-carveouts'].code} runnable context={samples['walker-carveouts'].context} />

  <h3 id="sigma-ops">5.3 · The two operations</h3>
  <p>
    <strong>① <code>bind_hyp</code> — mint a promise.</strong> The mechanism by which
    <code>Pi A B</code> verifies "for all x : A, the body holds". <code>bind_hyp</code> mints a
    fresh hypothesis of type <code>A</code>, runs the body <em>under the walker</em> (so the
    body cannot reflect on the mint), and finally performs an escape check: if the minted
    hypothesis is reachable in the result via any extractable path, the binder rejects. A
    promise may appear inside other neutrals — that's how spines accumulate — but never as a
    directly extractable value; otherwise a proof of <code>False</code> could leak out of its
    own hypothetical.
  </p>
  <p>
    <strong>② <code>hyp_reduce</code> — observe through a promise.</strong> The only legal move
    on a hypothesis is an observation: apply it, project a field from it, eliminate it. Every
    observation routes here. <code>hyp_reduce</code> reads the promise's stored type and
    forwards the observation to that type's <code>respond</code> — a metadata field every type
    carries. A respond has exactly two moves, the
    <Gloss tip="Action ::= Extend type | Reduce value — a neutral elimination either stays stuck at a type or computes to a value; the canonical normalization-by-evaluation dichotomy">Action</Gloss>
    dichotomy: <code>Extend T</code> ("stuck — but I know the result's type is T") or
    <code>Reduce v</code> ("this actually computes — here's the value").
  </p>
  <Code code={samples['neutral-eval'].code} runnable context={samples['neutral-eval'].context} />
  <p>Both Action arms, on one record type:</p>
  <Code code={samples['derived-fields'].code} runnable context={samples['derived-fields'].context} />
  <p>
    This is <em>neutral evaluation</em>, and it is how the opening question resolves:
    <code>quadruple</code>'s body runs on a promise, <code>double</code>'s recursor reaches it
    and parks as a stuck elimination typed by its motive, the body finishes as a stuck tree
    <em>of type Nat</em>, the codomain matches, and the definition is accepted.
  </p>
  <Deep title="Why exactly two operations?">
    <p>
      An earlier kernel had seven primitives: a dispatcher, <code>hyp_reduce</code>, framing
      handlers for recognizers and eliminators, <code>bind_hyp</code>, and a
      <code>guard</code>/<code>unguard</code> boundary pair. The 2026 redesign showed the other
      five were library code in disguise: the frames became <code>make_recognizer</code> and the
      library <code>elim</code>; the boundary pair dissolved into verification-through-the-
      dispatcher plus <code>bind_hyp</code>'s body-walking. What cannot be library code is
      exactly what the walker refuses to let library code do: mint an unforgeable token
      (<code>bind_hyp</code>) and read one (<code>hyp_reduce</code>). The kernel is the fixed
      point of "everything the walker permits is a library".
    </p>
  </Deep>

  <h3 id="spines">5.4 · Spines</h3>
  <p>
    A hypothesis's spine is the running log of how it has been observed so far. When
    <code>hyp_reduce</code> answers <code>Extend T</code>, the result is a new neutral whose
    stored type is <code>T</code> and whose payload records "the parent neutral, extended by
    this observation" — the typed shape <code>Spine ::= Mint id | Ext parent frame</code>.
    Spines let neutrals grow indefinitely without ever evaluating: apply a promise to a thousand
    arguments and the result is still a placeholder, just with a longer history. And since every
    spine-extended tree still carries the <code>hyp_reduce</code> signature, the walker's
    protections follow it everywhere.
  </p>

  <h3 id="who-checks">5.5 · Three layers of soundness</h3>
  <p>The informal theorem: <em>if <code>param_apply T v = Ok true</code>, then v inhabits T per T's predicate.</em></p>
  <table>
    <thead><tr><th>Attack (§5.1)</th><th>Blocked by</th></tr></thead>
    <tbody>
      <tr>
        <td>Forge a hypothesis with stored type False</td>
        <td>The walker's stem rule refuses to assemble kernel-signed forks; Σ routes pinned signatures to the <em>registered</em> handler, so a forged head grants nothing</td>
      </tr>
      <tr>
        <td>Inspect a hypothesis to write a non-uniform type family</td>
        <td>Triage on a neutral is refused under the walker; only the reader carve-outs (<code>pair_fst</code>, <code>neutral_type</code>, <code>I</code>, <code>tree_eq</code>) answer</td>
      </tr>
      <tr>
        <td>Leak a hypothesis out of its binder into a closed proof</td>
        <td><code>bind_hyp</code>'s body-walk + occurs escape-check</td>
      </tr>
    </tbody>
  </table>
  <p>
    These carve-outs are <em>pinned</em>: <code>lib/tests/soundness.test.disp</code> asserts
    both what must pass and what must be refused, so a kernel change that widens the walker
    fails the suite.
  </p>
  <p>
    What the kernel deliberately does <em>not</em> guarantee: termination of user predicates,
    semantic correctness of user-supplied responds (§6.4 tackles that behaviorally), or
    ahead-of-time detection of paradoxes. Russell-style definitions are syntactically legal and
    diverge under evaluation; the reduction budget catches divergence as <em>failure</em>,
    never as a proof.
  </p>
</section>

<style>
  .privilege {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 0.9rem;
    margin: 1.2rem 0;
  }
  .priv {
    border-radius: 10px;
    padding: 0.8rem 1rem;
    border: 1px solid var(--border);
    font-size: 0.92rem;
  }
  .priv p { margin: 0.35rem 0 0; color: var(--fg-muted); }
  .priv-title {
    font-family: var(--font-mono);
    font-size: 0.8rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.07em;
  }
  .priv.raw { border-left: 3px solid var(--warn); }
  .priv.raw .priv-title { color: var(--warn); }
  .priv.walker { border-left: 3px solid var(--g2); }
  .priv.walker .priv-title { color: var(--g2); }
  @media (max-width: 700px) {
    .privilege { grid-template-columns: 1fr; }
  }
</style>
