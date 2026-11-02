<script lang="ts">
  import Aside from '../Aside.svelte'
</script>

<section>
  <h2 id="future"><span class="secnum">Section 7</span>Where it is going</h2>
  <p>
    The goal is in <code>GOALS.md</code>:
  </p>
  <blockquote>
    I want to synthesize the best possible programs given a specification… turn the type
    checker into a function that takes a program and returns 1 or 0, combine that with
    functions that measure performance or program size, and somehow turn all that into a
    function represented purely in some low-level deterministic Turing-complete calculus.
    <span class="cite">— GOALS.md</span>
  </blockquote>
  <p>
    The <a href="../compare/">comparison page</a> splits that goal into five requirements and
    grades disp twice: on what runs today, and on what the design could reach by the arguments
    written down so far. This section is the short version of the second grade.
  </p>

  <h3 id="pinned">7.1 · What runs</h3>
  <p>
    Everything in Sections 2 to 6: programs are trees any program can take apart; claims, the
    honest search, lenses and their laws; certificates from the five rules; equality by case
    split and by induction; and a small <code>cegis</code> search. The tests in
    <code>lib/tests/</code> run all of it, and the examples on this page are one of those files.
  </p>

  <h3 id="designed">7.2 · What is designed</h3>
  <ul>
    <li>
      <strong>Cost in the score.</strong> A search scores each candidate by the checker's verdict
      times its cost, and a cost bound is one more claim
      (<code>research/OPTIMIZER.typ</code> §2).
    </li>
    <li>
      <strong>A model of the machine.</strong> A hardware model is an ordinary program that runs
      assembly and returns its output and cycle count, so "this assembly does what the spec says"
      is a claim over it (§9 of the same note).
    </li>
    <li>
      <strong>Handing work to a faster language.</strong> Effects are data, and one impure driver
      at the edge of the program performs them. That driver is where work goes to a faster
      language and comes back with its outputs, time and memory (<code>FOUNDATIONS.md</code> §8).
    </li>
    <li>
      <strong>Wider search.</strong> A claim can generate its own candidates instead of filtering
      every tree (<code>research/CLAIMS_OVER_TREES.md</code> §4). Whether search over a space worth
      searching is affordable is the project's main open question.
    </li>
  </ul>

  <h3 id="open">7.3 · What has no argument yet</h3>
  <p>
    Two things the goal asks for have no design behind them. Nothing writes <em>new</em> claims
    for the search to satisfy; it only keeps the counterexamples it finds. And no second,
    independent checker confirms certificates; the honest search is independent, but it can only
    refute. Self-application, an optimizer improving itself, is possible in principle, but by
    Löb's theorem (§5.4) each new version is trusted one level less than the one that admitted it.
  </p>
  <Aside title="Why the self-hosting discipline pays here.">
    <p>
      A synthesized program is only as trustworthy as the checker that admitted it. Because
      disp's checkers are ordinary programs, the optimizer can be pointed at them too, and every
      improvement it proposes for itself passes through the same checks it applies to everything
      else.
    </p>
  </Aside>
</section>
