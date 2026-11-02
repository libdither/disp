<script lang="ts">
  import Aside from '../Aside.svelte'
  import Code from '../Code.svelte'
  import { samples } from '../code-samples'
</script>

<section>
  <h2 id="saying"><span class="secnum">Section 6</span>What claims can say</h2>
  <p>
    A type theory usually earns its keep with function types, equality and induction. In disp
    each is a claim, and each is checked by the machinery of Sections 4 and 5.
  </p>

  <h3 id="function-types">6.1 · Function types</h3>
  <p>
    A claim about a function has to test it somewhere. A higher-order claim, about functions
    that take functions, is harder: the argument is itself a function, and you can't try it
    everywhere. Disp's answer is that a member of a function type is a lens: the program, plus
    a back map naming the point of its argument it relied on. <code>implies(A, B)</code>
    challenges such a pair with an argument and a challenge for the result, so search can test
    it at one point and refute it with a concrete counterexample:
  </p>
  <Code code={samples['function-type'].code} runnable context={samples['function-type'].context} />

  <h3 id="equality">6.2 · Equality</h3>
  <p>
    Comparing two trees is instant, since every distinct tree is stored once (§2.5). It is also
    too strict: two different programs can do the same thing, and an optimizer needs to know
    when. Equality at a type is a claim: the two programs agree at every challenge of that type.
    <code>id</code> and <code>shift</code> (which rebuilds a number from its predecessor) are
    different trees and are certified equal at <code>Nat -&gt; Nat</code> by a case split: zero,
    and one more than some number.
  </p>

  <h3 id="induction">6.3 · Induction</h3>
  <p>
    A case split is enough when each case can be run symbolically. Recursive programs need
    induction: in the second case, each program's recursive call on the smaller number is named
    by one shared token, so the check compares the two step functions instead of unrolling them.
  </p>
  <Code code={samples['same'].code} runnable context={samples['same'].context} />
  <Aside title="What stays open.">
    <p>
      A true claim with no computable witness stays Open forever, and swapping a program for an
      equal one is only licensed in claims that never look at the program's tree. Both limits are
      part of the design, not bugs.
    </p>
  </Aside>

  <h3 id="search">6.4 · Search</h3>
  <p>
    The point of all this is to search for programs. <code>cegis</code> (counterexample-guided
    synthesis) tries candidates against a claim in order. Each challenge that refutes a candidate
    is kept and tried first on the next one, so wrong candidates fail fast:
  </p>
  <Code code={samples['search'].code} runnable context={samples['search'].context} />
  <p>
    This is a small search over a fixed list. Searching a space worth searching, and scoring
    candidates by cost as well as by correctness, is the work Section 7 describes.
  </p>
</section>
