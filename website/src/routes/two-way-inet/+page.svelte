<script lang="ts">
  import NetVis from "$lib/twoway-inet/NetVis.svelte";
</script>

<svelte:head>
  <title>Two-way programs on an interaction net · disp</title>
  <meta
    name="description"
    content="Two-way programs run as an interaction net: a walker moves through its program a node at a time, and flip turns it around."
  />
</svelte:head>

<div class="container page">
  <h1>Two-way programs, <span class="grad-text">on an interaction net</span></h1>
  <p class="lede">
    The same programs as on <a href="../two-way/">the two-way page</a>, run as an interaction net: a net of
    agents joined by wires, where the only thing that ever happens is two agents meeting at their principal
    ports and being replaced by a few new ones. Programs and values are trees of the same three agents.
    A run is a <strong>walker</strong> that moves through its program a node at a time.
  </p>

  <NetVis />

  <h2>Where the run is</h2>
  <p>
    On the two-way page a run keeps a count of how many parts of its sequence have run. Here nothing
    counts: the walker stands in its program, and the nodes it has come down through point back up to it,
    each holding the part it did not go into (drawn as rings, joined by a thick line: the walker's way
    back). Going down a fork, the walker reads its left first, since a fork's left says what the fork is
    (a †, a sequence or a tri), and comes back up through the same nodes, putting each back as it was.
    So the program is never copied or changed by running it, and at the end it is whole again beside the
    result.
  </p>
  <p>
    <strong>Flip</strong> (⇆) turns every walker around without computing anything: a walker going down
    at a node becomes one going up from it, the other way, and goes back up the way it came. For the walk,
    undoing a step is flip, fire, flip, as on the two-way page; the checks run every step of the examples
    that way.
  </p>

  <h2>What costs more on a net</h2>
  <p>
    An interaction sees two agents and nothing else, so some steps take several. A move that reads two
    levels of a tree (<code>cross</code>, <code>sink</code>, <code>unit†</code>) takes two meetings, and
    so do <code>map</code> and <code>tri</code> reading the value. A join, the two halves of a
    <code>map</code> coming back, takes two, one for each half. Flip lets these land before it turns the
    walkers, and the bar's history undoes them exactly.
  </p>
  <p>
    The big cost is recursion. The net has no sharing: a wire joins exactly two ports, so
    <code>self</code> cannot run the fix's body while that body is being walked. <code>fix(b)</code>
    copies b as it starts, walks one copy and keeps the other beside the walker, and every
    <code>self</code> copies it again, merging the copy back when it has run; both halves of a
    <code>map</code> need a copy too. On <code>notw</code> four in five meetings are copying and merging,
    and the 4-bit adder takes 1.7 million (it is left out here: the counter machine runs it in 799
    rounds, the net in 9,893).
  </p>
</div>

<style>
  .page {
    padding-bottom: 3rem;
  }
  h2 {
    margin-top: 2rem;
  }
</style>
