<script lang="ts">
  import CompiledVis from "$lib/evaluators/twoway-compiled/CompiledVis.svelte";
</script>

<svelte:head>
  <title>Two-way programs, compiled into a net · disp</title>
  <meta
    name="description"
    content="Two-way programs compiled into an interaction net: one agent per part of the program, the value's top as the run, and flip turning each run around where it stands."
  />
</svelte:head>

<div class="container page">
  <h1>Two-way programs, <span class="grad-text">compiled into a net</span></h1>
  <p class="lede">
    The same programs as on <a href="../two-way/">the two-way page</a>, but compiled: each part of the
    program becomes one agent (a gate for each move, a node for a sequence, a tri, a map, a self), and the
    value goes through them. There is no walker reading the program as a tree: the value's top node is the
    run, and a move is done where the value meets its gate.
  </p>

  <CompiledVis />

  <h2>Every node faces the run</h2>
  <p>
    The program stays a tree, and so does the whole net. Each program node's principal port (the port it
    can meet on, marked with a dot) points the way the run is: up while the run is above it or has
    finished with it, down into the part it is running while the run is inside. The nodes on the way down
    to a run all point at it (drawn thick), so a node's facing is all it knows about where the run is.
    Nothing counts, and nothing is reversed but which port is principal.
  </p>
  <p>
    A run going down meets a node; going up, it meets the node it came out of. A sequence sends it into
    its next part, a tri chooses a part by the value's left and puts it back after, a map sends a fork's
    two halves into its two parts at once and joins them when both are back. A gate does its move and
    sends the run back up: <code>swap</code>, <code>stem</code> and <code>unit</code> in one meeting,
    since they only rearrange the top; <code>cross</code>, <code>sink</code> and <code>unit†</code> read
    a level down in a second. Going down into part of the value (<code>stem†</code>, map, tri) takes a
    head: one more meeting, to make that part the new top.
  </p>

  <h2>Flip, where the run stands</h2>
  <p>
    <strong>Flip</strong> (⇆) turns each run around in time: a root swaps its two wires and its direction,
    and goes back the way it came, undoing as it goes. A head or reader in the middle of a step finishes the
    step and comes out turned; a map holding one finished half sends it back down. That is the whole of it:
    no step has to land first, no other node changes, and nothing is kept on the side. Running back is the
    net's own rules, the same ones that ran it forward. The check (<code>npm run twoway-compiled-check</code>)
    flips after every round of every example and of a few thousand small programs, runs back, and gets the
    net exactly as it was loaded.
  </p>

  <h2>Recursion unrolls, and rolls back up</h2>
  <p>
    <code>self</code> is the one part that cannot be laid out ahead of time. When a run reaches a self,
    the self becomes a copy of its fix's body, made in that one meeting (drawn dashed, with the fix's
    name). When the run comes back out of the copy, either way, the copy is at rest again, the fix's body
    as written, so it is erased back into a self: erasers take it apart beside the run, off its way, and a
    run that comes back unrolls it again. Erasing loses nothing, since what goes is program text, not
    anything the run worked out. Nothing is copied node by node and nothing is merged back, which on the
    walker net was four meetings in five.
  </p>

  <h2>Against the other two</h2>
  <table>
    <thead>
      <tr><th>example</th><th>this net</th><th>its erasing, beside the run</th><th>the walker net</th><th>the counter machine</th></tr>
    </thead>
    <tbody>
      <tr><td><code>not</code></td><td>23 meetings, 23 rounds</td><td>0</td><td>134, 134</td><td>7 rounds</td></tr>
      <tr><td><code>add</code> 3 + 2</td><td>40, 40</td><td>36</td><td>537, 152</td><td>18</td></tr>
      <tr><td><code>mirror</code></td><td>88, 44</td><td>60</td><td>802, 145</td><td>24</td></tr>
      <tr><td><code>notw</code>, 8 bits</td><td>269, 46</td><td>255</td><td>5,611, 222</td><td>23</td></tr>
      <tr><td><code>copy</code></td><td>312, 289</td><td>576</td><td>37,824, 1,441</td><td>95</td></tr>
      <tr><td><code>addw</code>, 4 bits</td><td>5,617, 2,396</td><td>8,478</td><td>1,782,875, 10,517</td><td>894</td></tr>
    </tbody>
  </table>
  <p>
    The net takes about two rounds for each step of the counter machine: one meeting to do a part and one
    to move on to the next. Erasing the copies of fix bodies costs about as many meetings again, but they
    happen beside the run and never hold it up; keeping the copies instead would cost nothing to erase and
    leave the 4-bit adder holding 8,514 agents at the end rather than 36. Its parallelism is still map's,
    the same as the counter machine's, since a program that runs back cannot be used up as it runs.
  </p>

  <h2>Not done</h2>
  <p>
    <code>eval</code> is not compiled: running a program held in the value would need it compiled while
    the run waits, and put back after. A self's copy is made in one meeting however big the body is, which
    is cheap here but not what a net with small rules would do.
  </p>
</div>

<style>
  .page {
    padding-bottom: 3rem;
  }
  h2 {
    margin-top: 2rem;
  }
  table {
    border-collapse: collapse;
    font-size: 0.9rem;
    margin: 0.5rem 0 1rem;
  }
  th,
  td {
    text-align: left;
    padding: 0.25rem 0.9rem 0.25rem 0;
    border-bottom: 1px solid var(--border);
  }
  th {
    color: var(--fg-muted);
    font-weight: 500;
  }
</style>
