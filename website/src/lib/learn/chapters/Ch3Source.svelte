<script lang="ts">
  import Aside from '../Aside.svelte'
  import Deep from '../Deep.svelte'
  import Code from '../Code.svelte'
  import { samples } from '../code-samples'
</script>

<section>
  <h2 id="source"><span class="secnum">Section 3</span>A first look at disp</h2>
  <p>
    The notation is small, and from Section 4 onward we will read it constantly, so it pays to
    nail down what the symbols mean. The grammar's authoritative source is
    <a href="https://github.com/libdither/disp/blob/main/SYNTAX.typ">SYNTAX.typ</a>.
  </p>

  <h3 id="binders">3.1 · Binders and application</h3>
  <pre>{`{x} => body                // λx. body — the value lambda
{x, y} => body             // λx. λy. body  (curried)
{x : A} -> B               // the Pi type: for all x : A, B
A -> B                     // sugar for {_ : A} -> B
f(a, b)                    // application: the same tree as f(a)(b)
xs.map(succ)               // a glued .name(...) call: map(xs, succ)`}</pre>
  <p>
    The two arrows divide the work: a fat <code>=&gt;</code> binder is a value lambda, lowered
    via bracket abstraction (§2.4); a thin <code>-&gt;</code> binder is a <em>type</em> — it
    desugars to an application of the library <code>Pi</code> former, and every parameter needs
    its <code>: T</code>. Application is a call, <code>f(a, b)</code>, and a function given too
    few arguments is partially applied. The literal <code>t</code> denotes the leaf — the only "literal" the
    language truly has; numbers and strings are sugar for particular trees built by application.
  </p>

  <h3 id="declarations">3.2 · Declarations and tests</h3>
  <Code code={samples['declarations'].code} runnable context={samples['declarations'].context} />
  <p>
    A <strong>test</strong> is a compile-time obligation: parse both sides, reduce, demand the
    identical tree. If the equation does not hold, the file fails to load. Every example on this
    page is a test in one file, <code>lib/tests/by_example.test.disp</code>, so everything it
    shows is checked whenever the test suite runs.
  </p>
  <p>
    The claim layer this page teaches uses <code>::</code> signatures, which are written for the
    reader and never checked; its checks are claims (§4). A single-colon <code>:</code> annotation
    is checked when a module is loaded, by <code>lib/kernel/</code>, the older checker the claim
    layer is replacing (§5.5).
  </p>
  <Aside title="let and test are not keywords.">
    <p>
      <code>let x := e</code> is a decorated declaration whose head is the library value
      <code>let</code> (it marks the binding file-private); <code>test</code> is the prelude
      identity function, peeled off equation items. Even the declaration form is ordinary
      library machinery — a file can shadow <code>let</code> and change what declaring means.
      The only true keywords are <code>use open match if then else</code>.
    </p>
  </Aside>

  <h3 id="modules">3.3 · Modules</h3>
  <p>
    <code>open use "path/to/file.disp"</code> loads a module and splices its exports into
    scope. Modules are <em>hermetic</em>: a used file sees only its own definitions, its own
    opens, and its declared <code>given</code>s — never the user's scope. A module that needs
    something from outside declares it explicitly (<code>given X : T</code>) and callers fill
    it (<code>use "f.disp" &#123; X := v &#125;</code>) — which makes every module a checkable
    <em>functor</em> from its givens to its exports. <code>open use raw</code> loads only a file's
    values and skips its annotations, which is how the claim layer's files open each other.
  </p>
  <Deep title="The compiler pipeline (parse → elaborate → bracket-abstract → emit)">
    <p>
      Four passes. <strong>Parse</strong> builds the surface AST. <strong>Elaborate</strong>
      resolves names and reads the arrows — <code>&#123;x&#125; => e</code> is a value lambda;
      <code>&#123;x : A&#125; -> B</code> desugars to an application of the library
      <code>Pi</code> former.
      <strong>Bracket-abstract</strong> lowers binders with the §2.4 optimizations.
      <strong>Emit</strong> produces a closed tree. The punchline: the elaborator never
      <em>checks</em> types; it produces tree programs, and a claim checks those programs by
      running them. Elaboration is translation; checking is application.
    </p>
  </Deep>
</section>
