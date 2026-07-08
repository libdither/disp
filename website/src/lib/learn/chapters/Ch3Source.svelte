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
  <pre>{`{x} -> body                // λx. body
{x, y} -> body             // λx. λy. body  (curried)
{x : A, y : B} -> body     // annotated binders`}</pre>
  <p>
    The elaborator lowers binders via bracket abstraction (§2.4). Application is juxtaposition
    and associates left. The literal <code>t</code> denotes the leaf — the only "literal" the
    language truly has; numbers and strings are sugar for particular trees built by application.
  </p>

  <h3 id="declarations">3.2 · Declarations and tests</h3>
  <Code code={samples['decl-basics'].code} runnable context={samples['decl-basics'].context} />
  <p>
    A <strong>test</strong> is a compile-time obligation: parse both sides, reduce, demand the
    identical tree. If the equation does not hold, the file fails to load. Tests are how the
    library asserts properties of definitions — and how the type checker itself is exercised on
    every load.
  </p>
  <p>
    A typed definition carries its type before <code>:=</code>. The annotation is an ordinary
    expression (typically a <code>Pi</code>), and it is <em>enforced</em>: when a module loads,
    every typed export is verified through the kernel. An annotation the value doesn't satisfy
    is a compile error, not a comment.
  </p>
  <Code code={samples['decl-typed'].code} runnable context={samples['decl-typed'].context} />
  <Aside title="let and test are not keywords.">
    <p>
      <code>let x := e</code> is a decorated declaration whose head is the library value
      <code>let</code> (it marks the binding file-private); <code>test</code> is the prelude
      identity function, peeled off equation items. Even the declaration form is ordinary
      library machinery — a file can shadow <code>let</code> and change what declaring means.
      The only true keywords are <code>use open match if then else</code>.
    </p>
  </Aside>
  <p>
    Sum types have a literal form that auto-binds constructors, and <code>match</code>
    eliminates them by tag. Booleans are <em>not</em> a tagged sum (§4.2), so they eliminate
    with <code>if</code>:
  </p>
  <Code code={samples['match-sample'].code} runnable context={samples['match-sample'].context} />

  <h3 id="modules">3.3 · Modules</h3>
  <p>
    <code>open use "path/to/file.disp"</code> loads a module and splices its exports into
    scope. Modules are <em>hermetic</em>: a used file sees only its own definitions, its own
    opens, and its declared <code>given</code>s — never the user's scope. A module that needs
    something from outside declares it explicitly (<code>given X : T</code>) and callers fill
    it (<code>use "f.disp" &#123; X := v &#125;</code>) — which makes every module a checkable
    <em>functor</em> from its givens to its exports. The kernel bootstraps itself through
    exactly this mechanism.
  </p>
  <Deep title="The compiler pipeline (parse → elaborate → bracket-abstract → emit)">
    <p>
      Four passes. <strong>Parse</strong> builds the surface AST. <strong>Elaborate</strong>
      resolves names and dispatches binder modes — <code>&#123;x&#125; -> e</code> in term
      position becomes a lambda; in type position it participates in a <code>Pi</code>.
      <strong>Bracket-abstract</strong> lowers binders with the §2.4 optimizations.
      <strong>Emit</strong> produces a closed tree. The punchline: the elaborator never
      <em>checks</em> types; it produces tree programs, and the kernel checks those programs by
      reduction. Elaboration is translation; type-checking is application.
    </p>
  </Deep>
</section>
