# An Introduction to Recursion Schemes — in Disp

A disp-flavored retelling of Patrick Thomson's
[*An Introduction to Recursion Schemes*](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html)
(2014), itself a tour of Meijer/Fokkinga/Paterson's 1991 *Functional
Programming with Bananas, Lenses, Envelopes and Barbed Wire*.

The original is in Haskell. Each idea below is restated with its Haskell
original *and* its disp transcription, so you can read it through the lens of
the substrate you already know. The punchline for us: **disp already ships
three of the four moving parts** the article spends its length building up.
The Haskell article *constructs* a functor, a fixed-point type, and a fold;
disp's kernel *is* those things. The table is the whole article in miniature:

| Article concept | Haskell | Disp analogue (already exists) |
|---|---|---|
| Recursive sum type | `data Expr = …` | the §2.6 coproduct cut — `Coproduct_of { … }`, `inj`, `match` |
| Map over children | `Functor`/`fmap` | the **`functor` meta-field** on every type's MetaShape (`cut.disp`) |
| Fixed point of a functor | `newtype Term f = In { out :: f (Term f) }` | `fix` (value level) + `self`-threading recognizers (`make_rec_recognizer`) |
| The fold (sequel teaser) | `cata` / catamorphism | **`elim`** over a type's `respond`; `list_foldr`, `nat_rec`, `ord_rec` |

> The disp snippets are *illustrative translations* — readable surface syntax,
> not necessarily byte-for-byte compilable (the kernel's η/saturation and
> self-recursion workarounds in `CLAUDE.md` would apply to real code). Where a
> claim is load-bearing I point at the *real* disp construct that backs it
> (`lib/kernel/types.disp`'s `Ord`, `lib/std/list.disp`'s `list_foldr`,
> `lib/kernel/engine.disp`'s `elim`).

---

## 1. The problem: hand-written recursion is boilerplate

We start with an ordinary AST. In Haskell:

```haskell
data Lit
  = StrLit String
  | IntLit Int
  | Ident  String

data Expr
  = Index  Expr Expr
  | Call   Expr [Expr]
  | Unary  String Expr
  | Binary Expr String Expr
  | Paren  Expr
  | Literal Lit
```

In disp, a sum type is the §2.6 *coproduct cut*: `Coproduct_of` takes a record
of `tag := payloadType` and yields a type whose values are `inj "tag" payload`
and whose elimination is `match`. Note `Expr` mentions **itself** — that
self-reference is exactly the recursion this whole article is about, and we'll
see disp hands it to you for free in §5.

```disp
Lit := Coproduct_of {
  StrLit := String
  IntLit := Nat
  Ident  := String
}

// `Expr` is self-referential; in disp a recognizer receives its own fixed
// point as `self` (§5), so a recursive type is legal as written.
Expr := Coproduct_of {
  Index   := Pair Expr Expr
  Call    := Pair Expr (List Expr)
  Unary   := Pair String Expr
  Binary  := Pair Expr (Pair String Expr)
  Paren   := Expr
  Literal := Lit
}
```

Now write a transformation — say `flatten`, which strips redundant parens.
You have to recurse through **every** constructor by hand. Haskell:

```haskell
flatten :: Expr -> Expr
flatten (Literal i)    = Literal i
flatten (Paren e)      = flatten e
flatten (Index e i)    = Index (flatten e) (flatten i)
flatten (Call e args)  = Call (flatten e) (map flatten args)
flatten (Unary op arg) = Unary op (flatten arg)
flatten (Binary l op r)= Binary (flatten l) op (flatten r)
```

Disp, via the coproduct `match` (and `list_map` for the `Call` children):

```disp
flatten := fix ({self, e} -> match e {
  Literal i     => Literal i
  Paren x       => self x
  Index a b     => Index (self a) (self b)
  Call f args   => Call (self f) (list_map self args)
  Unary op a    => Unary op (self a)
  Binary l op r => Binary (self l) op (self r)
})
```

Only **two** of those six arms (`Paren`, `Literal`) carry real logic. The other
four are pure structural recursion — boilerplate we re-type for every traversal,
and re-edit every time `Expr` grows a constructor. That is the disease. The rest
of the article is the cure.

---

## 2. First aid: factor the recursion into one place

Push the structural recursion into a single helper, `apply_expr`, that applies a
function to each **immediate** child and reassembles. Haskell:

```haskell
applyExpr :: (Expr -> Expr) -> Expr -> Expr
applyExpr f (Literal i)    = Literal i
applyExpr f (Paren p)      = Paren (f p)
applyExpr f (Index e i)    = Index (f e) (f i)
applyExpr f (Call e args)  = Call (f e) (map f args)
applyExpr f (Unary op arg) = Unary op (f arg)
applyExpr f (Binary l op r)= Binary (f l) op (f r)
```

Disp:

```disp
apply_expr := {f, e} -> match e {
  Literal i     => Literal i
  Paren p       => Paren (f p)
  Index a b     => Index (f a) (f b)
  Call g args   => Call (f g) (list_map f args)
  Unary op a    => Unary op (f a)
  Binary l op r => Binary (f l) op (f r)
}
```

Now `flatten` keeps only the interesting case and delegates the rest:

```haskell
flatten' :: Expr -> Expr
flatten' (Paren e) = flatten' e
flatten' x         = applyExpr flatten' x
```

```disp
flatten := fix ({self, e} -> match e {
  Paren x => self x
  _       => apply_expr self e   // illustrative; see CLAUDE.md η/saturation note
})
```

Better — but `apply_expr` is still hand-written and still has six arms. It's
*tied to `Expr`*. Change the type, rewrite the helper. The boilerplate moved; it
didn't leave. To kill it we have to make "map over the children" something the
*machine* derives. That requires separating the **shape** of one layer from its
**recursion**.

---

## 3. Parameterize: the pattern functor `ExprF`

The trick: replace every recursive occurrence of `Expr` with a fresh type
variable `a`. The result, `ExprF a`, describes **one layer** of an `Expr` with
"holes" of type `a` where children used to be. Haskell:

```haskell
data ExprF a
  = Index  a a
  | Call   a [a]
  | Unary  String a
  | Binary a String a
  | Paren  a
  | Literal Lit
```

Disp — a coproduct *abstracted over the hole type* `a`:

```disp
ExprF := {a : Type} -> Coproduct_of {
  Index   := Pair a a
  Call    := Pair a (List a)
  Unary   := Pair String a
  Binary  := Pair a (Pair String a)
  Paren   := a
  Literal := Lit
}
```

`ExprF a` is **not recursive**. It's a flat, finite shape. `ExprF Nat` is "one
expression node whose children are numbers." `ExprF String` is "one node whose
children are strings." The recursion is *gone from the type* — we'll thread it
back in §5 with a fixed point. This is the central move of the article, and the
one thing disp doesn't have a ready-made name for (you'd write it as the binder
above). Everything else, disp already owns.

---

## 4. `fmap` for free — and disp's `functor` meta-field

Because `ExprF` just has holes of type `a`, "apply a function to every child" is
exactly `fmap` from the `Functor` typeclass. Haskell derives it mechanically:

```haskell
data ExprF a = … deriving (Show, Eq, Functor)

-- gives you, for free:
-- fmap :: (a -> b) -> ExprF a -> ExprF b
```

`fmap g` is precisely our hand-written `apply_expr g` from §2 — but **generated
from the shape**, not typed out arm by arm. This is the boilerplate's death: the
six structural arms become one derived function.

**Here is the disp connection.** Every disp type carries a `MetaShape` record,
and one of its four constitutive fields is named exactly **`functor`**
(`lib/kernel/cut.disp`):

```disp
// from cut.disp — the metadata every type carries:
metashape_header := ["recognizer_params", "functor", "respond", "behavioral_specs"]
trivial_functor  := t
make_meta := {params, respond} ->
  { recognizer_params := params; functor := trivial_functor; respond; … }
```

That `functor` slot **is** the structural home for `ExprF`'s `fmap`. Per
`CLAUDE.md`, "cubical operations live in each type's `functor` meta-field" — i.e.
the type's functorial action (map a function over its parameter / transport
along a path) is registered there. Today most concrete types set it to
`trivial_functor`, with cubical `comp`/`transp` as the live consumer — but the
slot exists for exactly the reason this article needs `Functor`: it's where a
type says *how to map over its holes*. The Haskell article asks the compiler to
*derive* this; disp asks the type to *carry* it.

---

## 5. The fixed point — and why disp gets it for free

`ExprF a` describes one layer. To get arbitrarily deep trees back, we need the
**fixed point of the functor**: the type that, given `f`, wraps an `f` whose
children are themselves wrapped `f`s, forever. Haskell calls it `Term` (a.k.a.
`Fix`):

```haskell
newtype Term f = In { out :: f (Term f) }
--      In  :: f (Term f) -> Term f     -- wrap one layer
--      out :: Term f -> f (Term f)     -- unwrap one layer

type Expr' = Term ExprF                 -- the old `Expr`, recovered
```

The author's own framing: `Term` is a **Y-combinator that runs in the type
system**. `y(f)` is the value-level fixed point of `f`; `Term f` is the
*type*-level fixed point of the functor `f`. `In` ties the recursive knot;
`out` unties one layer of it.

**Disp lives one level below this, so it needs no `Term` newtype.** The
substrate is tree calculus, where the value-level fixed point is a *primitive*:

```disp
fix := …      // the Y-combinator, a kernel idiom (prelude.disp)
// `fix ({self, …} -> body)` hands `body` its own fixed point as `self`.
```

And at the **type** level — where Haskell reaches for `Term` — disp's recursive
types tie the knot through `self`. `make_rec_recognizer` hands a type predicate
*its own fixed point*, so the recognizer can recurse into children. That `self`
**is** `Term`'s knot, and the `triage`/`match` that splits a node **is** `out`.
The worked example is `Ord` in `lib/kernel/types.disp`:

```disp
zero_ord   : Ord := t
omega_plus : Ord -> Ord -> Ord := {a, b} -> t (t a) b

Ord : Type := wait (make_rec_recognizer
  {self, m, v} ->                    // ← `self` = the fixed point, à la `In`/`out`
    triage                           // ← `triage` unpacks one layer = `out`
      (Ok TT)                        //   zero_ord
      ({_} -> Ok FF)                 //   bare stem: not an ordinal
      ({l, r} -> if (is_stem l)      //   omega_plus a b = fork(stem a, b)
        then (bind (self (stem_child l))    // recurse into child `a`  = f (Term f)
                ({oka} -> if oka then (self r) else (Ok FF)))  // …and child `b`
        else (Ok FF))
      v
) (make_meta unit_witness inductive_respond)
```

Read it against `Term f = In { out :: f (Term f) }`: `triage … v` is `out`
(split this node into its one-layer shape), and each `self child` says "the
children are themselves `Ord`s" — i.e. `f (Term f)`. The fixed point Haskell
must *introduce as a newtype* is, in disp, *the way recursive recognizers are
written at all*. Your `Expr` from §1 was legal as written for exactly this
reason.

So the mapping is:

| Haskell `Term` machinery | Disp |
|---|---|
| `In  :: f (Term f) -> Term f` | reconstruct a node — `inj` / the cut's `prod` |
| `out :: Term f -> f (Term f)` | destructure a node — `match` / `triage` |
| `Y` (value fixpoint) | `fix` (kernel primitive) |
| `Term` (type fixpoint) | `self` threaded by `make_rec_recognizer` |

---

## 6. The schemes: `bottomUp` and `topDown`

Now the payoff. With `out` (unwrap), `fmap` (map children), and `In` (rewrap) in
hand, a generic **bottom-up** rewrite is a four-step pipeline, defined *once*,
for *any* functor `f`. Haskell (`>>>` is left-to-right composition):

```haskell
bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn =
      out                     -- 1) unpack to access children
  >>> fmap (bottomUp fn)      -- 2) recurse into each child
  >>> In                      -- 3) repack
  >>> fn                      -- 4) apply the transformation
```

In disp, the cut *fuses* `out … In` (you destructure and rebuild in one `match`),
so `In ∘ fmap (bottomUp fn) ∘ out` collapses into exactly our `apply_expr self`
from §2 — "recurse into each child and reassemble." The whole scheme becomes:

```disp
// `apply_expr self` IS  In ∘ fmap self ∘ out  for the Expr functor.
bottomUp := {fn} -> fix ({self, e} -> fn (apply_expr self e))
//                                    └ apply fn last (step 4) ┘
```

The article's beautiful closing observation: **top-down is the same pipeline run
in reverse.** Swap `>>>` (forward) for `<<<` (backward) — apply `fn` *first*,
then recurse:

```haskell
topDown,  bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
topDown  f = In  <<< fmap (topDown  f) <<< out <<< f   -- fn, then recurse
bottomUp f = out >>> fmap (bottomUp f) >>> In  >>> f   -- recurse, then fn
```

Disp — the duality is just whether `fn` wraps the recursion on the outside or the
inside:

```disp
bottomUp := {fn} -> fix ({self, e} -> fn (apply_expr self e))  // recurse, then fn
topDown  := {fn} -> fix ({self, e} -> apply_expr self (fn e))  // fn, then recurse
```

And `flatten` — the function that started this whole mess — collapses to a
one-liner over `bottomUp`, with the structural recursion **completely gone**:

```haskell
flattenTerm :: Expr' -> Expr'
flattenTerm (In (ParenF e)) = e
flattenTerm other           = other

flatten'' = bottomUp flattenTerm
```

```disp
flatten_term := {e} -> match e { Paren x => x; _ => e }
flatten      := bottomUp flatten_term
```

Compare this to §1's six-arm `flatten`. The boilerplate didn't move — it
**evaporated**. Add a constructor to `Expr` and `flatten` doesn't change at all;
`apply_expr`/`fmap` absorbs it. That is the entire pitch of recursion schemes:
*they are to functional programming what `for`/`while` are to imperative
programming* — the named, reusable iteration patterns you stop hand-rolling.

---

## 7. What's next: the catamorphism — which disp already calls `elim`

`bottomUp`/`topDown` are the simplest schemes: they're **shape-preserving**
(`Term f -> Term f`). The article's sequel generalizes to schemes that
**collapse** a structure to *any* result type `a` — the **catamorphism**, `cata`
(a generalized fold). Its essence: replace each constructor with an algebra
`f a -> a` and fold the tree up.

Disp already has this, and it's load-bearing kernel infrastructure. The general
fold is **`elim`** in `lib/kernel/engine.disp`:

```disp
elim := {dispatcher, motive, cases, target} ->
  select_lazy
    ({_} -> target { motive := motive; cases := cases })  // neutral target: stuck fold
    ({_} -> dispatcher motive cases target)               // concrete target: run the fold
    (is_neutral target)
```

`cases` is precisely the catamorphism's *algebra* (one handler per constructor);
`elim` is `cata`. The recursors built on it — in `lib/kernel/types.disp` — are
catamorphisms for specific functors:

```disp
nat_rec : {P : Nat -> Type} -> P zero -> ({n : Nat} -> P n -> P (succ n)) -> {n : Nat} -> P n
       := {motive, base, step, target} -> elim nat_dispatcher motive { base; step } target

ord_rec := {motive, case_z, case_op, target} -> elim ord_dispatcher motive { case_z; case_op } target
```

And the textbook list catamorphism is literally `foldr`, already in
`lib/std/list.disp`:

```disp
// list_foldr f init [x₀, x₁, …] = f x₀ (f x₁ (… init))   — the list cata
list_foldr := {f, init, xs} -> list_foldr_inner xs f init

// e.g. sum = cata over (List Nat):
let sum = list_foldr add 0
```

Two things to carry away about how disp's version *exceeds* the Haskell article:

1. **The algebra is dependently typed.** `nat_rec`'s `motive` is a `P : Nat -> Type`,
   so the fold can *prove a theorem*, not just compute a value. The Haskell `cata`
   is the non-dependent special case (`P` constant).
2. **It folds over open terms.** Look at `elim`'s first branch: when the `target`
   is a *neutral* (an abstract variable, not a concrete constructor), the fold
   doesn't get stuck or crash — it routes through the type's `respond` meta-field
   via `hyp_reduce` and produces a well-typed *stuck* elimination. You can
   catamorph a value you don't have yet. That's the whole reason disp's
   eliminators verify under Π-binders (see `lib/std/option.disp`'s `option_rec`),
   and it has no analogue in the article's `cata`.

So the article's progression — *boilerplate → factor it → parameterize →
`Functor` → `Term` → schemes → `cata`* — lands, for us, on machinery the kernel
already runs:

```
Haskell article          Disp kernel
────────────────         ─────────────────────────────────────
data Expr = …            Coproduct_of { … } / inj / match     (the §2.6 cut)
fmap (derived)           the `functor` meta-field             (cut.disp)
Term f / In / out        self-threading recognizers + fix     (make_rec_recognizer)
bottomUp / topDown       apply-over-children + fix            (illustrative)
cata / catamorphism      elim · respond ; list_foldr · nat_rec· ord_rec
```

The Haskell programmer *builds the ladder* to reach folds over recursive types.
In disp, "types are predicates and the substrate is a fixed-point calculus"
means you start *standing on it* — the recursion scheme isn't a library you
import, it's the shape the kernel's eliminator already has.

---

## Source

- Patrick Thomson, *An Introduction to Recursion Schemes* —
  <https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html>
- Sequel (catamorphisms & anamorphisms) —
  <https://blog.sumtypeofway.com/posts/recursion-schemes-part-2.html>
- Meijer, Fokkinga, Paterson, *Functional Programming with Bananas, Lenses,
  Envelopes and Barbed Wire* (1991).
- Disp anchors referenced above: `lib/kernel/cut.disp` (`functor` meta-field),
  `lib/kernel/types.disp` (`Ord`, `nat_rec`, `ord_rec`, `Coproduct_of`),
  `lib/kernel/engine.disp` (`elim`), `lib/std/list.disp` (`list_foldr`),
  `lib/std/option.disp` (`option_rec`).
```