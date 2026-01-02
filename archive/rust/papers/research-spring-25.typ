//#import "lib.typ": *
#import "@preview/nifty-ntnu-thesis:0.1.2": *
#let chapters-on-odd = false

#show link: underline

#show: nifty-ntnu-thesis.with(
  title: [Spring 2025 Preliminary Research on Disp: A Decentralized Programming Language],
  short-title: [Disp: Decentralized Lisp],
  authors: ("By: Zyan",),
  titlepage: true,
  date-format: "[year]/[month]/[day]",
  date: datetime(day: 15, month: 5, year: 2025),
  chapters-on-odd: chapters-on-odd,
  bibliography: bibliography("research-spring-25.bib"),
  figure-index: (enabled: false, title: "Figures"),
  table-index: (enabled: false, title: "Tables"),
  listing-index: (enabled: false, title: "Code listings"),
  abstract-en: [
    The Disp programming language is a language for the decentralized internet. The goal is to create a framework for describing and transpiling languages in to each other, to enable truly distributed development with less fear of incompatibility. The end-goal is to have a flexible, modular language with an advanced type system, customizable syntax, and general program synthesis support built-in. For this independent study I spent time researching language design, exploring ideas, and writing type inference code.
  ],
)

= Background & Introduction
<introduction>

Programming languages do not quite have the flexibility that natural languages have. They are hard to evolve over time. They are hard to translate between. They don't have the flexibility of representation that natural languages have. In the context of programming languages, changing your punctuation, or shifting into Yoda-speak doesn't make sense. They also can't deal with ambiguity, or at least varying levels of ambiguity. Programming languages tend to be slower the more abstract we make them. The more high level a language, the easier it is to understand and often to iterate on, but the slower it is, compared to lower level languages which are hard to deal with and often come with high iteration costs. Programming languages also can't easily deal directly with logic or math. There is often a disconnect between the pseudocode of an algorithm presented in a paper, and the actual implementation of that pseudocode, to the point where theoretical algorithm improvements are often not taken advantage of until someone can write a performant implementation of them, and even then such a performant implementation may be non-trivial.

In the future, we will need to be literally 100% confident in the correctness of the software we write, while having it run as fast as humanly possible. We will need this software to be seamlessly integratable and backwards compatible with other languages. We will need tools to automatically implement high-level system models to increase the iteration speed of algorithm development, and these tools need to be easy to use and interpretable.

The Disp programming language is an attempt to create such a language and toolset. It is a language for the decentralized internet that tries to use category theory to frame its systems and design, and automated reasoning to allow formal verification of high-level programs.

Disp is currently still in the research phase, and thus currently has a much more restricted scope. The initial scope is to create a simple dependently typed language that I can play around with to learn latest practices and how type theories and associated algorithms can be implemented.

= Progress
<progress>

== Week 1

For the first week I speculated on various problems and potential solutions that I thought might be feasible.

1. Problem: How can we be sure a program does what it's supposed to (correctness), figure out its properties automatically, and even construct a correct program from a high-level description?
  - Idea: "Tridirectional type systems". Develop a type system that works in three ways. Firstly, it can check if a given program matches a given specification (like checking if a proof proves a theorem). Secondly, given a program, it can automatically determine its specification or properties (like figuring out what theorem a proof proves). Thirdly, given a specification, it could potentially construct a program that meets it (like finding a proof for a theorem).

2. Problem: We need a precise, formal language to define and get intuition behind the rules and components of a programming language, especially when dealing with dependent types (i.e. types that depend on computed values).
  - Idea: Use Category Theory. We can model types as objects and functions (programs) as morphisms (arrows) between them. Full dependent types can be modeled using something called Categories With Families (CwF).

3. Problem: How can we automatically transform a program to make it run faster or use less memory, without changing what it fundamentally computes?
  - Rough Idea: Keep track of different ways program segments can be written while producing the same outcome. Use this knowledge of equivalences to systematically rewrite the program into a functionally identical but more efficient version.
  - More concrete: Use saturating equivalence graphs to keep track of the best program so far, using learned heuristics of what makes a program good + reinforcement learning to figure out which rewrites to make, potentially even combined with live-profiling or various heuristics of various efficiency. @egglog

5. Problem: Can we find deeper mathematical structures to understand programming concepts, particularly different notions of "sameness" (equivalence) between programs or types?
  - Idea: Explore models of $infinity$-category theory via structures like simplicial sets or Homotopy Type Theory. These generalize the concepts of categories to add more nuanced notions of equality at various levels, corresponding the different ways to you write a program of some type or a proof of some theorem as itself a kind of category with internal structure. This might offer more sophisticated ways to reason about program equivalence and structure at a foundational level.

== Week 2

For week two I attempted to wrap my mind around Lean's typing rules. My reflection is that they seem unnecessarily complicated and I want to know how they could be simplified. Ideally, the type system itself should be expressible in the underlying model of computation / calculi.

The typing rules looked at were the following:

$ (Gamma tack.r t : A quad A scripts(=)_(β δ η ζ η) B) / (Gamma tack.r t : B) ("Conv") $

$ (x : U in Gamma) / (Gamma tack.r x : U) ("Var") $
If variable $x$ is declared with type $U$ in the context $Γ$, then $x$ has type $U$.

$ (Gamma tack.r f : Pi (x: A), B quad Gamma tack.r a : A) / (Gamma tack.r f a : B[a slash x]) ("App") $
If f is a function from Π(x: A), B (dependent function type) and a has type A, then the application f a has type B where x is substituted with a.

$ (Gamma, x : A tack.r b : B) / (Gamma tack.r lambda (x: A), b : Pi (x: A), B) ("Lam") $
If in a context extended with x of type A, the term b has type B, then the lambda abstraction λ(x: A), b has the dependent function type Π(x: A), B.

$ () / (Gamma tack.r "Sort" i : "Sort" (i+1)) ("Sort Formation") $
For any universe level i, Sort i is a valid type, and it belongs to the next higher universe Sort (i+1).

$ () / (Gamma tack.r "Prop" : "Sort" 1) ("Prop Formation") $
The universe of propositions, Prop, is a valid type and belongs to Sort 1.

$ (Gamma, x : A tack.r P : "Prop") / (Gamma tack.r Pi (x: A), P : "Prop") ("Impredicative Pi") $
If in a context extended with $x$ of type $A$, $P$ is a proposition, then the dependent function type $Π (x: A)$, $P$ (which represents universal quantification or implication if A is also a proposition) is also a proposition.

$
  (Gamma tack.r A : "Sort" i quad Gamma, x : A tack.r B : "Sort" i) / (Gamma tack.r Pi (x: A), B : "Sort" i) ("Predicative Pi")
$
If $A$ is of sort $i$ and in a context extended with $x : A, B$ is also of sort $i$, then the dependent function type $Π (x: A)$, $B$ is also of sort $i$. This rule is predicative, as the universe level of the function type is the same as the universe level of the argument and return types.

$ ("Inductive type" I "with constructor" c : (...) -> I) / (Gamma tack.r c : (...) -> I) ("Inductive Intro") $
For each constructor $c$ of an inductive type $I$, there is a constructor‐introduction rule that reflects its type signature. (Simplified representation, single‐constructor case.)

$
  (Gamma tack.r A : "Type" i quad Gamma tack.r R : A -> A -> "Prop") / (Gamma tack.r "Quot" R : "Type" i) ("Quot Type Formation")
$

Given a type $A$ and a relation $R$ on $A$, we can form the quotient type $"Quot" R$.

$
  (Gamma tack.r A : "Type" i quad Gamma tack.r R : A -> A -> "Prop" quad Gamma tack.r a : A) / (Gamma tack.r "Quot"."mk" R space a : "Quot" R) ("Quot Constructor")
$
If $a$ is of type $A$, we can lift it to the quotient type $"Quot" R$ using $"Quot"."mk" R$.

$ (Gamma tack.r t : A quad A scripts(=)_(β δ η ζ η) B) / (Gamma tack.r t : B) ("Conv") $
If a term $t$ has type $A$, and $A$ is definitionally equal to $B$ (via $β$, $δ$, $ι$, $ζ$, $η$ reductions and equivalence), then $t$ also has type $B$.

== Week 3

In week three I reflected on week two's issue sequence calculus complexity. I broke down the problem of language-building into the following:

- Computational Model
  - The language you use to represent programs, i.e. the lambda calculus, machine code, intermediate-representations, anything that can represent computation and simulate computational evaluation. This should ideally be Turing-Complete (which means it should be able to represent any possible program, even those that run forever) and be easy to reason about.
  - Intuitively, a model of computation requires some physical process to "implement" it, so in a sense, a program is a configuration of a physical process that manipulates information.
- Verification System
  - In a sense a verification system is just a bunch of other programs you run on programs that resolve to true if a property holds for its input. Types are in some sense _predicate-functions_.
  - The question now is "what makes a good type system?". Efficiency at representing constraints on a program? How does type inference fit in here?

== Week 4

In week four I started exploring the idea of a system of types-as-predicates.

Intuitively, we start with the idea that a type system (specifically: type checker) can be thought of a function of type: $"Program" times "Type" -> "Bool"$. Then since types are in theory "composable" similar to terms, perhaps we can re-use term composition to where a base type is simply a function.

- A base type is a program of type $"Program" -> "Bool"$.
- A basic type constructor is something of type $("Program" -> "Bool") times ("Program" -> "Bool") -> ("Program" -> "Bool")$.

I tried to extend this to dependent types, where you have a type that is dependent on a program, i.e. perhaps something of the sort $("Program" -> "Bool") times ("Program" -> ("Program" -> "Bool")) -> ("Program" -> "Bool")$, but I later learned issue comes when dealing with applications and variable lookups, as most lambda terms require environments of some kind.

Week four I also hypothesized as to whether this system could be made in a sense "agnostic" to the underlying computational framework, i.e. where you could potentially "bootstrap" a type system onto any kind of turing-complete (or perhaps even not turing complete?) computation / reduction system. Would it be possible then to write type systems for turing machines? What would that even look like? I explored this idea through simple correpondences between things like the basic Untyped Lambda Calculus (ULC) and versions of ULC where you add extra "base" constructions like natural numbers and relevant functions, which are themselves implementable in the ULC, but in a possibly ungeneralizable sense this makes a different computational model. The question then is how is it best to think about this correspondence, the idea of drawing correspondences between constructions in the language and constructions in the meta-language, and if this correspondence could allow for kind of a continuous chain of intensional (i.e. with respect to the internal structure) transformations between similar computation models (such as ULC and ULC+Natural Numbers) and more different computation models (such as ULC and Turing Machines).

As an aside I also looked into categorical models of eager vs lazy evaluation systems to see if they could be related in the same kind of intentional manner as ULC and ULC+Nat. Supposedly they correspond to Control and Cocontrol categories. @ControlCategories

== Week 5

Week five I continued to explore the types-as-predicates idea, attempting to reconcile it with notions of typing contexts and environments, as well as type inference. I also explored something called "Institution Theory". @institution_theory_iep @goguen1992institutions

First, my idea was to treat types instead as functions of programs _combined with_ typing contexts, i.e. $"Program" times "Context" -> "Bool"$, which more closely reflects the requirements of type checkers to look up variables in some context. Base terms would be function that represent variants of the base computational model, e.g. at `Nat` type would recognize instances of `1, 2, 3, 4, ...`

Ok the topic of computational models, I also investigated institution theory which sells itself as a framework for constructing logics—which I assume includes type theories. Institution theory is still a bit of an enigma to me but the basics of it is that there are 4 parts (its defined in terms of category theory concepts). I give Propositional Logic as an instance of an institution.
- A #link("https://en.wikipedia.org/wiki/Category_(mathematics)")[category] of "Signatures" *$"Sig"$*
  - Ex: sets of propositional variables and functions between these sets. (Equivalent to $"Set"$)
- A functor $"Sentences" : "Sig" -> "Set"$
  - Ex: the set of all well-formed propositional formulas formable using some set of variables and some logical connectives ($->, and, or , not$, etc.)
- A contravariant functor $"Mod" : "Sig" -> "Cat"^"op"$ mapping each signature to a category of its models, and each morphism between two signatures to a functor between models.
  - Ex:
    - Given an object $P in "ob"("Sig")$: One gets a category of models. A model in propositional logic is an assignment of variables to true or false, i.e. given a signature $P$, and model $M$ is a function $M : P -> {"True", "False"}$, (which can also be thought of as a subset of a signature $P$ denoting which elements in $P$ were assigned to true). The category of models is simply all such possible models as the objects and subset relations as the morphisms, creating a partially ordered set.
    - Given a morphism $phi : P -> P^prime in "Sig"$, this creates a functor between two categories of models (i.e. a mapping between two posets of variable assignments).
- A satisfaction ("truth") relation between sentences $s in "Sen"(P)$ and models $M in "Mod"(P)$ for a given variable set $P$ denoting that $M tack.r.double_P s$ if and only if the variable assignment described by the model $M$ results in the sentence $s$ resolving to true.
- An satisfaction condition saying that $"Mod"(phi)(M^prime) tack.r.double_P s <-> M^prime tack.r.double_(P^prime) "Sen"(phi)(s)$, basically saying that the truth of sentence in a logical system must be invariant under changes of notation.

== Week 6

Week six I really jumped into the weeds and started to think about what would be required to _actually implement_ a types-as-predicates system and I came to realize that, at least with the lambda calculus and how type systems are typically defined, this wouldn't really work too well.

The fundamental issue is that a given type, lets say (e.g. `Nat`) must be able to recognize basic `Nat`s, but also must be able to recognize, for example, the body of a function that returns a `Nat` given the type of the argument of the function. This means it needs to look up the type of variables in a typing context, as well as reason about the components of a function application, which requires inferring the type of the function involved and is non-trivial. For example, imagine trying to check if the following expression is of type `Nat -> Nat`

`{x} -> +(1, x) : Nat -> Nat`

To verify `{x} -> ((+ 1) x) : Nat -> Nat`, we need to verify that `((+ 1) x) : Nat` given a typing context associating `x` with `Nat`. We lookup the type of `x` to be `Nat` and since we require the expression to solve to `Nat`, we need to check that `(+ 1)` is of type `Nat -> Nat`. We repeat the process to check that `+` must be of `Nat -> (Nat -> Nat)`, which we can conclude so long as its type is in the context, and thus we have checked the original statement.

It is I think _technically possible_ to implement this kind of type checking in a types-as-predicates paradigm, but it is unclear the degree that this would actually create the kind of modularity desired, at least with the lambda calculus.

Where the idea really starts getting tenuous (although perhaps still feasible, it is unclear) is where we get to trying to implement more complicated type systems like dependent types or linear type theories.

== Week 7

Week seven I took a break to work on other projects, and to self-evaluate on what I had done so far. I realized that I was getting too much into theory and that I should probably take a step back to work on implementation and actually try to implement a type checker to see how far I get.

The self-evaluation was primarily about what the ultimate goal of the project is, i.e. what a "decentralized" language would truly look like in practice. As quick illustration of properties of this language I am going for:
- The syntax of the language should be fully customizable by users of the language, and be parse-able to a flexible and uniform backend representation that encodes term and type structure. This should include the ability to visualize the language through non-textual mediums.
- Programs written in less-complex type systems (such as the simply typed lambda calculus) should be transpilable automatically into programs in more complex type systems. The reverse case should also be possible, so long as there is a meaningful reverse translation or the program doesn't make use of the more advanced type system's features.
- The above properties should enable people to use their own versions of the language with minimimal or controllable compatibility issues with other people's versions, enabling truly decentralized language development.
- The language should come with a decently advanced type system out-of-the-box with support for homotopy type theory for theorem proving, algebraic effects for managing I/O, and linear/affine types for modeling object lifetimes.
- The language should come with state-of-the-art facilities for generating terms of arbitrary types for use in theorem proving or code generation.
  - This should also suppoer CEGIS (Counter-Example Guided Inductive Synthesis)


== Week 8

Week 8 I started thinking about type inference, specifically bidirectional type systems where you have both a "checking" function that takes a term and a type (and a context) and verifies that the term is of the type in the context, as well as an "inference" function that simply takes a term and a context and using the primitives in the term tries to infer what the type might be, these procedures when actually implemented are typically mutually recursive (i.e. they call each other).

Type inference can:
- Infer the "default type" of constants like integers or strings or booleans.
- Look up variables in some typing context to get their type.
- Look at the function and argument of an application, recursively infer the type of the argument, and then use that to infer the type of the function.
- Look at a function, and its body, see if the argument of the function is forced to be a value by anything it is used for in the function body, using that inferred argument type, infer the type of the body to infer the type of the whole function. Often times languages require programmers to denote at least the argument type for a function to make it easier (otherwise you may need to keep track of unknown types during inference).

Type checking can:
- Verify that a term is of a given type, potentially using the inferred type of sub-terms.
- Check if a constant has the expected type.
- Look up variables in the typing context and compare their type with the expected type.
- For applications, recursively check the type of the argument and then use that to check the type of the function.
- For functions, extend the context with the expected argument type and check the type of the body.

These functions can be combined into something called "bidirectional" type system. While full type inference is feasible in many common type theories, often in $O(n)$ or polynomial time, for complicated systems such as dependent types, type inference is undecidable in general @depTypesInferenceUndecidable. This issue can be solved by combining bottom-up type inference with top-down type checking, where the programmer simply has to give the types for some expressions and everything else can be inferred.

For the implementation of a bidirectional type system, type checking can be combined with the lowering of a parsed syntax tree, resulting in a function like the following:

```rust
fn lower(parsed_term: ParseTree, expected_type: Option<Term>, ctx: &mut Context) -> Result<(Term, Term), LoweringError> > {
  // ...
}
```
Where the lowering step does type checking if given an `expected_type = Some(Term)`, otherwise it does type inference if given `expected_type = None`, returning the lowered `Term` and either the `expected_type` passed, or whatever type was inferred from the `parsed_term` and its `ctx: Context`.

== Week 9

Week nine I felt like I understood the basics of bidirectional type theories, but I didn't quite understood how they applied in the case of dependent types where a type might depend on a term. Thus, I found and attempted to understand the paper called "Generic bidirectional typing for dependent type theories" @felicissimo2024genericbidirectionaltypingdependent. This paper unfortunately was not comprehensible at my level—or perhaps I was simply overloaded by the unfamiliar syntax, so I pivoted to thinking about implementation and what high-level functions I'd need to implement, such as `reduce` and `infer` and `check`. (I hadn't quite realized you could join these things together at this point.)


== Week 10-12

Week ten through twelve was mostly implementation work. To get an illustration of the kind of work done that was done during these few weeks, here is a non-exhaustive list of implementation challenges I faced.

- Realizing I could merge `infer` and `check` into a single function.
- Researching various ways to represent partially and fully-typed expressions, and what it even means to have types of types / type hierarchies. I looked into graph-based type checking @verdonck2023graphbased, and even hypergraphs @yasen2017unificationHypergraphs. I eventually settled on a simple recursive approach done in conjunction with type checking/inference while lowering the syntax tree.
- Actually implementing the `lower` function and all its corresponding sub-variants that needed to be handled.
  - Figuring out how to lower functions with multiple inputs where later input's types may be dependent on previous inputs in the function, thus requiring careful note of in what order and how the the types are lowered.
  - Figuring out how to make the `lower` function, and its associated sub-functions (`lower_func`, `lower_set`, `lower_abs`) work both for values and types (that may contain values).
- Investigating how to do efficient term reduction (i.e. term reduction where you cache the reduced versions of terms). I was having trouble with this as I didn't know what to do in contexts where a variable was assigned to a value outside of the scope of the term being reduced. I thought perhaps I could replace immediately whenever I found any bound expression while lowering a variable with its already-lowered definition, but that seemed unsatisfying for a couple reasons:
  1. I would loose the ability to reverse the parsed structure and understand the components a given parsed structure was made from, even if the components were more for human interpretability than computer evaluation.
  2. What about reduction on subterms where the variable is bound to a lambda abstraction? I suppose this wouldn't matter as much since the variables don't have any assignments, they are just free variables, but it still feels like there could be something better.
  3. How to deal with multiple caches of terms that are alpha-equivalent? This seems like a major problem with my design as it currently is (where identifiers are explicitly included in the representation of terms). I could make a version without identifiers in theory but that would run into (1), reversing the parsed structure for interpretability.

== Week 13 (Final Week)

Week thirteen was mostly spent working on other projects, finishing up the code and this report. It was also spent getting nerdsniped by a friend. You see, the ultimate goal of this rust implementation of dependent types, beyond simply gaining experience, is to be able to create a strong enough backbone to bootstrap the language interpreter/checker/compiler within itself to harness all the power of dependent types and everything else (formal verification, term inference, etc.) to make the language the best and as modular as can be, make it truly decentralized. One of the core things in this goal is being able to deal with variability in language structures, type theories, computational substrates, etc, without needing too much plumbing to translate code into data and back. So while I was working on the reduction algorithm for lowering, my friend sent me a link to this website: https://treecalcul.us/. The tree calculus is a model of computation that not only has no long-range links / need for variables like the lambda calculus does—at first glance seemingly fixing the issue I was having with term reduction depending on context to be cache-able. It also potentially supplants the need to even have a compiler in the first place, as tree calculus terms can inspect other tree calculus terms by default @jay2020treebook.

I investigated some more and the author seems to be trying to do very similar things to what I'm doing @jay2025arrogance. But the more interesting part is that the tree calculus is self-interpretable by default. You can write type checkers in the language and run them on programs in the language. Same with type inference, static analysis, compilation and likely everything else that needs some kind of _encoding_ of programs to manipulate their structure, can be done natively in the tree calculus, which I think is extraordinarily cool and is what I was going for initially: a customizable programming language that can be modified from within itself.

= Next Steps

I plan to at least attempt to finish the type lowering system I've been working on for the past weeks. I've learned much from it but I would at least like to see it compiled.

After that, I really want to check out the tree calculus more. There is a very real chance it could be a dead-end, but the promise of self-inspectable programming languages is very promising, and I've already managed to code an interpreted in only 2 days (with a little AI help for the parser). It can already do boolean negation!

#figure(
  image("tree-calculus-negation.png", width: 100%),
  caption: [
    We define `true`, `false`, `not`, and then verify that evaluating `not true` resolves `false` and same for `not false` resolving true.
  ],
)

I hope to continue working on this over the summer, continue learning about the tree calculus and how feasible it is for type theory formalization and compilation. Thanks for reading!
