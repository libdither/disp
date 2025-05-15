//#import "lib.typ": *
#import "@preview/nifty-ntnu-thesis:0.1.2": *
#let chapters-on-odd = false
#show: nifty-ntnu-thesis.with(
  title: [Disp: Decentralized Lisp],
  short-title: [Disp: Decentralized Lisp],
  authors: ("Zyan",),
  titlepage: true,
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

== Week 4 - Feb 21st - Feb 28th

In week four I started exploring the idea of a system of types-as-predicates.

Intuitively, we start with the idea that a type system (specifically: type checker) can be thought of a function of type: $"Program" times "Type" -> "Bool"$. Then since types are in theory "composable" similar to terms, perhaps we can re-use term composition to where a base type is simply a function.

- A base type is a program of type $"Program" -> "Bool"$.
- A basic type constructor is something of type $("Program" -> "Bool") times ("Program" -> "Bool") -> ("Program" -> "Bool")$.

I tried to extend this to dependent types, where you have a type that is dependent on a program, i.e. perhaps something of the sort $("Program" -> "Bool") times ("Program" -> ("Program" -> "Bool")) -> ("Program" -> "Bool")$, but I later learned issue comes when dealing with applications and variable lookups, as most lambda terms require environments of some kind.

Week four I also hypothesized as to whether this system could be made in a sense "agnostic" to the underlying computational framework, i.e. where you could potentially "bootstrap" a type system onto any kind of turing-complete (or perhaps even not turing complete?) computation / reduction system. Would it be possible then to write type systems for turing machines? What would that even look like? I explored this idea through simple correpondences between things like the basic Untyped Lambda Calculus (ULC) and versions of ULC where you add extra "base" constructions like natural numbers and relevant functions, which are themselves implementable in the ULC, but in a possibly ungeneralizable sense this makes a different computational model. The question then is how is it best to think about this correspondence, the idea of drawing correspondences between constructions in the language and constructions in the meta-language, and if this correspondence could allow for kind of a continuous chain of intensional (i.e. with respect to the internal structure) transformations between similar computation models (such as ULC and ULC+Natural Numbers) and more different computation models (such as ULC and Turing Machines).

As an aside I also looked into categorical models of eager vs lazy evaluation systems to see if they could be related in the same kind of intentional manner as ULC and ULC+Nat. Supposedly they correspond to Control and Cocontrol categories. @ControlCategories

== Week 5

Week five I continued to explore the types-as-predicates idea, attempting to reconcile it with notions of typing contexts and environments, as well as type inference. I also explored something called "Institution Theory". @institution_theory_iep @goguen1992institutions

First, my idea was to treat types instead as functions of programs _combined with_ typing contexts, i.e. $"Program" times "Context" -> "Bool"$, which more closely reflects the requirements of type checkers to look up variables in some context.

TODO: Talk about institution theory as a further area of research.


== Week 6

Week six I really jumped into the weeds and started to think about what would be required to _actually implement_ a types-as-predicates system and I came to realize that, at least with the lambda calculus and how type systems are typically defined, this wouldn't really work too well.

The fundamental issue is that a given type, lets say (e.g. `Nat`) must be able to recognize basic `Nat`s, but also must be able to recognize, for example, the body of a function that returns a `Nat` given the type of the argument of the function. This means it needs to look up the type of variables in a typing context, as well as reason about the components of a function application, which requires inferring the type of the function involved and is non-trivial. For example, imagine trying to check if the following expression is of type `Nat -> Nat`

`{x} -> +(1, x) : Nat -> Nat`

To verify `{x} -> ((+ 1) x) : Nat -> Nat`, we need to verify that `((+ 1) x) : Nat` given a typing context associating `x` with `Nat`. We lookup the type of `x` to be `Nat` and since we require the expression to solve to `Nat`, we need to check that `(+ 1)` is of type `Nat -> Nat`. We repeat the process to check that `+` must be of `Nat -> (Nat -> Nat)`, which we can conclude so long as its type is in the context, and thus we have checked the original statement.

It is I think _technically possible_ to implement this kind of type checking in a types-as-predicates paradigm, but it is unclear the degree that this would actually create the kind of modularity desired, at least with the lambda calculus.

TODO: Elaborate more on why dependent types and type inference make this even harder lol.

== Week 7

Week seven I took a break to work on other projects, and to self-evaluate on what I had done so far. I realized that I was getting too much into theory and that I should probably take a step back to work on implementation and actually try to implement a type checker to see how far I get.

== Week 8

Week 8 I started thinking about type inference, specifically bidirectional type systems where you have both a "checking" function that takes a term and a type (and a context) and verifies that the term is of the type in the context, as well as an "inference" function that simply takes a term and a context and using the primitives in the term tries to infer what the type might be, these procedures when actually implemented are typically mutually recursive (i.e. they call each other). Inference is used to get the type of primitive terms or variables.



== Week 9



== Week 10
== Week 11
== Week 12
== Week 13 (Final)

Pondered


/*
= Using the Template
<chap:usage>
== Thesis Setup
<sec:setup>
The document class is initialized by calling
```typst #show: nifty-ntnu-thesis.with()``` at the beginning of your `.typ` file. Currently it only supports english. The `nifty-ntnu-thesis` function has a number of options you can set, most of which will be described in this document. The rest will be documented in this templates repository.

The titlepage at the beginning of this document is a placeholder to be used when writing
the thesis. This should be removed before handing in the thesis, by settting `titlepage: false`.
Instead the official NTNU titlepage for the corresponding thesis type
should be added as described on Innsida.#footnote[see #link("https://innsida.ntnu.no/wiki/-/wiki/English/Finalizing+the+bachelor+and+master+thesis") for bachelor and master, and #link("https://innsida.ntnu.no/wiki/-/wiki/English/Printing+your+thesis")
  for PhD.]

== Title, Author, and Date
<title-author-and-date>
The title of your thesis should be set by changing the `title` parameter of the template. The title will appear on the titlepage as well as in the running header of the even numbered pages. If the title is too long for the header, you can use `short-title` to set a version for the header.

The authors should be listed with full names in the `authors` parameter. This is an array, with multiple authors separated by a comma. As with the title, you can use `short-author` to set a version for the header.

Use `date` to set the date of the document. It will only appear on
the temporary title page. To keep track of temporary versions, it can be
a good idea to use `date: datetime.today()` while working on the thesis.

== Page Layout
<page-layout>
The document class is designed to work with twosided printing. This
means that all chapters start on odd (right hand) pages, and that blank
pages are inserted where needed to make sure this happens. However,
since the theses are very often read on displays, the margins are kept
the same on even and odd pages in order to avoid that the page is
jumping back and forth upon reading.

By default this is turned off. You can turn it on by setting
`chapters-on-odd: false` at the top of the file.

== Structuring Elements
<structuring-elements>
The standard typst headings are supported, and are set using =.

=== This is a level 3 heading
<this-is-a-subsection>

==== This is level 4 heading
<this-is-a-subsubsection>

===== This is a level 5 heading
<this-is-a-paragraph>

Headings up to level 3 will be included in the table of
contents, whereas the lower level structuring elements will not appear
there. Don’t use too many levels of headings; how many are appropriate,
will depend on the size of the document. Also, don’t use headings too
frequently.

Make sure that the chapter and section headings are correctly
capitalised depending on the language of the thesis, e.g.,
'#emph[Correct Capitalisation of Titles in English];' vs. '#emph[Korrekt
  staving av titler på norsk];'.

Simple paragraphs are the lowest structuring elements and should be used
the most. They are made by leaving one (or more) blank line(s) in the
`.typ` file. In the typeset document they will appear indented and with
no vertical space between them.

== Lists
<lists>
Numbered and unnumbered lists are used just as in regular typst, but are typeset
somewhat more densely and with other labels. Unnumbered list:

- first item

- second item

  - first subitem

  - second subitem

    - first subsubitem

    - second subsubitem

- last item

Numbered list:

+ first item

+ second item

  + first subitem

  + second subitem

    + first subsubitem

    + second subsubitem

+ last item


== Figures
<figures>
Figures are added using ```typst #figure()```. An example is shown in
#link(<fig:mapNTNU>)[2.1];. By default figures are placed in the flow, exactly where it was specified. To change this set the ```placement``` option to either `top`, `bottom`, or `auto`. To add an image, use ```typst #image()``` and set the `height` or `width` to include the graphics file. If the caption consists of a single sentence fragment (incomplete sentence), it should not be punctuated.


#figure(
  image("figures/kart_student.png", width: 50%),
  caption: [
    The map shows the three main campuses of NTNU.
  ],
)
<fig:mapNTNU>

For figures compsed of several sub-figures, the `subpar` module has been used. To use it, use the function ```typst #subfigure()```. See #link(<fig:subfig>)[2.2]
with #link(<sfig:a>)[\[sfig:a\]] for an example.

#subfigure(
  figure(
    image("figures/kart_student.png", width: 100%),
    caption: [First sub-figure],
  ),
  <sfig:a>,
  figure(
    image("figures/kart_student.png", width: 100%),
    caption: [Second sub-figure],
  ),
  <sfig:b>,
  columns: (1fr, 1fr),
  caption: [A figure composed of two sub-figures. It has a long caption in order to demonstrate how that is typeset.
  ],
  <fig:subfig>,
)
```typst
#subfigure(
  figure(image("figures/kart_student.png", width: 100%),
    caption: [First sub-figure]), <sfig:a>,
  figure(image("figures/kart_student.png", width: 100%),
    caption: [Second sub-figure]), <sfig:b>,
    columns: (1fr, 1fr),
   caption: [A figure composed of two sub-figures. It has a long caption in order to demonstrate how that is typeset.
  ],
<fig:subfig>
```

== Tables
<tables>
Tables are added using ```typst #table()```, wrapped in a ```typst #figure()``` to allow referencing. An example is given in
@tab:example1. If the caption consists
of a single sentence fragment (incomplete sentence), it should not be
punctuated.


#figure(
  table(
    stroke: none,
    columns: 2,
    table.hline(),
    table.header([*age*], [*IQ*]),
    table.hline(),
    [10], [110],
    [20], [120],
    [30], [145],
    [40], [120],
    [50], [100],
    table.hline(),
  ),
  caption: [A simple, manually formatted example table],
) <tab:example1>
Tables can also be automatically generated from CSV files #footnote(link("https://typst.app/docs/reference/data-loading/csv/")).

== Listings
<listings>
Code listings are are also wrapped in a ```typst #figure()```. Code listings are defined by using three ``` `backticks` ```. The programming language can also be provided. See the typst documentation for details. The
code is set with the monospace font, and the font size is reduced to
allow for code lines up to at least 60 characters without causing line
breaks. If the caption consists of a single sentence
fragment (incomplete sentence), it should not be punctuated.

#figure(caption: "Python code in typst")[
  ```python
  import numpy as np
  import matplotlib.pyplot as plt

  x = np.linspace(0, 1)
  y = np.sin(2 * np.pi * x)

  plt.plot(x, y)
  plt.show()
  ```
]<lst:python>
#figure(caption: "C++ code in typst")[
  ```cpp
  #include <iostream>
  using namespace std;

  int main()
  {
    cout << "Hello, World!" << endl;
    return 0;
  }
  ```]<lst:cpp>


== Equations
<equations>
Equations are typeset as normally in typst. It is common to consider
equations part of the surrounding sentences, and include punctuation in
the equations accordingly, e.g.,
$ f (x) = integral_1^x 1 / y thin d y = ln x thin . $ <logarithm>
For more advanced symbols like, e.g., $grad, pdv(x, y)$, the `physica` module is preloaded.
As you can see, the simple math syntax makes typst very easy to use.
== Fonts
<fonts>
Charter at 11pt with the has been selected as the main font for the thesis template. For code examples, the monospaced font should be used – for this, a scaled
version of the DejaVu Sans Mono to match the main font is preselected.

== Cross References
<sec:crossref>
Cross references are inserted using `=` in typst. For examples on usage, see @sec:crossref in @chap:usage, @tab:example1
@fig:mapNTNU, @logarithm,
@lst:cpp and #link(<app:additional>)[Appendix A];.



== Bibliography
<bibliography>
The bibliography is typeset as in standard typst. It is added in the initializing function as such: ```typst bibliography: bibliography("thesis.bib")```.
With this setup, using `@` will give a number
only~@landes1951scrutiny, and ```typst #cite(, form: "prose") ``` will give author and number like this: #cite(<landes1951scrutiny>, form: "prose");.


== Appendices
<appendices>
Additional material that does not fit in the main thesis but may still
be relevant to share, e.g., raw data from experiments and surveys, code
listings, additional plots, pre-project reports, project agreements,
contracts, logs etc., can be put in appendices. Simply issue the command
```typst #show: appendix``` in the main `.typst` file, and then the following chapters become appendices. See #link(<app:additional>)[Appendix A]
for an example.

= Thesis Structure
<thesis-structure>
The following is lifted more or less directly from the original template.

The structure of the thesis, i.e., which chapters and other document
elements that should be included, depends on several factors such as the
study level (bachelor, master, PhD), the type of project it describes
(development, research, investigation, consulting), and the diversity
(narrow, broad). Thus, there are no exact rules for how to do it, so
whatever follows should be taken as guidelines only.

A thesis, like any book or report, can typically be divided into three
parts: front matter, body matter, and back matter. Of these, the body
matter is by far the most important one, and also the one that varies
the most between thesis types.

== Front Matter
<sec:frontmatter>
The front matter is everything that comes before the main part of the
thesis. It is common to use roman page numbers for this part to indicate
this. The minimum required front matter consists of a title page,
abstract(s), and a table of contents. A more complete front matter, in a
typical order, is as follows.

/ Title page\:: #block[
    The title page should, at minimum, include the thesis title, authors and
    a date. A more complete title page would also include the name of the
    study programme, and possibly the thesis supervisor(s). See
    #link(<sec:setup>)[2.1];.
  ]

/ Abstracts\:: #block[
    The abstract should be an extremely condensed version of the thesis.
    Think one sentence with the main message from each of the chapters of
    the body matter as a starting point.
    #cite(<landes1951scrutiny>, form: "prose") have given some very nice
    instructions on how to write a good abstract. A thesis from a Norwegian
    Univeristy should contain abstracts in both Norwegian and English
    irrespectively of the thesis language (typically with the thesis
    language coming first).
  ]

/ Dedication\:: #block[
    If you wish to dedicate the thesis to someone (increasingly common with
    increasing study level), you may add a separate page with a dedication
    here. Since a dedication is a personal statement, no template is given.
    Design it according to your preference.
  ]

/ Acknowledgements\:: #block[
    If there is someone who deserves a 'thank you', you may add
    acknowledgements here. If so, make it an unnumbered chapter.
  ]

/ Table of contents\:: #block[
    A table of contents should always be present in a document at the size
    of a thesis. It is generated automatically using the `outline()`
    command. The one generated by this document class also contains the
    front matter and unnumbered chapters.
  ]

/ List of figures\:: #block[
    If the thesis contains many figures that the reader might want to refer
    back to, a list of figures can be included here. It is generated using
    `outline()`.
  ]

/ List of tables\:: #block[
    If the thesis contains many tables that the reader might want to refer
    back to, a list of tables can be included here. It is generated using
    `outline()`.
  ]

/ List of code listings\:: #block[
    If the thesis contains many code listings that the reader might want to
    refer back to, a list of code listings can be included here. It is
    generated using `outline()`.
  ]

/ Other lists\:: #block[
    If there are other list you would like to include, this would be a good
    place. Examples could be lists of definitions, theorems, nomenclature,
    abbreviations, glossary etc.
  ]

/ Preface or Foreword\:: #block[
    A preface or foreword is a good place to make other personal statements
    that do not fit whithin the body matter. This could be information about
    the circumstances of the thesis, your motivation for choosing it, or
    possibly information about an employer or an external company for which
    it has been written. Add this in the initializing function of this template.
  ]

== Body Matter
<body-matter>
The body matter consists of the main chapters of the thesis. It starts
the Arabic page numbering with page~1. There is a great diversity in the
structure chosen for different thesis types. Common to almost all is
that the first chapter is an introduction, and that the last one is a
conclusion followed by the bibliography.

=== Development Project
<sec:development>
For many bachelor and some master projects in computer science, the main
task is to develop something, typically a software prototype, for an
'employer' (e.g., an external company or a research group). A thesis
describing such a project is typically structured as a software
development report whith more or less the following chapters:

/ Introduction\:: #block[
    The introduction of the thesis should take the reader all the way from
    the big picture and context of the project to the concrete task that has
    been solved in the thesis. A nice skeleton for a good introduction was
    given by #cite(<claerbout1991scrutiny>, form: "prose");:
    #emph[review–claim–agenda];. In the review part, the background of the
    project is covered. This leads up to your claim, which is typically that
    some entity (software, device) or knowledge (research questions) is
    missing and sorely needed. The agenda part briefly summarises how your
    thesis contributes.
  ]

/ Requirements\:: #block[
    The requirements chapter should lead up to a concrete description of
    both the functional and non-functional requirements for whatever is to
    be developed at both a high level (use cases) and lower levels (low
    level use cases, requirements). If a classical waterfall development
    process is followed, this chapter is the product of the requirement
    phase. If a more agile model like, e.g., SCRUM is followed, the
    requirements will appear through the project as, e.g., the user stories
    developed in the sprint planning meetings.
  ]

/ Technical design\:: #block[
    The technical design chapter describes the big picture of the chosen
    solution. For a software development project, this would typically
    contain the system arcitechture (client-server, cloud, databases,
    networking, services etc.); both how it was solved, and, more
    importantly, why this architecture was chosen.
  ]

/ Development Process\:: #block[
    In this chapter, you should describe the process that was followed. It
    should cover the process model, why it was chosen, and how it was
    implemented, including tools for project management, documentation etc.
    Depending on how you write the other chapters, there may be good reasons
    to place this chapters somewhere else in the thesis.
  ]

/ Implementation\:: #block[
    Here you should describe the more technical details of the solution.
    Which tools were used (programming languages, libraries, IDEs, APIs,
    frameworks, etc.). It is a good idea to give some code examples. If
    class diagrams, database models etc. were not presented in the technical
    design chapter, they can be included here.
  ]

/ Deployment\:: #block[
    This chapter should describe how your solution can be deployed on the
    employer’s system. It should include technical details on how to set it
    up, as well as discussions on choices made concerning scalability,
    maintenance, etc.
  ]

/ Testing and user feedback\:: #block[
    This chapter should describe how the system was tested during and after
    development. This would cover everything from unit testing to user
    testing; black-box vs. white-box; how it was done, what was learned from
    the testing, and what impact it had on the product and process.
  ]

/ Discussion\:: #block[
    Here you should discuss all aspect of your thesis and project. How did
    the process work? Which choices did you make, and what did you learn
    from it? What were the pros and cons? What would you have done
    differently if you were to undertake the same project over again, both
    in terms of process and product? What are the societal consequences of
    your work?
  ]

/ Conclusion\:: #block[
    The conclusion chapter is usually quite short – a paragraph or two –
    mainly summarising what was achieved in the project. It should answer
    the #emph[claim] part of the introduction. It should also say something
    about what comes next ('future work').
  ]

/ Bibliography\:: #block[
    The bibliography should be a list of quality-assured peer-reviewed
    published material that you have used throughout the work with your
    thesis. All items in the bibliography should be referenced in the text.
    The references should be correctly formatted depending on their type
    (book, journal article, conference publication, thesis etc.). The bibliography should
    not contain links to arbitrary dynamic web pages where the content is
    subject to change at any point of time. Such links, if necessary, should
    rather be included as footnotes throughout the document. The main point
    of the bibliography is to back up your claims with quality-assured
    material that future readers will actually be able to retrieve years
    ahead.
  ]

=== Research Project
<sec:resesarch>
For many master and some bachelor projects in computer science, the main
task is to gain knew knowledge about something. A thesis describing such
a project is typically structed as an extended form of a scientific
paper, following the so-called IMRaD (Introduction, Method, Results, and
Discussion) model:

/ Introduction\:: #block[
    See #link(<sec:development>)[3.2.1];.
  ]

/ Background\:: #block[
    Research projects should always be based on previous research on the
    same and/or related topics. This should be described as a background to
    the thesis with adequate bibliographical references. If the material
    needed is too voluminous to fit nicely in the review part of the
    introduction, it can be presented in a separate background chapter.
  ]

/ Method\:: #block[
    The method chapter should describe in detail which activities you
    undertake to answer the research questions presented in the
    introduction, and why they were chosen. This includes detailed
    descriptions of experiments, surveys, computations, data analysis,
    statistical tests etc.
  ]

/ Results\:: #block[
    The results chapter should simply present the results of applying the
    methods presented in the method chapter without further ado. This
    chapter will typically contain many graphs, tables, etc. Sometimes it is
    natural to discuss the results as they are presented, combining them
    into a 'Results and Discussion' chapter, but more often they are kept
    separate.
  ]

/ Discussion\:: #block[
    See #link(<sec:development>)[3.2.1];.
  ]

/ Conclusion\:: #block[
    See #link(<sec:development>)[3.2.1];.
  ]

/ Bibliography\:: #block[
    See #link(<sec:development>)[3.2.1];.
  ]

=== Monograph PhD Thesis
<sec:monograph>
Traditionally, it has been common to structure a PhD thesis as a single
book – a #emph[monograph];. If the thesis is in the form of one single
coherent research project, it can be structured along the lines of
#link(<sec:resesarch>)[3.2.2];. However, for such a big work that a PhD
thesis constitutes, the tasks undertaken are often more diverse, and
thus more naturally split into several smaller research projects as
follows:

/ Introduction\:: #block[
    The introduction would serve the same purpose as for a smaller research
    project described in #link(<sec:development>)[3.2.1];, but would
    normally be somewhat more extensive. The #emph[agenda] part should
    inform the reader about the structure of the rest of the document, since
    this may vary significantly between theses.
  ]

/ Background\:: #block[
    Where as background chapters are not necessarily needed in smaller
    works, they are almost always need in PhD thesis. They may even be split
    into several chapters if there are significantly different topics to
    cover. See #link(<sec:resesarch>)[3.2.2];.
  ]

/ Main chapters\:: #block[
    Each main chapter can be structured more or less like a scientific
    paper. Depending on how much is contained in the introduction and
    background sections, the individual introduction and background sections
    can be significantly reduced or even omitted completely.

    - (Introduction)

    - (Background)

    - Method

    - Results

    - Discussion

    - Conclusion
  ]

/ Discussion\:: #block[
    In addition to the discussions within each of the individual chapters,
    the contribution of the thesis #emph[as a whole] should be thoroughly
    discussed here.
  ]

/ Conclusion\:: #block[
    In addition to the conclusions of each of the individual chapters, the
    overall conclusion of the thesis, and how the different parts contribute
    to it, should be presented here. The conclusion should answer to the
    research questions set out in the main introduction. See also
    #link(<sec:development>)[3.2.1];.
  ]

/ Bibliography\:: #block[
    See #link(<sec:development>)[3.2.1];.
  ]

=== Compiled PhD Thesis
<sec:compiledphd>
Instead of writing up the PhD thesis as a monograph, compiled PhD theses
(also known as stapler theses, sandwich theses, integrated theses, PhD
by published work) consisting of reproductions of already published
research papers are becoming increasingly common. At least some of the
papers should already have been accepted for publication at the time of
submission of the thesis, and thus have been through a real quality
control by peer review.

/ Introduction\:: #block[
    See #link(<sec:monograph>)[3.2.3];.
  ]

/ Background\:: #block[
    See #link(<sec:monograph>)[3.2.3];.
  ]

/ Main contributions\:: #block[
    This chapter should sum up #emph[and integrate] the contribution of the
    thesis as a whole. It should not merely be a listing of the abstracts of
    the individual papers – they are already available in the attached
    papers, and, as such, not needed here.
  ]

/ Discussion\:: #block[
    See #link(<sec:monograph>)[3.2.3];.
  ]

/ Conclusion\:: #block[
    See #link(<sec:monograph>)[3.2.3];.
  ]

/ Bibliography\:: #block[
    See #link(<sec:development>)[3.2.1];.
  ]

/ Paper I\:: #block[
    First included paper with main contributions. It can be included
    verbatim as a PDF. The publishers PDF should be used if the copyright
    permits it. This should be checked with the SHERPA/RoMEO
    database#footnote[#link("http://sherpa.ac.uk/romeo/index.php");] or with
    the publisher. Even when it is no general permission by the publisher,
    you may write and ask for one.
  ]

/ Paper II\:: #block[
    etc.
  ]

== Back Matter
<back-matter>
Material that does not fit elsewhere, but that you would still like to
share with the readers, can be put in appendices. See
#link(<app:additional>)[5];.

= Conclusion
<conclusion>
You definitely should use the `nifty-ntnu-thesis` typst template for your
thesis.

#show: appendix.with(chapters-on-odd: chapters-on-odd)
= Additional Material
<app:additional>
Additional material that does not fit in the main thesis but may still
be relevant to share, e.g., raw data from experiments and surveys, code
listings, additional plots, pre-project reports, project agreements,
contracts, logs etc., can be put in appendices. Simply issue the command
```#show: appendix``` in the main `.typ` file, and make one chapter per appendix. */
