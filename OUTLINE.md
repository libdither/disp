What is Disp?
It's a new iteration in programming languages. It tries to :
1. Trying to allow you to work at higher levels of abstraction
2. Through fast compilation from those higher levels, iterate faster to get physics to do what you want.

What does is mean to work at higher levels of abstraction?
 - The history of programming languages is primarily the history of trying to do less and less of physics-wrangling yourself and more of problem-describing.
	 - Manual physical magnetic memory configuration -> Plugboards -> punch cards/hex coding -> assembly -> C -> Python -> Rust -> Lean/Agentic engineering -> (Disp)
	 - Simplification, lots of things done in parallel, but the pattern is:
		 - *A structure that previously needed to be manually configured, its configuration can now be derived from a more general structure*.
		 - Plugboards -> Easily configure a circuit from a variety of components with plugs
		 - Punchcards -> Easily configure a variety of circuits to run in loops with a set of sequential instructions
		 - Assembly -> Easily configure a sequence of instructions from re-usable instruction sequence parts that get jumped between.
		 - C/system languages -> Easily create re-usable instruction sequence parts out of a small set of primitive operations and making sure that parts produce states of memory that other parts expect.
		 - Python/GC languages -> Easily write code that is unlikely to cause memory errors which calls into a programming interface written in a lower-level language.
			 - Unfortunately this has some overhead and is slower than dedicated writing. 
		 - Rust -> Write safe code that's also fast because it respects the way memory is organized.
			 - Unfortunately this requires a slow compiler.
		 - Lean/Agentic engineering -> Write specifications that your program's inputs and outputs must satisfy, then have an AI agent iterate until it can write a program that is proven to match the specification.
			 - Unfortunately the agent loop is a rather slow process and lean is not as ideally fast as it could be (or requires a lot of slow agent iteration and money to get fast implementation)
 - What does Disp do?
	 - Disp -> Write specifications about your program's inputs and outputs
		 - It produces a program that satisfies a specification as quickly as possible (and in a way that gets faster the more it self-improves) that starts running immediately and optimizing itself as it runs. You view the result and then choose a direction to constrain or loosen the specification -> rinse and repeat.

How does Disp want to attempt to achieve this?
 1. Better type theories - Create a way to turn type theories into general functions that grade/check programs and represent all type theories this way in a modular and easy-to-understand fashion
	 - Most of the work on disp has been along this line. i.e. switch to tree calculus to make defining type theories more elegant, implementing abstract reduction, etc. Latest idea:
	 - Lenses/Claims/Witnesses/Challenges (Made lots of progress on this just this past week, but still have trouble explaining)
    	- A claim takes a "witness" and a "challenge" where challenge can be any tree and it can return Proved/Open/Rejected for a given challenge if it passes or fails the witness.
    	- We have like 5 basic claim-constructors (also called "lenses") that are themselves claims that we assume pass for all challenges and from this we can construct all sorts of claims.
        - A lens carries witnesses forward and challenges back. This allows you to translate a claim like "for all inputs to this function, some property holds" to "for this symbolic input to this function, this property holds" which is actually checkable in *finite time*.
        - A type is just a claim plus its own challenges and sample members. You can in theory build arbitrary type theories on top of this - Still working on this.
 2. General-purpose optimizer - A way to take any problem defined in the form of a boolean or number-valued function on programs and find a program that satisfies or maximally satisfies that function.
	 - This is under-researched, the general idea here is to think about the specification of general-purpose optimizers themselves.
	 - Specifications are programs that take a program and returns true or false / a natural number.
	 - Optimizers are programs that take a specification and return a program that tries to maximally satisfy the specification.
	 - A specification of optimizers is a program that takes an optimizer and grades it via its performance on a variety of tasks. The theoretical limit of this is an optimizer that does well on all tasks, this is probably complexity-theoretically impossible so we probably need to restrict to some space of physically relevant tasks.
	 - The optimizer itself could in theory be used to discover such tasks, i.e. write new specifications of a given type for it to try and satisfy. (via a self-play loop of some sort)
    	 - This is essentially recursive self improvement, but since these can be formal specifications and everything is in tree calculus, it would ideally be more interpretable / you could always make a new optimizer that is subject to additional more stringent comprehensibility constraints than the previous one (what these would be exactly tho, I only have rough ideas)
 3. Better theories / models of optimal execution
    - To get from a high-level specification to an actual configuration of silicon you need to have a model of the silicon to effectively search over the space.
    - There are lots of different types of chips though.
    - Solution: Think about what the most general possible chip might be such that every other chip is a restriction / special case of the general chip
      - This leads directly to the idea of self-reconfiguring chips, where regular chips are just reconfiguring chips but with certain constraints on how much it can reconfigure. (i.e. set circuits in a set construction, only reconfiguring is different toggled connections of the chips)
      - To do this I am investigating cellular automata-based reduction ideas, to gather intuition on what a universal self-reconfiguring chip might look like.
      - Currently still places for further generalization. (actively researching reversible / linear substrates)
 4. Make transition / inter-compatibility with other languages trivial
    - Since we can do any type theory (see above) we should be able to use the semantics of any language, thus compiling that language should just be a matter of creating a parser/pretty-printer pair that translates too and from a dialect of disp and that language.
    - Then semantics can be added via new typings / adding constraints (possibly)