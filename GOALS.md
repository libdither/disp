# Goals

I want to be able to synthesize the best possible programs given a specification.
> I want to be able to write a specification in a type system. Turn type checker into a function that takes a program and returns 1 or 0, combine that with functions that measure performance or program size, and somehow turn all that into a function represented purely in some low-level deterministic turing-complete calculus that has support for representing raw assembly (CPU and GPU instructions). Then I want to run an optimizer that takes this function and returns a new function (in the same calculus) that satisfies the original function as much as possible, ideally producing a program that is both formally correct, and efficient among the desired axes.

To do this we basically need a universal self-improving optimizer, but to bootstrap this we first need:
 - A simple calculus with:
   - Programs are data (to enable defining type checkers in the language itself)
   - Must have a primitive that enables outsourcing execution to an external faster language, using an encoding of that language in the base calculus, where it takes as input calculus-encoded data and outputs calculus-encoded data. The results of primitive language execution (including time spent, memory usage, etc.) should be returned along with the host-calculus encoded outputs.
 - A type system written in the host calculus that:
   - Is a dependently typed system or strictly more powerful (i.e. HoTT).
   - Provides either basic axiomatic assertions of equivalence between a sufficient family of structures in the external language and the host calculus, or an deterministic model of the external language in the host calculus that can be fully reasoned about.
 - A way to write scoring functions that take trees and returns a tree-encoded score, to be combined with the type system function modified to return a 0 or 1 based on type checking success, i.e. a way to write functions that fully 'score' a program on how good it is (whether it follows the specification and is well-written/optimized).
 - An external optimizer that can run the host calculus evaluator and take a scoring function and generate a term in the host calculus that satisfies it as best as it can when run.
   - Must handle combinatorial search over programs
   - Must be able to self-play, continually improve by generating score functions itself to then satisfy.

Once I have this baseline, and the external optimizer works well enough for a wide variety of programs. It should be able to bootstrap itself and be used to redesign the whole process from the ground up. The following should happen gradually to different parts.
 - We should be able to figure out the best host calculus base formulation and evolve it alongside the relevant type systems to improve type checking and execution speed. Type checking functions should be optimizable for improved speed of iteration for satisfying a given type.
 - We should be able to write a meta-utility function for the optimizer and then have the optimizer produce an ideal version of itself.
 - Given the speed of the new base hardware and power of the new bootstrapped optimizer, we should be able to make efficient deterministic models of the base hardware and encode them into the type system to improve performance even more by generating programs that perfectly exploit all available resources on the hardware as best as possible.