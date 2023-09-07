# Disp

Goals
 - All-purpose: Disp will be able to be used for scripting, shells, and compiled languages.
 - Speed: Disp will be as fast if not faster than Rust, C and adjacent languages, even when writing high-level code.
 - Provably Safe: Disp programs compiled to machinecode will carry proofs of correctness written for a software model of the target CPU.
 - Syntax-agnostic: Anyone from any language should be able to come to Disp without having to re-learn syntax preferences. This can be solved through various syntax variants of disp expressions that reflect the syntax of various languages.

Disp is still just a personal project as this stage. It is currently just a simple untyped lambda calculus evaluator and some math functions made using Church Encoding.

To Run:
```shell
nix develop
cargo run

load "std.disp"
```

## Old Roadmap

Function that takes an untyped program and infers its type in a two-sort [pure type system](https://en.wikipedia.org/wiki/Pure_type_system) based on the [calculus of inductive constructions](https://en.wikipedia.org/wiki/Calculus_of_inductive_constructions).

Function that takes a typed program and generates a program that replaces inductive types with types that can be easily represented in binary. (i.e. translate Church encoded numbers into unsigned integers).

Function that takes program with supported types in addition to different translation strategies and generates relevant LLVM bytecode. (translation strategies are programs that say: "if I see this function and this type, I should generate this assembly).

## Implementation Details

 - [Deduplication-friendly Lambda term representation](https://dither.link/docs/disp/bind-trees.html)
