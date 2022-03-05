# Disp

Goal: An all-purpose systems-level provable programming language.

Disp is still just a personal project as this stage. It currently comes with a simple untyped lambda calculus evaluator and some pre-built math functions using church encoding.

To Run:
```shell
nix develop
cargo run

load "functions.disp"
```

## Roadmap

Function that takes an untyped program and infers its type in a two-sort [pure type system](https://en.wikipedia.org/wiki/Pure_type_system) based on the [calculus of inductive constructions](https://en.wikipedia.org/wiki/Calculus_of_inductive_constructions).

Function that takes a typed program and generates a program that replaces inductive types with types that can be easily represented in binary. (i.e. translate Church encoded numbers into unsigned integers).

Function that takes program with supported types in addition to different translation strategies and generates relevant LLVM bytecode. (translation strategies are programs that say: "if I see this function and this type, I should generate this assembly).