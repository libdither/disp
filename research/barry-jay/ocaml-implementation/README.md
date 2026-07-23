This is an OCaml implementation of the type inference developed for the paper "Simple Types for Polymorphic Functions".
Barry Jay's Rocq proofs can be found [here](https://github.com/barry-jay-personal/combinatory-types).

## Getting Started

### Prerequisites
* Install [OCaml](https://ocaml.org/docs/installing-ocaml)
* Install [Dune](https://dune.build/install)

### Build and run tests
```
dune build @default @runtest
```
or to continuously build and update test outputs
```
dune build @default @runtest --auto-promote --watch
```

### Format code
```
dune fmt
```
