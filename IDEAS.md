Note: No LLM should write in this file. Only read.

We have a program that produces a stream of trees.
We can take $n$ items from this stream, and resume it.
All sampling ops from this stream needs to be reasonable about in terms of how long it takes to do.

Base stream:
 - `Trees := enumerate_all_trees`

We then have an arbitrary predicate.
`Bool := triage true is_leaf false`

We can enumerate all booleans.
`Bools := Trees.filter(Bool).next(2)`

This should terminate relatively quickly.

We should be able to enumerate all boolean functions.
But how do we define a boolean function filter?

`Arrow := {A, B, v} => Trees.filter(A).map({b} => v b).all(B)`


```rust
fn f(x: String) -> String {
	match x {
		"61C" => "A prereq!",
		"164" => "I'm taking this now!",
		"265" => "I may take this later!",
		_ => format!("Default: {}", x),
	}
}
println!("{}", f("test"))

```