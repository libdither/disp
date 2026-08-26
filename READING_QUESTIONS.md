
We have a bunch of textfiles.

Need a simple evaluator - tree calculus + simple effects

Simple encoding of effects
 - Only: `read_file : FilePath -> String`

Need a function, does parsing and elaboration:
 - String -> IOEffect
   - String -> AST
   - AST -> 



 - A type system I'm told should have two properties, "soundness" (if you put in two syntactically equal things, you get equal outputs), completeness (if they have the same normal form, they are syntactically equal), and round-trip (if you eval a normal-form, it should get back itself). And using these, you can decide equality.