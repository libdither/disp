Bigger questions
 - Can CheckerError in most cases  be subsumed by returning false (does not check as given type), or if the CheckerError is due to the type being defined wrong, shift that to the responsibility of the elaborator to make sure all type annotations pass typechecking with `Type` (which in the future should provably mean no errors arise from poorly-specified types?)
 - Can we integrate coproducts into the doc and clean things up? i.e. for Action enum, CheckerResult/CheckerError defs, anything else that is using kinda arbitrary tagging for variants.

Smaller writing questions:
 - There is a general AI-writing smell. I need to make a list of these.
 - "Kleisli-lifted" and mention of "Kleisli" everywhere is kinda obscure terminology. Maybe "monadic"? There's gotta be a more intuitive way to do this.
 - "sig" is kinda bad shorthand, maybe just "tree" or "specific tree" or "form"?