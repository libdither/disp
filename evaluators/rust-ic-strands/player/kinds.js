// Every kind of agent, as the player numbers them (lattice.rs `code`): names, ports, what each rule
// does, colours. Shared by index.html and rev.html.
const TAGS = [null, "L", "S", "F", "P", "Pair", "A", "T1", "Sel", "Unp", "Dn", "Eps", "Nrm", "Out", "Sup", "K", "Sx", "B", "C", "I", "W",
  // Keeping history (set=resid=1, tags 21 on, rules.rs `RESID_TAGS`): walkers forward, walkers back, residuals,
  // running and running back a term given as a tree, forgetting.
  "A+", "T1+", "Sel+", "SL+", "SS+", "SF+", "T1s+", "T1sf+", "T1sc0+", "T1sc+", "T1scf+", "TSb+",
  "A-", "Ar-", "Sel-", "SL-", "SS-", "SF-", "Dn-", "DnL-", "DnS-", "DnF-", "Arms3", "Rk", "Rs", "Rt", "Rb", "Rc",
  "H", "E", "E-", "F!", "?", "Go", "Back", "Rh", "Rq", "Arms-",
  // Two-way programs (two:, tags 59 on, rules.rs `TWO_TAGS`): the opcodes a program is a tree of, then the walkers.
  "swap", "assocl", "assocr", "s2f", "f2s", "uniti", "unite", "copy", "merge", "eval", "flip", "seq", "par", "cond", "loop",
  "R", "Sp", "SpS", "Eat", "Cd", "Lk", "Lk2", "Fl", "Mrg", "MrgL", "MrgS", "MrgF"];
const ARITY = [0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 1, 2, 1, 3, 2, 3, 3, 3, 1, 2,
  4, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 3, 4, 4, 3, 2, 3, 4, 4, 2, 4, 4, 3, 3, 4, 3, 3, 2, 1, 1, 1, 3, 2, 2,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 2, 4, 3, 2, 1, 4, 4, 4, 2, 3, 2, 3, 4];
/// Two-way programs (two:): the opcodes, programs as data (59 to 73), and the walkers that run them (74 to 85).
const isOpcode = t => t >= 59 && t <= 73, isTwoWalker = t => t >= 74 && t <= 85;
/// Keeping history (set=resid=1): the walkers that work the answer out leaving a residual (21 to 32, E 50 and
/// the click Go 54), the walkers that run it back (33 to 42, E- 51, the click Back 55 and Arms- 58), the arms
/// a triage carries (43), the residuals and histories (44 to 49, 56, 57), the forgetter (52) and the hole (53).
const isForward = t => t >= 21 && t <= 32 || t === 50 || t === 54, isBackward = t => t >= 33 && t <= 42 || t === 51 || t === 55 || t === 58;
const isResidual = t => t >= 44 && t <= 49 || t === 56 || t === 57;
/// What each residual stands for, as a tree.
const RESIDUAL_TREE = { Rk: "△△c", Rs: "△(△j₁)(△j₂j₃)", Rt: "△(△hd)r", Rb: "△(△j₁j₂)", Rc: "△(△(△j₁j₂))", H: "△j(△h_f h_x)",
  Rh: "△(△(△(△fx)))", Rq: "△(△(△(△a)))" };
/// Movement agents (set=views=1, tags 15 to 20): loaded code standing for a tree shape, the tree each spells out as.
const VIEW_TREE = { K: "△△y", Sx: "△(△s)b", B: "△(△(△△f))g", C: "△(△f)(△△g)", I: "△(△△△)△", W: "△(△s)s" };
const isView = t => t >= 15 && t <= 20;
const NAMES = { L: "leaf", S: "stem", F: "fork", P: "suspended apply", Pair: "pair", A: "apply", T1: "triage", Sel: "second dispatch",
  Unp: "unpair", Dn: "duplicator", Eps: "eraser", Nrm: "normalizer", Out: "root", Sup: "superposition",
  K: "K: keeps, erases its argument", Sx: "Sx: copies its argument to both sides", B: "B: moves its argument right",
  C: "C: moves its argument left", I: "I: becomes a wire", W: "W: works out once, copies the result",
  "A+": "apply, keeping history", "T1+": "triage, keeping history", "Sel+": "dispatch, keeping history",
  "SL+": "dispatch on a leaf, reading the arms", "SS+": "dispatch on a stem, reading the arms", "SF+": "dispatch on a fork, reading the arms",
  "T1s+": "S rule looking for B", "T1sf+": "S rule looking for B, a level down", "T1sc0+": "S rule opening its arms", "T1sc+": "S rule looking for C",
  "T1scf+": "S rule looking for C, a level down", "TSb+": "S rule, copying its argument",
  "A-": "un-apply", "Ar-": "un-apply of a constructor, reading the result", "Sel-": "dispatch run back",
  "SL-": "dispatch on a leaf run back", "SS-": "dispatch on a stem run back", "SF-": "dispatch on a fork run back",
  "Dn-": "merge (a copy run back)", "DnL-": "merge, having read a leaf", "DnS-": "merge, having read a stem", "DnF-": "merge, having read a fork",
  Arms3: "a triage's three arms", Rk: "residual of K", Rs: "residual of the S rule", Rt: "residual of a triage", Rb: "residual of B", Rc: "residual of C",
  H: "history of an application", E: "run: works a term given as a tree out", "E-": "run back: works the history back to the term",
  "F!": "forgetter", "?": "hole: something forgotten", Go: "the root, clicked: run", Back: "the root, clicked: run back",
  Rh: "residual of reading a hole", Rq: "residual of a dispatch on a hole", "Arms-": "arms back into a triage",
  swap: "swap: (u, v) ↔ (v, u)", assocl: "regroup: (u, (v, w)) → ((u, v), w)", assocr: "regroup: ((u, v), w) → (u, (v, w))",
  s2f: "stem to fork: △t → (△, t)", f2s: "fork to stem: (△, t) → △t", uniti: "add a leaf: x → (△, x)", unite: "drop a leaf: (△, x) → x",
  copy: "copy: x → (x, x)", merge: "merge: (x, x) → x", eval: "eval: (q, z) → (q, q z)", flip: "flip a choice: (△, v) ↔ (△△, v)",
  seq: "a; b: a, then b", par: "(a | b): (u, v) → (a u, b v)", cond: "cond(a, b): a on a leaf's side, b on the other",
  loop: "loop(c): c again while it gives the left side", R: "run: a program on a tree", Sp: "splitter: a fork taken apart",
  SpS: "splitter: a stem taken apart", Eat: "eats a leaf", Cd: "cond, reading the tag", Lk: "loop, reading the state", Lk2: "loop, reading its tag",
  Fl: "flip, reading the tag", Mrg: "merge, reading the first half", MrgL: "merge, having read a leaf", MrgS: "merge, having read a stem",
  MrgF: "merge, having read a fork" };
const DESC = {
  L: "a leaf △, the simplest value", S: "a stem △x: a value with one child", F: "a fork △xy: a value with two children",
  P: "a suspended application f a: it becomes a value once something needs it", Pair: "a pair carrying the arms of a pending choice",
  A: "applies the operator it reads to an argument (leaf → stem, stem → fork, fork → triage)",
  T1: "looks at the shape of a value and picks one of its arms", Sel: "the second half of triage: picks among three arms by the shape of a value",
  Unp: "splits a pair into its two parts", Dn: "copies the value it reads for two readers, one layer at a time; where its 2×2×2 block has room, before anything asks for the copies",
  Eps: "erases whatever it reads", Nrm: "drives its input all the way to a finished value", Out: "the root: the answer arrives here",
  Sup: "a superposition &ℓ{a,b}: a in one universe and b in the other; whatever reads it splits in two",
  ...Object.fromEntries(Object.entries({ K: "applied to c it gives y and erases c", Sx: "applied to c it gives s c (b c), c copied",
    B: "applied to c it gives f (g c): c moves right, uncopied", C: "applied to c it gives f c g: c moves left, uncopied",
    I: "applied to c it gives c, a plain wire", W: "applied to c it gives t t, t = s c worked out once and copied" })
    .map(([v, d]) => [v, `a movement agent (set=views=1): loaded code standing for ${VIEW_TREE[v]}; ${d}, in one rewrite. A duplicator or eraser takes it whole; anything else reading it spells it out as its tree first`])),
  "A+": "applies the function it reads to an argument, and puts what it would have dropped (its residual) out on a fourth port",
  "T1+": "triage keeping history: what a K step drops goes into its residual instead of an eraser", "Sel+": "dispatch keeping history: the two arms not taken go into its residual",
  "A-": "un-apply: reads a residual as the function and the result as the argument, and gives back the argument and the function the apply had; an un-apply meeting an apply that never ran just joins them up",
  "Ar-": "the un-apply of a stem or fork built by applying a leaf or stem: takes the result apart", "Sel-": "picks how a triage is run back by the argument's head kept in its residual",
  "Dn-": "a merge: the two copies a duplicator made, read back into one (when they meet the duplicator itself, both just go)",
  Arms3: "a triage's three arms w, x, b, carried to the dispatch",
  E: "runs a term given as a tree (△v a value, △⌜f⌝⌜x⌝ an application): each application becomes an apply, and its history (H) is kept beside the answer",
  "E-": "runs the answer and its history back to the term as a tree: for each application an un-apply of its residual and result",
  "F!": "forgetting is on: erases what an apply dropped (K's argument, a triage's arms not taken) and leaves a hole in its place",
  "?": "a hole: something forgotten. A merge of a hole and a copy gives the copy; run again, what reads a hole gives a hole",
  Go: "the root, clicked: drops E on the term here, to run it", Back: "the root, clicked: drops E- on the answer and history here, to run them back",
  H: "the history of an application of the loaded term: its residual, and the histories of its function and argument",
  "Arms-": "running back a dispatch on a hole: the arms it kept, as the triage they were",
  ...Object.fromEntries(Object.entries(RESIDUAL_TREE).map(([r, t]) => [r, `a residual: what a step would have dropped, standing for the tree ${t}; read back as the function by an un-apply`])),
  ...Object.fromEntries(["swap", "assocl", "assocr", "s2f", "f2s", "uniti", "unite", "copy", "merge", "eval", "flip"].map(o =>
    [o, `an opcode of a two-way program, a bijection on trees: ${NAMES[o]}. Data until a run (R) meets it`])),
  seq: "a two-way program: a, then b on what a gives. A run meeting it becomes two runs in a row, rewired", par: "a two-way program: a on the left half and b on the right, at once",
  cond: "a two-way program: on (t, y), a on y if t is a leaf, else b; t is kept, so the choice can be undone", loop: "a two-way program: the input goes in as (△△, x); the body c runs while it gives a left side (△, s), and its right side (△△, y) gives y",
  R: "runs the program on its first port on the tree on its second, and hands the program back on its third beside the result on its fourth. Meeting an opcode it rewires the tree around splitters and new runs, or leaves a walker to read data (cond, loop, flip, merge)",
  Sp: "takes a fork apart: each child goes straight to its reader", SpS: "takes a stem apart: its child goes straight to its reader",
  Eat: "eats a leaf, and is stuck on anything else", Cd: "reads cond's tag: a leaf runs a on y, anything else b; the tag and the program come back",
  Lk: "takes the loop's state apart to read its tag", Lk2: "reads the loop's tag: a left side runs the body again, a right side ends the loop",
  Fl: "flips a choice's tag: △ becomes △△ and △△ becomes △", Mrg: "merges two halves into one tree, one layer at a time: stuck where they differ",
  MrgL: "a merge whose first half was a leaf: the second must be one too", MrgS: "a merge whose first half was a stem: the second must be one too",
  MrgF: "a merge whose first half was a fork: the second must be one too" };
const PORTS = { L: ["value"], S: ["value", "child"], F: ["value", "left", "right"], P: ["value", "function", "argument"], Pair: ["value", "first", "second"],
  A: ["operator", "argument", "result"], T1: ["subject", "arms", "result"], Sel: ["subject", "arms", "result"], Unp: ["pair", "first", "second"],
  Dn: ["value", "copy 1", "copy 2"], Eps: ["input"], Nrm: ["input", "result"], Out: ["answer"], Sup: ["value", "first side", "second side"],
  K: ["value", "y"], Sx: ["value", "s", "b"], B: ["value", "f", "g"], C: ["value", "f", "g"], I: ["value"], W: ["value", "s"],
  "A+": ["function", "argument", "result", "residual"], "T1+": ["subject", "⟨b, c⟩", "result", "residual"], "Sel+": ["subject", "arms", "result", "residual"],
  "SL+": ["arms", "result", "residual"], "SS+": ["arms", "u", "result", "residual"], "SF+": ["arms", "⟨u, v⟩", "result", "residual"],
  "T1s+": ["s", "⟨b, c⟩", "result", "residual"], "T1sf+": ["s's left", "⟨s's right, ⟨b, c⟩⟩", "result", "residual"], "T1sc0+": ["⟨b, c⟩", "s", "result", "residual"],
  "T1sc+": ["b", "⟨s, c⟩", "result", "residual"], "T1scf+": ["b's left", "⟨b's right, ⟨s, c⟩⟩", "result", "residual"], "TSb+": ["⟨s, c⟩", "b", "result", "residual"],
  "A-": ["residual", "result", "argument", "function"], "Ar-": ["result", "argument", "function"], "Sel-": ["argument's head", "arms", "argument", "function"],
  "SL-": ["arms", "argument", "function"], "SS-": ["arms", "j", "argument", "function"], "SF-": ["arms", "⟨j₁, j₂⟩", "argument", "function"],
  "Dn-": ["copy", "copy", "value"], "DnL-": ["other copy", "value"], "DnS-": ["other copy", "child", "value"], "DnF-": ["other copy", "left", "right", "value"],
  Arms3: ["value", "w", "x", "b"], Rk: ["value", "c"], Rs: ["value", "j₁", "j₂", "j₃"], Rt: ["value", "h", "d", "r"], Rb: ["value", "j₁", "j₂"], Rc: ["value", "j₁", "j₂"],
  H: ["value", "residual", "function's history", "argument's history"], E: ["term", "value", "history"], "E-": ["history", "value", "term"],
  "F!": ["residual", "up"], "?": ["value"], Go: ["term"], Back: ["answer and history"], Rh: ["value", "function", "argument"], Rq: ["value", "arms"],
  "Arms-": ["arms", "triage"],
  ...Object.fromEntries(["swap", "assocl", "assocr", "s2f", "f2s", "uniti", "unite", "copy", "merge", "eval", "flip"].map(o => [o, ["program"]])),
  seq: ["program", "a", "b"], par: ["program", "a (left)", "b (right)"], cond: ["program", "a (on a leaf)", "b (otherwise)"], loop: ["program", "body"],
  R: ["program", "input", "program out", "result"], Sp: ["fork", "left", "right"], SpS: ["stem", "child"], Eat: ["leaf"],
  Cd: ["tag", "⟨a, b⟩", "y", "⟨program, result⟩"], Lk: ["state", "program", "program out", "result"], Lk2: ["tag", "⟨program, rest⟩", "program out", "result"],
  Fl: ["tag", "tag out"], Mrg: ["first", "second", "merged"], MrgL: ["second", "merged"], MrgS: ["second", "first's child", "merged"],
  MrgF: ["second", "first's left", "first's right", "merged"] };
const MEANING = {
  "A·L": "apply(leaf, x) = stem x", "A·S": "apply(stem a, x) = fork a x", "A·F": "apply(fork a b, x) = triage a with arms ⟨b, x⟩",
  "A·P": "the operator is still suspended: force it first, then apply", "T1·L": "the subject a is a leaf: the result is b, and c is erased",
  "T1·S": "the subject a is a stem s: the result is (s c)(b c), sharing c through a duplicator", "T1·F": "the subject a is a fork w x: dispatch on c with arms ⟨w, ⟨x, b⟩⟩",
  "T1·P": "the subject a is still suspended: force it, then triage again", "Sel·L": "the subject z is a leaf: the result is w; the other arms are erased",
  "Sel·S": "the subject z is a stem u: the result is x u; w and b are erased", "Sel·F": "the subject z is a fork u v: the result is (b u) v; w and x are erased",
  "Sel·P": "the subject z is still suspended: force it, then dispatch again", "Unp·Pair": "the pair comes apart: each part is wired straight to its reader",
  "Dn·L": "two copies of a leaf", "Dn·S": "copy the stem; a new duplicator copies its child", "Dn·F": "copy the fork; two new duplicators copy its children",
  "Dn·P": "force it once, then copy the value", "Eps·L": "the leaf is erased", "Eps·S": "the stem is erased; an eraser moves on to its child",
  "Eps·F": "the fork is erased; erasers move on to both children", "Eps·P": "the suspension is erased; erasers move on to its function and argument",
  "Eps·Pair": "the pair is erased; erasers move on to both parts", "Nrm·L": "a leaf is already finished", "Nrm·S": "keep the stem; normalize its child",
  "Nrm·F": "keep the fork; normalize both children", "Nrm·P": "force it, then keep normalizing",
  "A·Sup": "apply each side, to its own copy of the argument: the result is a superposition of the two",
  "T1·Sup": "triage each side, with its own copy of the arms", "Sel·Sup": "dispatch on each side, with its own copy of the arms",
  "Nrm·Sup": "keep the superposition; normalize both sides", "Eps·Sup": "the superposition is erased; erasers move on to both sides",
  "Dn·Sup": "a copy for this superposition hands each copy its own side; any other copies both sides and superposes the copies",
  "Dn·Pair": "copy the pair; two new duplicators copy its parts",
  "A·K": "K y c = y: c is erased", "A·Sx": "Sx s b c = s c (b c): c is copied", "A·B": "B f g c = f (g c): c moves right, uncopied",
  "A·C": "C f g c = f c g: c moves left, uncopied", "A·I": "I c = c: argument and result are wired together", "A·W": "W s c = t t: t = s c is worked out once, then copied",
  ...Object.fromEntries(Object.keys(VIEW_TREE).flatMap(v => [[`Dn·${v}`, `two copies of the ${v} view in one rewrite; duplicators copy its children`],
    [`Eps·${v}`, `the ${v} view is erased in one rewrite; erasers move on to its children`],
    ...["T1", "Sel", "Nrm"].map(q => [`${q}·${v}`, `the ${v} view is spelled out as its tree ${VIEW_TREE[v]}, for the ${NAMES[q]} to read`])])),
  "A+·L": "apply(leaf, x) = stem x; residual △", "A+·S": "apply(stem a, x) = fork a x; residual △", "A+·F": "apply(fork a b, x) = triage a with ⟨b, x⟩, keeping history",
  "T1+·L": "K: the result is b; c goes into the residual Rk(c) instead of an eraser", "T1+·S": "the S rule: first look whether it is B or C",
  "T1+·F": "dispatch on c with arms ⟨w, x, b⟩", "Sel+·L": "c is a leaf: read the arms", "Sel+·S": "c is a stem: read the arms", "Sel+·F": "c is a fork: read the arms",
  "SL+·Arms3": "the result is w; residual Rt(△, x, b)", "SS+·Arms3": "the result is x u; residual Rt(△j, w, b)", "SF+·Arms3": "the result is b u v; residual Rt(△j₁j₂, w, x)",
  "T1sf+·L": "B f g c = f (g c): c goes to the side that uses it, uncopied; residual Rb", "T1scf+·L": "C f g c = f c g: c goes to the side that uses it, uncopied; residual Rc",
  "TSb+·Pair": "the S rule: s c (b c), c copied; residual Rs(j₁, j₂, j₃)",
  "A-·L": "the step was a constructor: take the result apart", "Ar-·S": "the result was a stem △z: the function was △, the argument z", "Ar-·F": "the result was a fork △xz: the function was △x, the argument z",
  "A-·Rk": "the step was K: the function was △△(result), the argument c", "A-·Rs": "the step was the S rule: un-apply its three applies, merge the two copies of c",
  "A-·Rt": "the step was a triage: dispatch back on the argument's head", "A-·Rb": "the step was B: un-apply both applies", "A-·Rc": "the step was C: un-apply both applies",
  "Dn-·L": "merge: the first copy is a leaf", "Dn-·S": "merge: the first copy is a stem", "Dn-·F": "merge: the first copy is a fork",
  "DnL-·L": "merge: both copies leaves, one leaf", "DnS-·S": "merge: both copies stems, one stem; merge the children", "DnF-·F": "merge: both copies forks, one fork; merge the children",
  "E·S": "a value of the term: out as it is; its history △", "E·F": "an application of the term: an apply of its two parts run; its history H",
  "E-·L": "a value: back into the term as △v", "E-·H": "an application: un-apply its residual and result, and run the function's and argument's histories back",
  "Go·S": "run the term here; the answer and history, bundled, at a new root", "Go·F": "run the term here; the answer and history, bundled, at a new root",
  "Back·F": "run the answer and history bundled here back to the term, at a new root",
  "F!·Rk": "forget K's dropped argument: a hole in its place", "F!·Rt": "forget a triage's arms not taken: holes in their place",
  "Dn·?": "two copies of a hole", "Nrm·?": "a hole is as finished as it gets", "Dn-·?": "merge: a hole and a copy give the copy",
  "DnL-·?": "merge: a leaf and a hole give the leaf", "DnS-·?": "merge: a stem and a hole give the stem", "DnF-·?": "merge: a fork and a hole give the fork",
  "Ar-·?": "the result was forgotten: the function and argument are holes too",
  "A+·?": "applying a hole gives a hole; Rh keeps the argument", "T1+·?": "a triage on a hole gives a hole; Rh keeps the rest",
  "Sel+·?": "a dispatch on a hole gives a hole; Rq keeps the arms", "A-·Rh": "give back the function and argument Rh kept",
  "A-·Rq": "give back the arms Rq kept as the triage, and a hole for the argument", "Arms-·Arms3": "the arms w, x, b as the triage △(△wx)b",
  "T1s+·?": "the S rule: a hole is not B", "T1sf+·?": "the S rule: a hole is not B", "T1sc+·?": "the S rule: a hole is not C", "T1scf+·?": "the S rule: a hole is not C",
  "R·L": "the identity: the input is the result, and the leaf goes back as the program", "R·swap": "split (u, v) and join it again as (v, u)",
  "R·assocl": "split (u, (v, w)) twice and join ((u, v), w)", "R·assocr": "split ((u, v), w) twice and join (u, (v, w))",
  "R·s2f": "split the stem △t and join (△, t)", "R·f2s": "split (△, t), eat the leaf and join △t", "R·uniti": "join (△, x)",
  "R·unite": "split (△, x) and eat the leaf", "R·copy": "a duplicator copies x into (x, x)", "R·merge": "split (x, y) and merge the halves, stuck if they differ",
  "R·eval": "split (q, z) and run q on z: q comes back beside its result", "R·seq": "run a, then b on what a gives: two runs in a row, the programs joined again as they come back",
  "R·par": "split (u, v) and run a on u and b on v at once", "R·cond": "split (t, y): a cond walker reads t", "R·loop": "the input goes in as (△△, x): a loop walker reads what each round gives",
  "R·flip": "split (t, v): a flip walker reads t", "Sp·F": "the fork comes apart: each child is wired to its reader", "SpS·S": "the stem comes apart", "Eat·L": "the leaf is eaten",
  "Cd·L": "the tag is a leaf: run a on y; b comes back untouched", "Cd·S": "the tag is a stem: run b on y; a comes back untouched", "Cd·F": "the tag is a fork: run b on y; a comes back untouched",
  "Lk·F": "the state (t, y) comes apart: read its tag t", "Lk2·L": "a left side (△, y): run the body again on it", "Lk2·S": "a right side (△△, y): the loop is done, and y is its result",
  "Fl·L": "△ becomes △△", "Fl·S": "△△ becomes △", "Mrg·L": "the first half is a leaf: read the second", "Mrg·S": "the first half is a stem: read the second",
  "Mrg·F": "the first half is a fork: read the second", "MrgL·L": "both halves leaves: one leaf", "MrgS·S": "both halves stems: one stem; merge their children",
  "MrgF·F": "both halves forks: one fork; merge their children",
  ...Object.fromEntries(["swap", "assocl", "assocr", "s2f", "f2s", "uniti", "unite", "copy", "merge", "eval", "flip"].map(o => [`Dn·${o}`, `two copies of ${o}`])),
  "Dn·seq": "copy the node; duplicators copy both parts", "Dn·par": "copy the node; duplicators copy both parts", "Dn·cond": "copy the node; duplicators copy both parts",
  "Dn·loop": "copy the loop; a duplicator copies its body" };
// Values (leaf, stem, fork) in greys: the colours are for applications and what carries them out.
// Movement agents in colours of what they do: K red (erases), Sx and W greens (copy), B orange and C yellow (move), I sky (a wire).
// Keeping history: walkers forward as apply and triage are, walkers back in pinks, residuals and histories amber,
// the forgetter red, a hole white, the clicked root green (run) or pink (run back).
const COLOR = ["#000", "#dfe4ea", "#aab3bf", "#7c8694", "#e86bd8", "#c792ea", "#56c8ff", "#4fd6be", "#2bb5a0", "#7a8cff", "#9d7cff", "#8a96a8", "#6ee07a", "#ffffff", "#f7768e",
  "#db4b4b", "#c3e88d", "#ff9e64", "#f4d35e", "#7dcfff", "#9ece6a",
  "#56c8ff", "#4fd6be", "#2bb5a0", "#2bb5a0", "#2bb5a0", "#2bb5a0", "#4fd6be", "#4fd6be", "#4fd6be", "#4fd6be", "#4fd6be", "#4fd6be",
  "#ff6ac1", "#ff8fd0", "#e05fb0", "#e05fb0", "#e05fb0", "#e05fb0", "#e07cff", "#e07cff", "#e07cff", "#e07cff", "#c792ea",
  "#ffc777", "#ffc777", "#ffc777", "#ffc777", "#ffc777",
  "#ffc777", "#56c8ff", "#ff6ac1", "#db4b4b", "#ffffff", "#6ee07a", "#ff6ac1", "#ffc777", "#ffc777", "#e05fb0",
  // Two-way programs: opcodes, programs as data, in pale violets (the nodes that hold parts deeper); the run sky blue as
  // an apply is, splitters teal, eating a leaf an eraser's grey, the walkers that read data warm, merges violet.
  ...Array(11).fill("#c4b5f0"), "#a898e0", "#a898e0", "#a898e0", "#a898e0",
  "#56c8ff", "#4fd6be", "#4fd6be", "#8a96a8", "#f4d35e", "#ff9e64", "#ff9e64", "#ff6ac1", "#e07cff", "#e07cff", "#e07cff", "#e07cff"];
const kindColor = t => COLOR[t] ?? "#8a96a8";
/// A kind as its icon (shapes.js) and its name.
const kindHTML = t => `${shapeSVG(t, kindColor(t))}${NAMES[TAGS[t]] ?? `agent of kind ${t}`}`;
const isConsumer = t => t >= 6 && t <= 12 || t >= 21 && t <= 42 || [50, 51, 52, 54, 55, 58].includes(t) || isTwoWalker(t);
const isProducer = t => t >= 1 && t <= 5 || t === 14 || isView(t) || t >= 43 && t <= 49 || t === 53 || t === 56 || t === 57 || isOpcode(t);
// Which ports are outputs (sources): a producer's principal, a computation's result (and residual), a
// duplicator's or unpair's two parts, a merge's value, what a run gives or runs back to.
const OUTS = { 24: [1, 2], 34: [1, 2], 36: [1, 2], 39: [2], 40: [1], 41: [2], 42: [3], 50: [1, 2], 51: [2], 52: [1], 54: [], 55: [], 58: [1],
  74: [2, 3], 75: [1, 2], 76: [1], 77: [], 78: [3], 79: [2, 3], 80: [2, 3], 81: [1], 82: [2], 83: [1], 84: [2], 85: [3] };
const isSource = (t, p) => isProducer(t) ? p === 0 : (t >= 6 && t <= 8) ? p === 2 : t === 12 ? p === 1 : (t === 9 || t === 10) ? p > 0
  : OUTS[t] ? OUTS[t].includes(p) : t >= 21 && t <= 42 ? p >= 2 : false;
