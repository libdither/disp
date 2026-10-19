// Every kind of agent, as the player numbers them (lattice.rs `code`): names, ports, what each rule
// does, colours. Shared by index.html and rev.html.
const TAGS = [null, "L", "S", "F", "P", "Pair", "A", "T1", "Sel", "Unp", "Dn", "Eps", "Nrm", "Out", "Sup", "K", "Sx", "B", "C", "I", "W",
  // Keeping history (tags 21 to 58, rules.rs `RESID_TAGS`), which the player does not run (strands-resid does):
  // named here only to keep the numbering.
  "A+", "T1+", "Sel+", "SL+", "SS+", "SF+", "T1s+", "T1sf+", "T1sc0+", "T1sc+", "T1scf+", "TSb+",
  "A-", "Ar-", "Sel-", "SL-", "SS-", "SF-", "Dn-", "DnL-", "DnS-", "DnF-", "Arms3", "Rk", "Rs", "Rt", "Rb", "Rc",
  "H", "E", "E-", "F!", "?", "Go", "Back", "Rh", "Rq", "Arms-",
  // Two-way programs (two:, tags 59 on, rules.rs `TWO_TAGS`): walkers all, programs being trees. The run, and
  // the run having read 1 to 14 stems of an instruction; splitters, cond, loop, flip and merge; triage; inverting.
  "R", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12", "R13", "R14",
  "Sp", "SpS", "Eat", "Cd", "Lk", "Lk2", "Fl", "Mrg", "MrgL", "MrgS", "MrgF", "Tr", "Ut", "Ut2", "IsF", "Dg0", "Dg1", "Dg2", "Dg3", "DgE", "DgO"];
const ARITY = [0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 1, 2, 1, 3, 2, 3, 3, 3, 1, 2,
  4, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 3, 4, 4, 3, 2, 3, 4, 4, 2, 4, 4, 3, 3, 4, 3, 3, 2, 1, 1, 1, 3, 2, 2,
  ...Array(15).fill(4), 3, 2, 1, 4, 4, 4, 2, 3, 2, 3, 4, 2, 3, 3, 2, ...Array(6).fill(2)];
/// Two-way programs (two:): the walkers that run them (59 to 94), the run and its reading an instruction first (59 to 73).
const isTwoWalker = t => t >= 59 && t <= 94, isRun = t => t >= 59 && t <= 73;
/// A two-way program's instructions, as many stems on a leaf as their place here (rules.rs `TWO_RULES`), and those on a fork.
const INSTR = ["id", "swap", "flip", "assocl", "assocr", "uniti", "unite", "copy", "merge", "eval", "uneval", "triage", "untriage", "s2f", "f2s"];
const INSTR_FORK = ["a; b", "(a | b)", "cond(a, b)", "loop(a)"];
const VIEW_TREE = { K: "△△y", Sx: "△(△s)b", B: "△(△(△△f))g", C: "△(△f)(△△g)", I: "△(△△△)△", W: "△(△s)s" };
const isView = t => t >= 15 && t <= 20;
const NAMES = { L: "leaf", S: "stem", F: "fork", P: "suspended apply", Pair: "pair", A: "apply", T1: "triage", Sel: "second dispatch",
  Unp: "unpair", Dn: "duplicator", Eps: "eraser", Nrm: "normalizer", Out: "root", Sup: "superposition",
  K: "K: keeps, erases its argument", Sx: "Sx: copies its argument to both sides", B: "B: moves its argument right",
  C: "C: moves its argument left", I: "I: becomes a wire", W: "W: works out once, copies the result",
  R: "run: a program on a tree",
  ...Object.fromEntries(INSTR.slice(1).map((_, i) => [`R${i + 1}`, `run, ${i + 1} stem${i ? "s" : ""} of an instruction read`])),
  Sp: "splitter: a fork taken apart", SpS: "splitter: a stem taken apart", Eat: "eats a leaf", Cd: "cond, reading the tag", Lk: "loop, reading the state",
  Lk2: "loop, reading its tag", Fl: "flip, reading the tag", Mrg: "merge, reading the first half", MrgL: "merge, having read a leaf",
  MrgS: "merge, having read a stem", MrgF: "merge, having read a fork", Tr: "triage", Ut: "untriage, reading the outer tag",
  Ut2: "untriage, reading the inner tag", IsF: "fork check", Dg0: "a program's inverse, made as it is read", Dg1: "inverting, 1 stem read",
  Dg2: "inverting, 2 stems read", Dg3: "inverting, 3 stems read", DgE: "inverting, an even number of stems read", DgO: "inverting, an odd number of stems read" };
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
  R: "runs the program on its first port on the tree on its second, reading the program a node at a time, and hands the program back on its third beside the result on its fourth. A leaf is id and a fork (a, b) runs a then b; a stem starts an instruction, read on by R1, R2, …",
  ...Object.fromEntries(INSTR.slice(1).map((_, i) => [`R${i + 1}`, `a run that has read ${i + 1} stem${i ? "s" : ""} of an instruction: a leaf under ${i ? "them" : "it"} is ${INSTR[i + 1]}${INSTR_FORK[i + 1] ? `, a fork ${INSTR_FORK[i + 1]}` : ""}${i < 13 ? ", and another stem reads on" : ""}`])),
  Sp: "takes a fork apart: each child goes straight to its reader", SpS: "takes a stem apart: its child goes straight to its reader",
  Eat: "eats a leaf, and is stuck on anything else", Cd: "reads cond's tag: a leaf runs a on y, anything else b; the tag and the program come back",
  Lk: "takes the loop's state apart to read its tag", Lk2: "reads the loop's tag: a left side runs the body again, a right side ends the loop",
  Fl: "flips a choice's tag: △ becomes △△ and △△ becomes △", Mrg: "merges two halves into one tree, one layer at a time: stuck where they differ",
  MrgL: "a merge whose first half was a leaf: the second must be one too", MrgS: "a merge whose first half was a stem: the second must be one too",
  MrgF: "a merge whose first half was a fork: the second must be one too",
  Tr: "triage: tells a leaf, stem and fork apart as a choice, △ → (△, (△, △)), △t → (△, (△△, t)), △ab → (△△, (a, b)), so a program can choose by the shape of any tree, a program's included",
  Ut: "untriage, triage's inverse, reading the outer tag: a leaf opens the inner choice (Ut2), △△ lets the fork under it through",
  Ut2: "untriage reading the inner tag: (△, △) gives a leaf, (△△, t) the stem △t", IsF: "lets a fork through, and is stuck on anything else",
  Dg0: "makes the inverse of the program it reads, a node at a time, for uneval: a; b becomes b⁻¹; a⁻¹, each instruction its partner (3 with 4, 5 with 6, …), swap and flip themselves",
  ...Object.fromEntries(["Dg1", "Dg2", "Dg3", "DgE", "DgO"].map(d => [d, `an instruction being inverted, ${NAMES[d].slice(11)}: one stem is held back, so the end can give one fewer or one more`])) };
const PORTS = { L: ["value"], S: ["value", "child"], F: ["value", "left", "right"], P: ["value", "function", "argument"], Pair: ["value", "first", "second"],
  A: ["operator", "argument", "result"], T1: ["subject", "arms", "result"], Sel: ["subject", "arms", "result"], Unp: ["pair", "first", "second"],
  Dn: ["value", "copy 1", "copy 2"], Eps: ["input"], Nrm: ["input", "result"], Out: ["answer"], Sup: ["value", "first side", "second side"],
  K: ["value", "y"], Sx: ["value", "s", "b"], B: ["value", "f", "g"], C: ["value", "f", "g"], I: ["value"], W: ["value", "s"],
  R: ["program", "input", "program out", "result"], Sp: ["fork", "left", "right"], SpS: ["stem", "child"], Eat: ["leaf"],
  Cd: ["tag", "⟨a, b⟩", "y", "⟨program, result⟩"], Lk: ["state", "program", "program out", "result"], Lk2: ["tag", "⟨program, rest⟩", "program out", "result"],
  Fl: ["tag", "tag out"], Mrg: ["first", "second", "merged"], MrgL: ["second", "merged"], MrgS: ["second", "first's child", "merged"],
  MrgF: ["second", "first's left", "first's right", "merged"],
  ...Object.fromEntries(INSTR.slice(1).map((_, i) => [`R${i + 1}`, ["program", "input", "program out", "result"]])),
  Tr: ["tree", "choice"], Ut: ["outer tag", "rest", "tree"], Ut2: ["inner tag", "rest", "tree"], IsF: ["tree", "fork"],
  ...Object.fromEntries(["Dg0", "Dg1", "Dg2", "Dg3", "DgE", "DgO"].map(d => [d, ["program", "inverse"]])) };
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
  "R·L": "id: the input is the result, and the leaf goes back as the program",
  "R·F": "a; b: run a, then b on what a gives, two runs in a row; the programs they give back join in the fork again",
  ...Object.fromEntries(INSTR.slice(0, 14).map((_, k) => [`${k ? `R${k}` : "R"}·S`, `one more stem of an instruction: read on (R${k + 1}), the stem going back into the program`])),
  ...Object.fromEntries(Object.entries({ 1: "swap: split (u, v) and join (v, u)", 2: "flip: split (t, v): a flip walker reads t",
    3: "assocl: split (u, (v, w)) twice and join ((u, v), w)", 4: "assocr: split ((u, v), w) twice and join (u, (v, w))", 5: "uniti: join (△, x)",
    6: "unite: split (△, x) and eat the leaf", 7: "copy: a duplicator copies x into (x, x)", 8: "merge: split (x, y) and merge the halves, stuck if they differ",
    9: "eval: split (q, z) and run q on z; q comes back beside its result", 10: "uneval: split (q, w), invert q, run it on w, and invert what comes back into q again",
    11: "triage: a triage walker reads x", 12: "untriage: split (t, y): an untriage walker reads t", 13: "s2f: split the stem △t and join (△, t)",
    14: "f2s: split (△, t), eat the leaf and join △t" }).map(([k, m]) => [`R${k}·L`, `${m}; the leaf goes back into the program`])),
  "R1·F": "(a | b): split (u, v) and run a on u and b on v at once", "R2·F": "cond(a, b): split (t, y): a cond walker reads t",
  "R3·F": "loop(a): the input goes in as (△△, x), a loop walker reading what each round gives; the fork goes back at once, a joining it at the end",
  "Sp·F": "the fork comes apart: each child is wired to its reader", "SpS·S": "the stem comes apart", "Eat·L": "the leaf is eaten",
  "Cd·L": "the tag is a leaf: run a on y; b comes back untouched", "Cd·S": "the tag is a stem: run b on y; a comes back untouched", "Cd·F": "the tag is a fork: run b on y; a comes back untouched",
  "Lk·F": "the state (t, y) comes apart: read its tag t", "Lk2·L": "a left side (△, y): run the body again on it", "Lk2·S": "a right side (△△, y): the loop is done, y its result and the body back in the loop's fork",
  "Fl·L": "△ becomes △△", "Fl·S": "△△ becomes △", "Mrg·L": "the first half is a leaf: read the second", "Mrg·S": "the first half is a stem: read the second",
  "Mrg·F": "the first half is a fork: read the second", "MrgL·L": "both halves leaves: one leaf", "MrgS·S": "both halves stems: one stem; merge their children",
  "MrgF·F": "both halves forks: one fork; merge their children",
  "Tr·L": "a leaf: (△, (△, △))", "Tr·S": "a stem △t: (△, (△△, t))", "Tr·F": "a fork △ab: (△△, (a, b))",
  "Ut·L": "the outer tag is △: read the inner choice", "Ut·S": "the outer tag is △△: what it holds must be a fork", "Ut2·L": "(△, (△, △)) was a leaf",
  "Ut2·S": "(△, (△△, t)) was the stem △t", "IsF·F": "a fork, as untriage wants",
  "Dg0·L": "id is its own inverse", "Dg0·F": "(a; b)⁻¹ = b⁻¹; a⁻¹: the parts invert crossed over", "Dg0·S": "an instruction: hold its first stem back and read on",
  ...Object.fromEntries([["Dg1", "Dg2"], ["Dg2", "Dg3"], ["Dg3", "DgE"], ["DgE", "DgO"], ["DgO", "DgE"]].map(([d, e]) => [`${d}·S`, `another stem: the one held back goes out, and this one is held back (${e})`])),
  "Dg1·L": "swap is its own inverse", "Dg2·L": "flip is its own inverse", "Dg1·F": "(a | b)⁻¹ = (a⁻¹ | b⁻¹)", "Dg2·F": "cond(a, b)⁻¹ = cond(a⁻¹, b⁻¹)",
  "Dg3·F": "loop(a)⁻¹ = loop(a⁻¹)", "Dg3·L": "assocl's inverse is assocr: one stem more", "DgO·L": "an odd instruction's inverse is the next: one stem more",
  "DgE·L": "an even instruction's inverse is the one before: the stem held back is dropped" };
// Values (leaf, stem, fork) in greys: the colours are for applications and what carries them out.
// Movement agents in colours of what they do: K red (erases), Sx and W greens (copy), B orange and C yellow (move), I sky (a wire).
// Keeping history (21 to 58, not run here): walkers forward as apply and triage are, walkers back in pinks,
// residuals and histories amber.
const COLOR = ["#000", "#dfe4ea", "#aab3bf", "#7c8694", "#e86bd8", "#c792ea", "#56c8ff", "#4fd6be", "#2bb5a0", "#7a8cff", "#9d7cff", "#8a96a8", "#6ee07a", "#ffffff", "#f7768e",
  "#db4b4b", "#c3e88d", "#ff9e64", "#f4d35e", "#7dcfff", "#9ece6a",
  "#56c8ff", "#4fd6be", "#2bb5a0", "#2bb5a0", "#2bb5a0", "#2bb5a0", "#4fd6be", "#4fd6be", "#4fd6be", "#4fd6be", "#4fd6be", "#4fd6be",
  "#ff6ac1", "#ff8fd0", "#e05fb0", "#e05fb0", "#e05fb0", "#e05fb0", "#e07cff", "#e07cff", "#e07cff", "#e07cff", "#c792ea",
  "#ffc777", "#ffc777", "#ffc777", "#ffc777", "#ffc777",
  "#ffc777", "#56c8ff", "#ff6ac1", "#db4b4b", "#ffffff", "#6ee07a", "#ff6ac1", "#ffc777", "#ffc777", "#e05fb0",
  // Two-way programs (their programs trees, in greys): the run sky blue as an apply is, splitters teal, eating a leaf
  // an eraser's grey, the walkers that read data warm, merges violet, triage green, inverting pink as running back is.
  ...Array(15).fill("#56c8ff"), "#4fd6be", "#4fd6be", "#8a96a8", "#f4d35e", "#ff9e64", "#ff9e64", "#ff6ac1", "#e07cff", "#e07cff", "#e07cff", "#e07cff",
  "#9ece6a", "#c3e88d", "#c3e88d", "#8a96a8", ...Array(6).fill("#e05fb0")];
const kindColor = t => COLOR[t] ?? "#8a96a8";
/// A kind as its icon (shapes.js) and its name.
const kindHTML = t => `${shapeSVG(t, kindColor(t))}${NAMES[TAGS[t]] ?? `agent of kind ${t}`}`;
const isConsumer = t => t >= 6 && t <= 12 || t >= 21 && t <= 42 || [50, 51, 52, 54, 55, 58].includes(t) || isTwoWalker(t);
const isProducer = t => t >= 1 && t <= 5 || t === 14 || isView(t) || t >= 43 && t <= 49 || t === 53 || t === 56 || t === 57;
// Which ports are outputs (sources): a producer's principal, a computation's result (and residual), a
// duplicator's or unpair's two parts, a merge's value, what a run gives or runs back to.
const OUTS = { 24: [1, 2], 34: [1, 2], 36: [1, 2], 39: [2], 40: [1], 41: [2], 42: [3], 50: [1, 2], 51: [2], 52: [1], 54: [], 55: [], 58: [1],
  ...Object.fromEntries(Array.from({ length: 15 }, (_, i) => [59 + i, [2, 3]])),
  74: [1, 2], 75: [1], 76: [], 77: [3], 78: [2, 3], 79: [2, 3], 80: [1], 81: [2], 82: [1], 83: [2], 84: [3], 85: [1], 86: [2], 87: [2], 88: [1],
  89: [1], 90: [1], 91: [1], 92: [1], 93: [1], 94: [1] };
const isSource = (t, p) => isProducer(t) ? p === 0 : (t >= 6 && t <= 8) ? p === 2 : t === 12 ? p === 1 : (t === 9 || t === 10) ? p > 0
  : OUTS[t] ? OUTS[t].includes(p) : t >= 21 && t <= 42 ? p >= 2 : false;
