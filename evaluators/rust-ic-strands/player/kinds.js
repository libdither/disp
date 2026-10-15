// Every kind of agent, as the player numbers them (lattice.rs `code`): names, ports, what each rule
// does, colours. Shared by index.html and rev.html.
const TAGS = [null, "L", "S", "F", "P", "Pair", "A", "T1", "Sel", "Unp", "Dn", "Eps", "Nrm", "Out", "Sup", "K", "Sx", "B", "C", "I", "W",
  // Keeping history (set=resid=1, tags 21 on, rules.rs `RESID_TAGS`): walkers forward, walkers back, residuals, gates.
  "A+", "T1+", "Sel+", "SL+", "SS+", "SF+", "T1s+", "T1sf+", "T1sc0+", "T1sc+", "T1scf+", "TSb+",
  "A-", "Ar-", "Sel-", "SL-", "SS-", "SF-", "Dn-", "DnL-", "DnS-", "DnF-", "Arms3", "Rk", "Rs", "Rt", "Rb", "Rc", "Gate", "F-", "S-"];
const ARITY = [0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 1, 2, 1, 3, 2, 3, 3, 3, 1, 2,
  4, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 3, 4, 4, 3, 2, 3, 4, 4, 2, 4, 4, 3, 3, 2, 3, 2];
/// Keeping history (set=resid=1): the walkers that work the answer out leaving a residual (21 to 32), the
/// walkers that run it back (33 to 42, and 50, 51 taking apart a loaded value that holds applications), the arms a
/// triage carries (43), the residuals (44 to 48), the gates (49).
const isForward = t => t >= 21 && t <= 32, isBackward = t => t >= 33 && t <= 42 || t === 50 || t === 51, isResidual = t => t >= 44 && t <= 48;
/// What each residual stands for, as a tree.
const RESIDUAL_TREE = { Rk: "△△c", Rs: "△(△j₁)(△j₂j₃)", Rt: "△(△hd)r", Rb: "△(△j₁j₂)", Rc: "△(△(△j₁j₂))" };
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
  Gate: "gate", "F-": "a loaded fork taken apart", "S-": "a loaded stem taken apart" };
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
  Arms3: "a triage's three arms w, x, b, carried to the dispatch", Gate: "holds a wire shut: the answer and each apply's residual wait here until the run is taken back",
  "F-": "running back, a fork of the loaded term that holds applications, taken apart so each part runs back on its own", "S-": "running back, a stem of the loaded term that holds an application, taken apart",
  ...Object.fromEntries(Object.entries(RESIDUAL_TREE).map(([r, t]) => [r, `a residual: what a step would have dropped, standing for the tree ${t}; read back as the function by an un-apply`])) };
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
  Gate: ["in", "out"], "F-": ["fork", "left", "right"], "S-": ["stem", "child"] };
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
  "F-·F": "the loaded fork's two parts, each back to where it runs back", "S-·S": "the loaded stem's child, back to where it runs back" };
// Values (leaf, stem, fork) in greys: the colours are for applications and what carries them out.
// Movement agents in colours of what they do: K red (erases), Sx and W greens (copy), B orange and C yellow (move), I sky (a wire).
// Keeping history: walkers forward as apply and triage are, walkers back in pinks, residuals amber, gates white.
const COLOR = ["#000", "#dfe4ea", "#aab3bf", "#7c8694", "#e86bd8", "#c792ea", "#56c8ff", "#4fd6be", "#2bb5a0", "#7a8cff", "#9d7cff", "#8a96a8", "#6ee07a", "#ffffff", "#f7768e",
  "#db4b4b", "#c3e88d", "#ff9e64", "#f4d35e", "#7dcfff", "#9ece6a",
  "#56c8ff", "#4fd6be", "#2bb5a0", "#2bb5a0", "#2bb5a0", "#2bb5a0", "#4fd6be", "#4fd6be", "#4fd6be", "#4fd6be", "#4fd6be", "#4fd6be",
  "#ff6ac1", "#ff8fd0", "#e05fb0", "#e05fb0", "#e05fb0", "#e05fb0", "#e07cff", "#e07cff", "#e07cff", "#e07cff", "#c792ea",
  "#ffc777", "#ffc777", "#ffc777", "#ffc777", "#ffc777", "#ffffff", "#e05fb0", "#e05fb0"];
const kindColor = t => COLOR[t] ?? "#8a96a8";
/// A kind as its icon (shapes.js) and its name.
const kindHTML = t => `${shapeSVG(t, kindColor(t))}${NAMES[TAGS[t]] ?? `agent of kind ${t}`}`;
const isConsumer = t => t >= 6 && t <= 12 || t >= 21 && t <= 42 || t === 50 || t === 51;
const isProducer = t => t >= 1 && t <= 5 || t === 14 || isView(t) || t >= 43 && t <= 48;
// Which ports are outputs (sources): a producer's principal, a computation's result (and residual), a
// duplicator's or unpair's two parts, a merge's value, a gate's far side.
const OUTS = { 24: [1, 2], 34: [1, 2], 36: [1, 2], 39: [2], 40: [1], 41: [2], 42: [3], 49: [1], 50: [1, 2], 51: [1] };
const isSource = (t, p) => isProducer(t) ? p === 0 : (t >= 6 && t <= 8) ? p === 2 : t === 12 ? p === 1 : (t === 9 || t === 10) ? p > 0
  : OUTS[t] ? OUTS[t].includes(p) : t >= 21 && t <= 42 ? p >= 2 : false;
