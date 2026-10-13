// Every kind of agent, as the player numbers them (lattice.rs `code`): names, ports, what each rule
// does, colours. Shared by index.html and rev.html.
const TAGS = [null, "L", "S", "F", "P", "Pair", "A", "T1", "Sel", "Unp", "Dn", "Eps", "Nrm", "Out", "Sup", "K", "Sx", "B", "C", "I", "W"];
const ARITY = [0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 1, 2, 1, 3, 2, 3, 3, 3, 1, 2];
/// Movement agents (set=views=1, tags 15 to 20): loaded code standing for a tree shape, the tree each spells out as.
const VIEW_TREE = { K: "△△y", Sx: "△(△s)b", B: "△(△(△△f))g", C: "△(△f)(△△g)", I: "△(△△△)△", W: "△(△s)s" };
const isView = t => t >= 15 && t <= 20;
const NAMES = { L: "leaf", S: "stem", F: "fork", P: "suspended apply", Pair: "pair", A: "apply", T1: "triage", Sel: "second dispatch",
  Unp: "unpair", Dn: "duplicator", Eps: "eraser", Nrm: "normalizer", Out: "root", Sup: "superposition",
  K: "K: keeps, erases its argument", Sx: "Sx: copies its argument to both sides", B: "B: moves its argument right",
  C: "C: moves its argument left", I: "I: becomes a wire", W: "W: works out once, copies the result" };
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
    .map(([v, d]) => [v, `a movement agent (set=views=1): loaded code standing for ${VIEW_TREE[v]}; ${d}, in one rewrite. A duplicator or eraser takes it whole; anything else reading it spells it out as its tree first`])) };
const PORTS = { L: ["value"], S: ["value", "child"], F: ["value", "left", "right"], P: ["value", "function", "argument"], Pair: ["value", "first", "second"],
  A: ["operator", "argument", "result"], T1: ["subject", "arms", "result"], Sel: ["subject", "arms", "result"], Unp: ["pair", "first", "second"],
  Dn: ["value", "copy 1", "copy 2"], Eps: ["input"], Nrm: ["input", "result"], Out: ["answer"], Sup: ["value", "first side", "second side"],
  K: ["value", "y"], Sx: ["value", "s", "b"], B: ["value", "f", "g"], C: ["value", "f", "g"], I: ["value"], W: ["value", "s"] };
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
    ...["T1", "Sel", "Nrm"].map(q => [`${q}·${v}`, `the ${v} view is spelled out as its tree ${VIEW_TREE[v]}, for the ${NAMES[q]} to read`])])) };
// Values (leaf, stem, fork) in greys: the colours are for applications and what carries them out.
// Movement agents in colours of what they do: K red (erases), Sx and W greens (copy), B orange and C yellow (move), I sky (a wire).
const COLOR = ["#000", "#dfe4ea", "#aab3bf", "#7c8694", "#e86bd8", "#c792ea", "#56c8ff", "#4fd6be", "#2bb5a0", "#7a8cff", "#9d7cff", "#8a96a8", "#6ee07a", "#ffffff", "#f7768e",
  "#db4b4b", "#c3e88d", "#ff9e64", "#f4d35e", "#7dcfff", "#9ece6a"];
const kindColor = t => COLOR[t] ?? "#8a96a8";
/// A kind as its icon (shapes.js) and its name.
const kindHTML = t => `${shapeSVG(t, kindColor(t))}${NAMES[TAGS[t]] ?? `agent of kind ${t}`}`;
const isConsumer = t => t >= 6 && t <= 12;
const isProducer = t => t >= 1 && t <= 5 || t === 14 || isView(t);
// Which ports are outputs (sources): a producer's principal, a computation's result, a duplicator's or unpair's two parts.
const isSource = (t, p) => isProducer(t) ? p === 0 : (t >= 6 && t <= 8) ? p === 2 : t === 12 ? p === 1 : (t === 9 || t === 10) ? p > 0 : false;
