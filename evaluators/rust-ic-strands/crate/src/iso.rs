//! Two-way programs: a calculus on trees in which every program runs both ways
//! (research/REVERSIBLE_PIPELINE.md). Data, programs, the library of definitions and the trace an
//! ordinary program leaves are all trees, and a program is built only from steps that lose nothing:
//! - rearrange by pattern (`Re`): take the tree apart by one pattern and put the pieces together by
//!   another with the same pieces; this does every swapping, regrouping, wrapping and unwrapping;
//! - copy (`Dup`, `a → F a a`), whose inverse takes back a copy only from two equal halves;
//! - work on one part (`On`), run one step after another (`Seq`), choose (`Case`), loop (`Loop`);
//! - call a definition (`Call`), or run a program that arrives as data (`Eval`).
//! So every program has an inverse read off its text (`Iso::dagger`). A choice goes forward by the
//! input matching an arm's `when` and backward by the output matching its `gives`, the first arm that
//! matches winning both ways (the "symmetric first-match" of reversible functional languages); a loop
//! states what holds on the way in and on no later round, and on the way out and on no earlier round,
//! as Janus's loops do. `Forget` throws a tree away: the one step that cannot be taken back.
//!
//! The tree calculus is a library program here, `tc`: the application of a tree to a tree, eagerly,
//! handing back with the answer a trace of what it dropped. K moves its argument onto the trace, the
//! S rule copies its argument, and triage moves the arms it does not take onto the trace. With the
//! trace kept, any disp program runs both ways; `bennett` cleans the trace up again.
//!
//! `Machine` runs a program as a token walking the program tree: down into an instruction, through its
//! parts, back up. Working on one part parks the rest of the tree at that instruction until the token
//! comes back up with the part; calling a definition lays a copy of it out under the call while the
//! token is inside. Every step is taken back by turning the token around and stepping: nothing is
//! recorded anywhere.
use rust_ca_lattice::oracle::Term;
use std::rc::Rc;

pub(crate) fn fork(a: Term, b: Term) -> Term { Term::F(Rc::new(a), Rc::new(b)) }
pub(crate) fn stem(a: Term) -> Term { Term::S(Rc::new(a)) }
pub fn nat(n: usize) -> Term { (0..n).fold(Term::L, |t, _| fork(Term::L, t)) }
pub(crate) fn list(xs: Vec<Term>) -> Term { xs.into_iter().rev().fold(Term::L, |t, x| fork(x, t)) }

// ---- patterns -----------------------------------------------------------------------------------

/// A tree with holes: `Var` names a piece (once on each side of a rearrangement), `Any` matches
/// anything (only in tests).
#[derive(Clone, Debug, PartialEq)]
pub enum Pat { L, S(Box<Pat>), F(Box<Pat>, Box<Pat>), Var(u8), Any }

impl Pat {
    /// `L`, `S(p)`, `F(p, q)`, a lowercase letter for a piece, `_` for anything.
    pub fn parse(src: &str) -> Pat {
        fn go(b: &[u8], i: &mut usize) -> Pat {
            while b[*i] == b' ' { *i += 1; }
            let c = b[*i];
            *i += 1;
            let arg = |b: &[u8], i: &mut usize| { while b[*i] == b' ' || b[*i] == b'(' || b[*i] == b',' { *i += 1; } let p = go(b, i); while *i < b.len() && b[*i] == b' ' { *i += 1; } p };
            match c {
                b'L' => Pat::L,
                b'_' => Pat::Any,
                b'S' => { let a = arg(b, i); *i += 1; Pat::S(Box::new(a)) }
                b'F' => { let a = arg(b, i); let p = arg(b, i); *i += 1; Pat::F(Box::new(a), Box::new(p)) }
                c if c.is_ascii_lowercase() => Pat::Var(c - b'a'),
                c => panic!("a pattern cannot have {:?}", c as char),
            }
        }
        let mut i = 0;
        go(src.as_bytes(), &mut i)
    }
    /// As the player writes trees: △ a leaf, △x a stem, (x, y) a fork.
    pub fn show(&self) -> String {
        match self {
            Pat::L => "△".into(),
            Pat::S(a) => format!("△{}", a.show()),
            Pat::F(a, b) => format!("({}, {})", a.show(), b.show()),
            Pat::Var(v) => ((b'a' + v) as char).to_string(),
            Pat::Any => "_".into(),
        }
    }
    fn vars(&self, out: &mut Vec<u8>) {
        match self { Pat::S(a) => a.vars(out), Pat::F(a, b) => { a.vars(out); b.vars(out) } Pat::Var(v) => out.push(*v), _ => {} }
    }
    fn has_any(&self) -> bool { match self { Pat::Any => true, Pat::S(a) => a.has_any(), Pat::F(a, b) => a.has_any() || b.has_any(), _ => false } }
    /// Whether t fits, binding pieces in env.
    fn bind(&self, t: &Term, env: &mut [Option<Term>; 26]) -> bool {
        match (self, t) {
            (Pat::Any, _) => true,
            (Pat::Var(v), _) => { env[*v as usize] = Some(t.clone()); true }
            (Pat::L, Term::L) => true,
            (Pat::S(a), Term::S(x)) => a.bind(x, env),
            (Pat::F(a, b), Term::F(x, y)) => a.bind(x, env) && b.bind(y, env),
            _ => false,
        }
    }
    pub(crate) fn fits(&self, t: &Term) -> bool { self.bind(t, &mut Default::default()) }
    /// The pattern only this tree fits.
    pub fn lit(t: &Term) -> Pat {
        match t { Term::L => Pat::L, Term::S(a) => Pat::S(Box::new(Pat::lit(a))), Term::F(a, b) => Pat::F(Box::new(Pat::lit(a)), Box::new(Pat::lit(b))), Term::Ap(..) => unreachable!() }
    }
    fn build(&self, env: &mut [Option<Term>; 26]) -> Term {
        match self {
            Pat::L => Term::L,
            Pat::S(a) => stem(a.build(env)),
            Pat::F(a, b) => { let x = a.build(env); fork(x, b.build(env)) }
            Pat::Var(v) => env[*v as usize].take().expect("a piece the pattern names"),
            Pat::Any => panic!("a rearrangement builds no `_`"),
        }
    }
}

/// Which part of a tree: a fork's left or right, or a stem's child.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Dir { Left, Right, Under }

// ---- programs -------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Clause { pub when: Pat, pub body: Iso, pub gives: Pat }

#[derive(Clone, Debug, PartialEq)]
pub enum Iso {
    Seq(Vec<Iso>),
    /// Rearrange: the tree as the left pattern, rebuilt as the right one.
    Re(Pat, Pat),
    /// Copy (`a → F a a`), or with `true` take back a copy.
    Dup(bool),
    /// Work on the part at a path, the rest parked.
    On(Vec<Dir>, Box<Iso>),
    Case(Vec<Clause>),
    /// `from` holds on the way in and on no later round; `body` runs; `until` holds on the way out and on
    /// no earlier round, else `again` runs and the loop goes round.
    Loop { from: Pat, body: Box<Iso>, until: Pat, again: Box<Iso> },
    /// A library definition, by number; `true`: its inverse.
    Call(usize, bool),
    /// `F p x → F p (p x)`: run the program that is the left half on the right half; `true`: backward.
    Eval(bool),
    /// `F a t → a`: throw t away. The one step with no inverse.
    Forget,
    /// What the part does, said in words: the part runs as it is (a note in the program's tree).
    Note(String, Box<Iso>),
    /// `F a b → F (f a) (g b)`: each half of a pair by its own program, the two at once (a machine with
    /// more than one token splits it in two; one runs them one after the other).
    Both(Box<Iso>, Box<Iso>),
}

impl Iso {
    /// The inverse, read off the text. (`Forget` has none; its inverse refuses to run.)
    pub fn dagger(&self) -> Iso {
        match self {
            Iso::Seq(xs) => Iso::Seq(xs.iter().rev().map(Iso::dagger).collect()),
            Iso::Re(l, r) => Iso::Re(r.clone(), l.clone()),
            Iso::Dup(b) => Iso::Dup(!b),
            Iso::On(p, f) => Iso::On(p.clone(), Box::new(f.dagger())),
            Iso::Case(cs) => Iso::Case(cs.iter().map(|c| Clause { when: c.gives.clone(), body: c.body.dagger(), gives: c.when.clone() }).collect()),
            Iso::Loop { from, body, until, again } =>
                Iso::Loop { from: until.clone(), body: Box::new(body.dagger()), until: from.clone(), again: Box::new(again.dagger()) },
            Iso::Call(d, v) => Iso::Call(*d, !v),
            Iso::Eval(v) => Iso::Eval(!v),
            Iso::Forget => Iso::Forget,
            Iso::Note(s, f) => Iso::Note(s.clone(), Box::new(f.dagger())),
            Iso::Both(f, g) => Iso::Both(Box::new(f.dagger()), Box::new(g.dagger())),
        }
    }
    /// What the program's node says, for the player.
    pub fn label(&self, defs: &Defs) -> String {
        match self {
            Iso::Seq(_) => "then".into(),
            Iso::Re(l, r) => format!("{} ↔ {}", l.show(), r.show()),
            Iso::Dup(false) => "copy".into(),
            Iso::Dup(true) => "un-copy".into(),
            Iso::On(p, _) => format!("on {}", show_path(p)),
            Iso::Case(_) => "choose".into(),
            Iso::Loop { from, until, .. } => format!("loop from {} until {}", from.show(), until.show()),
            Iso::Call(d, v) => format!("{}{}", defs.names[*d], if *v { "⁻¹" } else { "" }),
            Iso::Eval(v) => format!("run{}", if *v { "⁻¹" } else { "" }),
            Iso::Forget => "forget".into(),
            Iso::Note(s, _) => s.clone(),
            Iso::Both(..) => "both".into(),
        }
    }
}

pub fn show_path(p: &[Dir]) -> String {
    p.iter().map(|d| match d { Dir::Left => "left", Dir::Right => "right", Dir::Under => "child" }).collect::<Vec<_>>().join(".")
}

/// Named definitions; a `Call` refers to one by its place.
#[derive(Clone, Debug)]
pub struct Defs { pub names: Vec<&'static str>, pub bodies: Vec<Iso>, inverses: Vec<Iso>, pub jets: Vec<Jet> }

/// A disp program the library has a two-way definition for, giving what the program gives (a jet, as
/// Urbit calls one; disp's `.opt.disp` overrides are the same idea): the program's tree, how many
/// arguments it takes, and the definition, which takes one argument to the answer, or two, `F a b`, to
/// `F a answer`.
#[derive(Clone, Debug)]
pub struct Jet { pub tree: Term, pub arity: usize, pub def: usize }

/// disp's programs (programs.disp) that a library definition gives the same answers as, and how many
/// arguments each takes.
pub const JETTED: [(&str, usize); 2] = [("add", 2), ("rev", 1)];

impl Defs {
    pub fn new(defs: Vec<(&'static str, Iso)>) -> Defs {
        let (names, bodies): (Vec<_>, Vec<_>) = defs.into_iter().unzip();
        let inverses = bodies.iter().map(Iso::dagger).collect();
        Defs { names, bodies, inverses, jets: vec![] }
    }
    /// The library with jets for those of disp's programs, given by name and tree, that it has a
    /// definition for (`JETTED`).
    pub fn with_jets(mut self, programs: &[(String, Term)]) -> Defs {
        for (name, tree) in programs {
            let Some(&(_, arity)) = JETTED.iter().find(|(n, _)| n == name) else { continue };
            self.jets.push(Jet { tree: tree.clone(), arity, def: self.find(name).expect("a jetted definition") });
        }
        self
    }
    pub fn find(&self, name: &str) -> Option<usize> { self.names.iter().position(|n| *n == name) }
    pub fn body(&self, def: usize, inv: bool) -> &Iso { if inv { &self.inverses[def] } else { &self.bodies[def] } }
    /// Every rearrangement names each piece once on each side, and builds no `_`.
    pub fn check(&self) -> Result<(), String> {
        fn go(i: &Iso, name: &str) -> Result<(), String> {
            match i {
                Iso::Re(l, r) => {
                    let (mut a, mut b) = (vec![], vec![]);
                    l.vars(&mut a); r.vars(&mut b);
                    a.sort(); b.sort();
                    if a != b || a.windows(2).any(|w| w[0] == w[1]) || l.has_any() || r.has_any() {
                        return Err(format!("{name}: {} ↔ {} does not keep every piece exactly once", l.show(), r.show()));
                    }
                    Ok(())
                }
                Iso::Seq(xs) => xs.iter().try_for_each(|x| go(x, name)),
                Iso::On(_, f) => go(f, name),
                Iso::Case(cs) => cs.iter().try_for_each(|c| go(&c.body, name)),
                Iso::Loop { body, again, .. } => { go(body, name)?; go(again, name) }
                Iso::Note(_, f) => go(f, name),
                Iso::Both(f, g) => { go(f, name)?; go(g, name) }
                _ => Ok(()),
            }
        }
        self.names.iter().zip(&self.bodies).try_for_each(|(n, b)| go(b, n))
    }
}

/// The library: arithmetic and lists on disp values (a number is n forks over a leaf, F(L, n-1); a list
/// is F(head, tail) ending in a leaf), the tree calculus as `tc`, and `dagger`, which inverts a program
/// given as a tree.
pub fn library() -> Defs {
    use Iso::*;
    let re = |l: &str, r: &str| Re(Pat::parse(l), Pat::parse(r));
    let on = |p: &[Dir], f: Iso| On(p.to_vec(), Box::new(f));
    let cl = |w: &str, body: Iso, g: &str| Clause { when: Pat::parse(w), body, gives: Pat::parse(g) };
    let (l, r) = (Dir::Left, Dir::Right);
    let id = || Seq(vec![]);
    let note = |s: &str, f: Iso| Note(s.into(), Box::new(f));
    let names = ["add", "double", "snoc", "rev", "revfast", "tc", "dagger", "daggers", "daggerclauses", "flip", "swap", "succ", "tck"];
    let n = |s: &str| names.iter().position(|x| *x == s).unwrap();
    let call = |s: &str| Call(n(s), false);
    let defs = Defs::new(vec![
        // F a b → F a (a + b): b gets a's forks one by one, and a comes back as it was.
        ("add", Case(vec![
            cl("F(L,b)", id(), "F(L,_)"),
            cl("F(F(L,a),b)", Seq(vec![re("F(F(L,a),b)", "F(L,F(a,b))"), on(&[r], call("add")), re("F(L,F(a,s))", "F(F(L,a),F(L,s))")]), "F(F(_,_),_)"),
        ])),
        // a → F a (a + a).
        ("double", Seq(vec![Dup(false), call("add")])),
        // F xs x → xs with x on the end. Backward the arms are told apart by the answer's tail.
        ("snoc", Case(vec![
            cl("F(L,x)", re("F(L,x)", "F(x,L)"), "F(_,L)"),
            cl("F(F(y,z),x)", Seq(vec![re("F(F(y,z),x)", "F(y,F(z,x))"), on(&[r], call("snoc"))]), "F(_,F(_,_))"),
        ])),
        // A list reversed by putting each head on the end of the reversed tail: n² steps.
        ("rev", Case(vec![
            cl("L", id(), "L"),
            cl("F(x,y)", Seq(vec![on(&[r], call("rev")), re("F(x,y)", "F(y,x)"), call("snoc")]), "F(_,_)"),
        ])),
        // The same in n steps: heads move one by one onto an accumulator that is empty on the way in
        // (and never again), until the list is empty (and not before).
        ("revfast", Seq(vec![re("x", "F(x,L)"),
            Loop { from: Pat::parse("F(_,L)"), body: Box::new(id()), until: Pat::parse("F(L,_)"), again: Box::new(re("F(F(x,y),a)", "F(y,F(x,a))")) },
            re("F(L,x)", "x")])),
        // The tree calculus: F (F p x) t → F (p x) t', p applied to x eagerly, the trace t' being t with
        // what it took to get there on top. Its arms are told apart, going back, by that top entry.
        ("tc", Case(vec![
            // △ x = △x and (△a) x = △ a x build: the trace says which.
            cl("F(F(L,x),t)", note("△ x = △x", re("F(F(L,x),t)", "F(S(x),F(L,t))")), "F(_,F(L,_))"),
            cl("F(F(S(a),x),t)", note("△a x = △ a x", re("F(F(S(a),x),t)", "F(F(a,x),F(S(L),t))")), "F(_,F(S(_),_))"),
            cl("F(F(F(_,_),_),_)", Case(vec![
                // K: (△ △ b) x = b; x goes onto the trace.
                cl("F(F(F(L,b),x),t)", note("K: △ △ b x = b", re("F(F(F(L,b),x),t)", "F(b,F(F(L,x),t))")), "F(_,F(F(L,_),_))"),
                // S: (△ (△c) b) x = c x (b x): x copied, three applications.
                cl("F(F(F(S(c),b),x),t)", note("S: △ (△c) b x = c x (b x)", Seq(vec![
                    on(&[l, r], Dup(false)),
                    re("F(F(F(S(c),b),F(x,y)),t)", "F(F(F(c,x),t),F(b,y))"),
                    on(&[l], call("tc")),
                    re("F(F(p,t),F(b,y))", "F(p,F(F(b,y),t))"),
                    on(&[r], call("tc")),
                    re("F(p,F(q,t))", "F(F(p,q),t)"),
                    call("tc"),
                    re("F(p,t)", "F(p,F(F(S(L),L),t))"),
                ])), "F(_,F(F(S(_),_),_))"),
                // Triage: (△ (△ w y) b) x by the shape of x; the arms not taken go onto the trace.
                cl("F(F(F(F(_,_),_),_),_)", Case(vec![
                    cl("F(F(F(F(w,y),b),L),t)", note("triage: △ (△ w y) b △ = w", re("F(F(F(F(w,y),b),L),t)", "F(w,F(F(F(L,L),F(y,b)),t))")), "F(_,F(F(F(L,_),_),_))"),
                    cl("F(F(F(F(w,y),b),S(u)),t)", note("triage: △ (△ w y) b (△u) = y u", Seq(vec![
                        re("F(F(F(F(w,y),b),S(u)),t)", "F(F(F(y,u),t),F(w,b))"),
                        on(&[l], call("tc")),
                        re("F(F(p,t),F(w,b))", "F(p,F(F(F(S(L),L),F(w,b)),t))"),
                    ])), "F(_,F(F(F(S(_),_),_),_))"),
                    cl("F(F(F(F(w,y),b),F(u,v)),t)", note("triage: △ (△ w y) b (△ u v) = b u v", Seq(vec![
                        re("F(F(F(F(w,y),b),F(u,v)),t)", "F(F(F(F(b,u),t),v),F(w,y))"),
                        on(&[l, l], call("tc")),
                        re("F(F(F(p,t),v),F(w,y))", "F(F(F(p,v),t),F(w,y))"),
                        on(&[l], call("tc")),
                        re("F(F(p,t),F(w,y))", "F(p,F(F(F(F(L,L),L),F(w,y)),t))"),
                    ])), "F(_,F(F(F(F(_,_),_),_),_))"),
                ]), "F(_,F(F(F(_,_),_),_))"),
            ]), "F(_,F(F(_,_),_))"),
        ])),
        // A program, as a tree (`encode`), → its inverse, as a tree. Its own inverse, as a function.
        ("dagger", Case(vec![
            // then: the steps reversed, each inverted
            cl("F(L,_)", note("then: each step inverted, last first", on(&[r], Seq(vec![call("daggers"), call("revfast")]))), "F(L,_)"),
            // rearrange: the two patterns swapped
            cl("F(F(L,L),_)", note("rearrange: the patterns swapped", on(&[r], re("F(a,b)", "F(b,a)"))), "F(F(L,L),_)"),
            // copy ↔ un-copy, call ↔ call backward, run ↔ run backward
            cl("F(F(L,F(L,L)),_)", note("copy ↔ un-copy", on(&[r], call("flip"))), "F(F(L,F(L,L)),_)"),
            // on a part: the step inverted
            cl("F(F(L,F(L,F(L,L))),_)", note("on a part: the step inverted", on(&[r, r], call("dagger"))), "F(F(L,F(L,F(L,L))),_)"),
            // choose: each arm's when and gives swapped, its body inverted
            cl("F(F(L,F(L,F(L,F(L,L)))),_)", note("choose: when ↔ gives, each arm inverted", on(&[r], call("daggerclauses"))), "F(F(L,F(L,F(L,F(L,L)))),_)"),
            // loop: from and until swapped, both bodies inverted
            cl("F(F(L,F(L,F(L,F(L,F(L,L))))),_)", note("loop: from ↔ until, both parts inverted", Seq(vec![
                on(&[r], re("F(F(f,u),F(b,a))", "F(F(u,f),F(b,a))")),
                on(&[r, r, l], call("dagger")), on(&[r, r, r], call("dagger")),
            ])), "F(F(L,F(L,F(L,F(L,F(L,L))))),_)"),
            cl("F(F(L,F(L,F(L,F(L,F(L,F(L,L)))))),_)", note("call ↔ call backward", on(&[r, r], call("flip"))), "F(F(L,F(L,F(L,F(L,F(L,F(L,L)))))),_)"),
            cl("F(F(L,F(L,F(L,F(L,F(L,F(L,F(L,L))))))),_)", note("run ↔ run backward", on(&[r], call("flip"))), "F(F(L,F(L,F(L,F(L,F(L,F(L,F(L,L))))))),_)"),
            // a note: its words kept, its part inverted
            cl("F(F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,L))))))))),_)", note("a note: the words kept, the part inverted", on(&[r, r], call("dagger"))), "F(F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,L))))))))),_)"),
            // both: each side inverted, the two at once
            cl("F(F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,L)))))))))),_)", note("both: each side inverted", on(&[r], Both(Box::new(call("dagger")), Box::new(call("dagger"))))), "F(F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,F(L,L)))))))))),_)"),
        ])),
        // Each program of a list inverted.
        ("daggers", Case(vec![
            cl("L", id(), "L"),
            cl("F(_,_)", Seq(vec![on(&[l], call("dagger")), on(&[r], call("daggers"))]), "F(_,_)"),
        ])),
        // Each arm of a choice: when and gives swapped, body inverted.
        ("daggerclauses", Case(vec![
            cl("L", id(), "L"),
            cl("F(_,_)", Seq(vec![on(&[l], re("F(F(w,g),b)", "F(F(g,w),b)")), on(&[l, r], call("dagger")), on(&[r], call("daggerclauses"))]), "F(_,_)"),
        ])),
        // false ↔ true (0 ↔ 1).
        ("flip", Case(vec![
            cl("L", re("L", "F(L,L)"), "F(L,L)"),
            cl("F(L,L)", re("F(L,L)", "L"), "L"),
        ])),
        // (a, b) → (b, a): one step.
        ("swap", re("F(a,b)", "F(b,a)")),
        // n → n + 1: a fork with a leaf on top. Backward it takes one off, and has no answer for 0.
        ("succ", re("x", "F(L,x)")),
        // The tree calculus keeping the program it applies: F (F p x) t → F (F p (p x)) t'. Going back,
        // the rule is read off p, so the trace holds only what the data decides: K's dropped argument,
        // the function an S or a fork's dispatch made on the way (c x, b u), and which way a dispatch went.
        ("tck", Case(vec![
            cl("F(F(L,_),_)", note("△ x = △x", re("F(F(L,x),t)", "F(F(L,S(x)),t)")), "F(F(L,_),_)"),
            cl("F(F(S(_),_),_)", note("△a x = △ a x", Seq(vec![on(&[l, l, Dir::Under], Dup(false)), re("F(F(S(F(a,b)),x),t)", "F(F(S(a),F(b,x)),t)")])), "F(F(S(_),_),_)"),
            cl("F(F(F(_,_),_),_)", Case(vec![
                cl("F(F(F(L,_),_),_)", note("K: △ △ b x = b", Seq(vec![on(&[l, l, r], Dup(false)), re("F(F(F(L,F(b,c)),x),t)", "F(F(F(L,b),c),F(x,t))")])), "F(F(F(L,_),_),_)"),
                // c x and b x at once: c x going on with the trace, b x starting one of its own, which
                // then goes onto it
                cl("F(F(F(S(_),_),_),_)", note("S: △ (△c) b x = c x (b x)", Seq(vec![
                    on(&[l, r], Dup(false)),
                    re("F(F(F(S(c),b),F(x,y)),t)", "F(F(F(c,x),t),F(F(b,y),L))"),
                    Both(Box::new(call("tck")), Box::new(call("tck"))),
                    re("F(F(F(c,p),t),F(F(b,q),v))", "F(F(F(p,q),F(v,t)),F(c,b))"),
                    on(&[l], call("tck")),
                    re("F(F(F(p,r),t),F(c,b))", "F(F(F(S(c),b),r),F(p,t))"),
                ])), "F(F(F(S(_),_),_),_)"),
                cl("F(F(F(F(_,_),_),_),_)", Case(vec![
                    cl("F(F(F(F(_,_),_),L),_)", note("triage: △ (△ w y) b △ = w", Seq(vec![
                        on(&[l, l, l, l], Dup(false)),
                        re("F(F(F(F(F(w,v),y),b),L),t)", "F(F(F(F(w,y),b),v),F(L,t))"),
                    ])), "F(_,F(L,_))"),
                    cl("F(F(F(F(_,_),_),S(_)),_)", note("triage: △ (△ w y) b (△u) = y u", Seq(vec![
                        re("F(F(F(F(w,y),b),S(u)),t)", "F(F(F(y,u),t),F(w,b))"),
                        on(&[l], call("tck")),
                        re("F(F(F(y,r),t),F(w,b))", "F(F(F(F(w,y),b),r),F(S(L),t))"),
                    ])), "F(_,F(S(_),_))"),
                    cl("F(F(F(F(_,_),_),F(_,_)),_)", note("triage: △ (△ w y) b (△ u v) = b u v", Seq(vec![
                        re("F(F(F(F(w,y),b),F(u,v)),t)", "F(F(F(b,u),t),F(F(w,y),v))"),
                        on(&[l], call("tck")),
                        re("F(F(F(b,p),t),F(F(w,y),v))", "F(F(F(p,v),t),F(F(w,y),b))"),
                        on(&[l], call("tck")),
                        re("F(F(F(p,r),t),F(F(w,y),b))", "F(F(F(F(w,y),b),r),F(F(p,F(L,L)),t))"),
                    ])), "F(_,F(F(_,_),_))"),
                ]), "F(F(F(F(_,_),_),_),_)"),
            ]), "F(F(F(_,_),_),_)"),
        ])),
    ]);
    debug_assert_eq!(defs.names, names);
    defs.check().expect("the library's rearrangements keep their pieces");
    defs
}

/// `f` with the trace cleaned up, after Bennett: x → F x (f x) for an f taking F x t to F y t'. It runs
/// f, copies the answer, then runs f backward, which takes the trace back to nothing.
pub fn bennett(f: Iso) -> Iso {
    let re = |l: &str, r: &str| Iso::Re(Pat::parse(l), Pat::parse(r));
    Iso::Seq(vec![
        re("x", "F(x,L)"),
        f.clone(),
        Iso::On(vec![Dir::Left], Box::new(Iso::Dup(false))),
        re("F(F(y,c),t)", "F(F(y,t),c)"),
        Iso::On(vec![Dir::Left], Box::new(f.dagger())),
        re("F(F(x,L),c)", "F(x,c)"),
    ])
}

/// A bijection given as two tree-calculus programs, f and its inverse g, run with nothing left over:
/// F (F f g) x → F (F f g) (f x). Bennett's second step: f x is worked out (cleaned up, so x comes back
/// with it), then x is taken back by working g out on f x backward, which only fits if g (f x) is x.
pub fn bijection(defs: &Defs) -> Iso {
    let re = |l: &str, r: &str| Iso::Re(Pat::parse(l), Pat::parse(r));
    let clean = bennett(tc_apply(defs, 1));
    Iso::Seq(vec![
        re("F(F(f,g),x)", "F(g,F(f,x))"),
        Iso::On(vec![Dir::Right], Box::new(clean.clone())),
        re("F(g,F(F(f,x),y))", "F(f,F(F(g,y),x))"),
        Iso::On(vec![Dir::Right], Box::new(clean.dagger())),
        re("F(f,F(g,y))", "F(F(f,g),y)"),
    ])
}

/// `tck` (the tree calculus keeping what it applies) on k arguments one after the other: F (… F (F p a₁) a₂ …)
/// aₖ with a trace → F (F (F … (p, r₁) …) rₖ₋₁) rₖ) with the trace, each function kept beside what it gave.
pub fn tck_apply(defs: &Defs, k: usize) -> Iso {
    let tck = Iso::Call(defs.find("tck").unwrap(), false);
    if k <= 1 { return tck; }
    let re = |l: &str, r: &str| Iso::Re(Pat::parse(l), Pat::parse(r));
    Iso::Seq(vec![
        re("F(F(f,a),t)", "F(F(f,t),a)"),
        Iso::On(vec![Dir::Left], Box::new(tck_apply(defs, k - 1))),
        re("F(F(F(g,q),t),a)", "F(F(F(q,a),t),g)"),
        Iso::On(vec![Dir::Left], Box::new(tck)),
        re("F(F(F(q,r),t),g)", "F(F(F(g,q),r),t)"),
    ])
}

/// `tck_apply`, but a program the library has a definition for (`Defs::jets`) runs as that instead: its
/// arguments to the answer, the program and the trace left as they were (with two arguments, the first
/// is kept where the program would have kept what it made of it).
pub fn jet_apply(defs: &Defs, k: usize) -> Iso {
    let re = |l: &str, r: &str| Iso::Re(Pat::parse(l), Pat::parse(r));
    let on = |d: Dir, f: Iso| Iso::On(vec![d], Box::new(f));
    let fork = |a: Pat, b: Pat| Pat::F(Box::new(a), Box::new(b));
    let mut arms: Vec<Clause> = defs.jets.iter().filter(|j| j.arity == k && k <= 2).map(|j| {
        let call = Iso::Call(j.def, false);
        let (shape, body) = if k == 1 {
            (fork(fork(Pat::lit(&j.tree), Pat::Any), Pat::Any), on(Dir::Left, on(Dir::Right, call)))
        } else {
            (fork(fork(fork(Pat::lit(&j.tree), Pat::Any), Pat::Any), Pat::Any),
             on(Dir::Left, Iso::Seq(vec![re("F(F(p,a),b)", "F(p,F(a,b))"), on(Dir::Right, call), re("F(p,F(a,r))", "F(F(p,a),r)")])))
        };
        Clause { when: shape.clone(), body, gives: shape }
    }).collect();
    if arms.is_empty() { return tck_apply(defs, k); }
    arms.push(Clause { when: Pat::Any, body: tck_apply(defs, k), gives: Pat::Any });
    Iso::Case(arms)
}

/// The tree calculus applied to k arguments one after the other: F (… F (F p a₁) a₂ …) aₖ, with a
/// trace, → the answer, with the trace.
pub fn tc_apply(defs: &Defs, k: usize) -> Iso {
    let tc = Iso::Call(defs.find("tc").unwrap(), false);
    if k <= 1 { return tc; }
    let re = |l: &str, r: &str| Iso::Re(Pat::parse(l), Pat::parse(r));
    Iso::Seq(vec![
        re("F(F(f,a),t)", "F(F(f,t),a)"),
        Iso::On(vec![Dir::Left], Box::new(tc_apply(defs, k - 1))),
        re("F(F(y,t),a)", "F(F(y,a),t)"),
        tc,
    ])
}

// ---- programs as trees ---------------------------------------------------------------------------

/// A program as a tree: F (n) args with n the instruction's number (0 then, 1 rearrange, 2 copy, 3 on,
/// 4 choose, 5 loop, 6 call, 7 run, 8 forget, 9 a note: its words as a list of code points) and its arguments after it; a pattern as F (n) … too
/// (0 leaf, 1 stem, 2 fork), a piece as a stem of its number, anything as a leaf. A piece's number is its
/// rank among the letters its instruction's patterns use (a rearrangement's two sides together), so the
/// tree spells it with as few agents as can be, and swapping the sides (`dagger`) keeps the numbers.
pub fn encode(i: &Iso) -> Term {
    let b = |v: bool| nat(v as usize);
    let (op, args) = match i {
        Iso::Seq(xs) => (0, list(xs.iter().map(encode).collect())),
        Iso::Re(l, r) => { let [a, b] = encode_pats([l, r]); (1, fork(a, b)) }
        Iso::Dup(v) => (2, b(*v)),
        Iso::On(p, f) => (3, fork(list(p.iter().map(|d| nat(*d as usize)).collect()), encode(f))),
        Iso::Case(cs) => (4, list(cs.iter().map(|c| fork(fork(encode_pat(&c.when), encode_pat(&c.gives)), encode(&c.body))).collect())),
        Iso::Loop { from, body, until, again } => (5, fork(fork(encode_pat(from), encode_pat(until)), fork(encode(body), encode(again)))),
        Iso::Call(d, v) => (6, fork(nat(*d), b(*v))),
        Iso::Eval(v) => (7, b(*v)),
        Iso::Forget => (8, Term::L),
        Iso::Note(s, f) => (9, fork(list(s.chars().map(|c| nat(c as usize)).collect()), encode(f))),
        Iso::Both(f, g) => (10, fork(encode(f), encode(g))),
    };
    fork(nat(op), args)
}
pub fn encode_pat(p: &Pat) -> Term { let [t] = encode_pats([p]); t }
/// Patterns that name the same pieces, as trees, each piece numbered by its rank among their letters.
pub fn encode_pats<const N: usize>(ps: [&Pat; N]) -> [Term; N] {
    let mut letters = vec![];
    for p in ps { p.vars(&mut letters); }
    letters.sort(); letters.dedup();
    fn go(p: &Pat, letters: &[u8]) -> Term {
        match p {
            Pat::Any => Term::L,
            Pat::Var(v) => stem(nat(letters.binary_search(v).unwrap())),
            Pat::L => fork(nat(0), Term::L),
            Pat::S(a) => fork(nat(1), go(a, letters)),
            Pat::F(a, b) => fork(nat(2), fork(go(a, letters), go(b, letters))),
        }
    }
    ps.map(|p| go(p, &letters))
}
fn read_nat(t: &Term) -> Option<usize> { let mut n = 0; let mut t = t; while let Term::F(a, b) = t { if **a != Term::L { return None; } n += 1; t = b; } (*t == Term::L).then_some(n) }
fn read_list(t: &Term) -> Option<Vec<&Term>> { let mut out = vec![]; let mut t = t; while let Term::F(a, b) = t { out.push(&**a); t = b; } (*t == Term::L).then_some(out) }
/// The program a tree is, if it is one.
pub fn decode(t: &Term) -> Option<Iso> {
    let Term::F(op, a) = t else { return None };
    let pair = |t: &Term| match t { Term::F(x, y) => Some(((**x).clone(), (**y).clone())), _ => None };
    let flag = |t: &Term| match read_nat(t)? { 0 => Some(false), 1 => Some(true), _ => None };
    Some(match read_nat(op)? {
        0 => Iso::Seq(read_list(a)?.into_iter().map(decode).collect::<Option<_>>()?),
        1 => { let (l, r) = pair(a)?; Iso::Re(decode_pat(&l)?, decode_pat(&r)?) }
        2 => Iso::Dup(flag(a)?),
        3 => { let (p, f) = pair(a)?; Iso::On(read_list(&p)?.into_iter().map(|d| match read_nat(d)? { 0 => Some(Dir::Left), 1 => Some(Dir::Right), 2 => Some(Dir::Under), _ => None }).collect::<Option<_>>()?, Box::new(decode(&f)?)) }
        4 => Iso::Case(read_list(a)?.into_iter().map(|c| { let (wg, b) = pair(c)?; let (w, g) = pair(&wg)?; Some(Clause { when: decode_pat(&w)?, body: decode(&b)?, gives: decode_pat(&g)? }) }).collect::<Option<_>>()?),
        5 => { let (fu, ba) = pair(a)?; let (f, u) = pair(&fu)?; let (b, g) = pair(&ba)?; Iso::Loop { from: decode_pat(&f)?, body: Box::new(decode(&b)?), until: decode_pat(&u)?, again: Box::new(decode(&g)?) } }
        6 => { let (d, v) = pair(a)?; Iso::Call(read_nat(&d)?, flag(&v)?) }
        7 => Iso::Eval(flag(a)?),
        8 => Iso::Forget,
        9 => { let (w, f) = pair(a)?; Iso::Note(read_list(&w)?.into_iter().map(|c| char::from_u32(read_nat(c)? as u32)).collect::<Option<_>>()?, Box::new(decode(&f)?)) }
        10 => { let (f, g) = pair(a)?; Iso::Both(Box::new(decode(&f)?), Box::new(decode(&g)?)) }
        _ => return None,
    })
}
pub fn decode_pat(t: &Term) -> Option<Pat> {
    Some(match t {
        Term::L => Pat::Any,
        Term::S(n) => Pat::Var(read_nat(n)? as u8),
        Term::F(k, a) => match read_nat(k)? {
            0 if **a == Term::L => Pat::L,
            1 => Pat::S(Box::new(decode_pat(a)?)),
            2 => { let Term::F(x, y) = &**a else { return None }; Pat::F(Box::new(decode_pat(x)?), Box::new(decode_pat(y)?)) }
            _ => return None,
        },
        Term::Ap(..) => return None,
    })
}

// ---- running on trees ------------------------------------------------------------------------------

pub fn show(t: &Term) -> String { rust_ca_lattice::sup::show(&t.into()) }

fn with_part(t: &Term, path: &[Dir], f: &mut dyn FnMut(Term) -> Result<Term, String>) -> Result<Term, String> {
    let Some((d, rest)) = path.split_first() else { return f(t.clone()) };
    let missing = || format!("no {} part in {}", show_path(&[*d]), show(t));
    match (d, t) {
        (Dir::Left, Term::F(a, b)) => Ok(fork(with_part(a, rest, f)?, (**b).clone())),
        (Dir::Right, Term::F(a, b)) => Ok(fork((**a).clone(), with_part(b, rest, f)?)),
        (Dir::Under, Term::S(a)) => Ok(stem(with_part(a, rest, f)?)),
        _ => Err(missing()),
    }
}

/// Run a program on a tree, directly; `fuel` counts down the machine's steps (about one an instruction).
pub fn run(defs: &Defs, i: &Iso, t: Term, fuel: &mut u64) -> Result<Term, String> {
    if *fuel == 0 { return Err("out of fuel".into()); }
    *fuel -= 1;
    match i {
        Iso::Seq(xs) => xs.iter().try_fold(t, |t, x| run(defs, x, t, fuel)),
        Iso::Re(l, r) => {
            let mut env = Default::default();
            if !l.bind(&t, &mut env) { return Err(format!("{} does not fit {}", show(&t), l.show())); }
            Ok(r.build(&mut env))
        }
        Iso::Dup(false) => Ok(fork(t.clone(), t)),
        Iso::Dup(true) => match &t { Term::F(a, b) if a == b => Ok((**a).clone()), _ => Err(format!("un-copy of unequal halves {}", show(&t))) },
        Iso::On(p, f) => with_part(&t, p, &mut |x| run(defs, f, x, fuel)),
        Iso::Case(cs) => {
            let k = cs.iter().position(|c| c.when.fits(&t)).ok_or_else(|| format!("no arm takes {}", show(&t)))?;
            let out = run(defs, &cs[k].body, t, fuel)?;
            if !cs[k].gives.fits(&out) || cs[..k].iter().any(|c| c.gives.fits(&out)) { return Err(format!("arm {k} gave {}, which goes back down another arm", show(&out))); }
            Ok(out)
        }
        Iso::Loop { from, body, until, again } => {
            if !from.fits(&t) { return Err(format!("{} does not start the loop", show(&t))); }
            let mut t = t;
            loop {
                t = run(defs, body, t, fuel)?;
                if until.fits(&t) { return Ok(t); }
                t = run(defs, again, t, fuel)?;
                if from.fits(&t) { return Err(format!("{} would start the loop again", show(&t))); }
            }
        }
        Iso::Call(d, v) => run(defs, defs.body(*d, *v), t, fuel),
        Iso::Eval(v) => {
            let Term::F(p, x) = &t else { return Err(format!("run needs a program and its input, not {}", show(&t))) };
            let prog = decode(p).ok_or_else(|| format!("{} is not a program", show(p)))?;
            let prog = if *v { prog.dagger() } else { prog };
            Ok(fork((**p).clone(), run(defs, &prog, (**x).clone(), fuel)?))
        }
        Iso::Forget => match &t { Term::F(a, _) => Ok((**a).clone()), _ => Err(format!("forget needs a pair, not {}", show(&t))) },
        Iso::Note(_, f) => run(defs, f, t, fuel),
        Iso::Both(f, g) => match &t {
            Term::F(a, b) => { let a = run(defs, f, (**a).clone(), fuel)?; Ok(fork(a, run(defs, g, (**b).clone(), fuel)?)) }
            _ => Err(format!("both needs a pair, not {}", show(&t))),
        },
    }
}

// ---- the machine ------------------------------------------------------------------------------------

/// An instruction of the program tree, as laid out: its parts are its children.
#[derive(Clone, Debug)]
pub enum Op {
    Seq,
    /// A note (its words are the label); runs its one part.
    Note,
    Re(Pat, Pat),
    Dup(bool),
    On(Vec<Dir>),
    /// Each arm's when and gives; the bodies are the children.
    Case(Vec<(Pat, Pat)>),
    /// From and until; the children are body and again.
    Loop(Pat, Pat),
    /// Laid out (one child, a copy of the definition) while the token is inside.
    Call(usize, bool),
    /// Laid out (one child, the program the data held) while the token is inside; the program waits parked.
    Eval(bool),
    Forget,
}

#[derive(Clone, Debug)]
pub struct PNode { pub op: Op, pub kids: Vec<u32>, pub up: Option<(u32, u32)>, pub park: Option<u32>, pub label: String }

/// A node of a tree the machine carries or parks: L, S, F, or the hole a part was taken out of.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DK { L, S, F, Hole }
/// `rc`: how many places hold the node. A copy is one more holder, not a new tree; a node is copied (one
/// layer, its children shared) only when something about to take it apart is not its only holder.
#[derive(Clone, Copy, Debug)]
pub struct DNode { pub kind: DK, pub kids: [u32; 2], pub rc: u32 }
const NONE: u32 = u32::MAX;

/// Which side of program node n the token is on: about to go in (Down) or just come out (Up).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Way { Down, Up }

/// A two-way program running on a tree: the program tree, the token's place on it, what it carries.
pub struct Machine {
    pub defs: Defs,
    pub prog: Vec<Option<PNode>>,
    pfree: Vec<u32>,
    pub data: Vec<Option<DNode>>,
    dfree: Vec<u32>,
    pub root: u32,
    pub at: (u32, Way),
    pub payload: u32,
    /// Whether the token is going forward in time.
    pub forward: bool,
    pub clock: i64,
    /// Definitions and programs laid out now.
    pub depth: usize,
    /// Trees thrown away by `Forget`.
    pub forgotten: usize,
    /// Copies share what they copy (else each copy is made node by node).
    pub share: bool,
}

impl Machine {
    pub fn new(defs: Defs, main: &Iso, input: &Term) -> Machine {
        let mut m = Machine { defs, prog: vec![], pfree: vec![], data: vec![], dfree: vec![], root: 0, at: (0, Way::Down), payload: NONE,
            forward: true, clock: 0, depth: 0, forgotten: 0, share: true };
        m.root = m.lay(main, None);
        m.at = (m.root, Way::Down);
        m.payload = m.plant(input);
        m
    }

    // -- the program tree
    pub fn pnode(&self, n: u32) -> &PNode { self.prog[n as usize].as_ref().expect("live instruction") }
    fn pmut(&mut self, n: u32) -> &mut PNode { self.prog[n as usize].as_mut().expect("live instruction") }
    fn lay(&mut self, i: &Iso, up: Option<(u32, u32)>) -> u32 {
        // One token runs a both's halves one after the other.
        if let Iso::Both(f, g) = i { return self.lay(&Iso::Seq(vec![Iso::On(vec![Dir::Left], f.clone()), Iso::On(vec![Dir::Right], g.clone())]), up); }
        let (op, kids): (Op, Vec<&Iso>) = match i {
            Iso::Seq(xs) => (Op::Seq, xs.iter().collect()),
            Iso::Re(l, r) => (Op::Re(l.clone(), r.clone()), vec![]),
            Iso::Dup(v) => (Op::Dup(*v), vec![]),
            Iso::On(p, f) => (Op::On(p.clone()), vec![&**f]),
            Iso::Case(cs) => (Op::Case(cs.iter().map(|c| (c.when.clone(), c.gives.clone())).collect()), cs.iter().map(|c| &c.body).collect()),
            Iso::Loop { from, body, until, again } => (Op::Loop(from.clone(), until.clone()), vec![&**body, &**again]),
            Iso::Call(d, v) => (Op::Call(*d, *v), vec![]),
            Iso::Eval(v) => (Op::Eval(*v), vec![]),
            Iso::Forget => (Op::Forget, vec![]),
            Iso::Note(_, f) => (Op::Note, vec![&**f]),
            Iso::Both(..) => unreachable!(),
        };
        let node = PNode { op, kids: vec![], up, park: None, label: i.label(&self.defs) };
        let n = match self.pfree.pop() { Some(k) => { self.prog[k as usize] = Some(node); k } None => { self.prog.push(Some(node)); self.prog.len() as u32 - 1 } };
        let kids: Vec<u32> = kids.into_iter().enumerate().map(|(k, x)| self.lay(x, Some((n, k as u32)))).collect();
        self.pmut(n).kids = kids;
        n
    }
    fn unlay(&mut self, n: u32) {
        let kids = std::mem::take(&mut self.pmut(n).kids);
        debug_assert!(self.pnode(n).park.is_none(), "taking away an instruction with a tree parked on it");
        for k in kids { self.unlay(k); }
        self.prog[n as usize] = None;
        self.pfree.push(n);
    }

    // -- trees
    pub fn dnode(&self, n: u32) -> DNode { self.data[n as usize].expect("live tree node") }
    fn dmk(&mut self, kind: DK, kids: [u32; 2]) -> u32 {
        let n = Some(DNode { kind, kids, rc: 1 });
        match self.dfree.pop() { Some(k) => { self.data[k as usize] = n; k } None => { self.data.push(n); self.data.len() as u32 - 1 } }
    }
    fn ddrop(&mut self, n: u32) { self.data[n as usize] = None; self.dfree.push(n); }
    fn hold(&mut self, n: u32) { if n != NONE { self.data[n as usize].as_mut().unwrap().rc += 1; } }
    /// One holder lets go of n; a node no one holds goes, and lets go of its children.
    fn release(&mut self, n: u32) {
        let d = self.data[n as usize].as_mut().unwrap();
        d.rc -= 1;
        if d.rc > 0 { return; }
        let kids = d.kids;
        self.ddrop(n);
        for k in kids { if k != NONE { self.release(k); } }
    }
    /// Take n apart: its children, now held by the caller. n goes if this was its only holder.
    fn consume(&mut self, n: u32) -> [u32; 2] {
        let d = self.dnode(n);
        if d.rc > 1 { self.data[n as usize].as_mut().unwrap().rc -= 1; for k in d.kids { self.hold(k); } } else { self.ddrop(n); }
        d.kids
    }
    /// n made the caller's alone, to change: a copy of its top layer if someone else holds it too.
    fn own(&mut self, n: u32) -> u32 {
        let d = self.dnode(n);
        if d.rc == 1 { return n; }
        self.data[n as usize].as_mut().unwrap().rc -= 1;
        for k in d.kids { self.hold(k); }
        self.dmk(d.kind, d.kids)
    }
    fn plant(&mut self, t: &Term) -> u32 {
        match t {
            Term::L => self.dmk(DK::L, [NONE; 2]),
            Term::S(a) => { let a = self.plant(a); self.dmk(DK::S, [a, NONE]) }
            Term::F(a, b) => { let a = self.plant(a); let b = self.plant(b); self.dmk(DK::F, [a, b]) }
            Term::Ap(..) => panic!("an application is not a tree"),
        }
    }
    pub fn tree(&self, n: u32) -> Term {
        let d = self.dnode(n);
        match d.kind { DK::L => Term::L, DK::S => stem(self.tree(d.kids[0])), DK::F => fork(self.tree(d.kids[0]), self.tree(d.kids[1])), DK::Hole => panic!("a hole in a tree being read") }
    }
    fn copy(&mut self, n: u32) -> u32 {
        let d = self.dnode(n);
        let kids = d.kids.map(|k| if k == NONE { NONE } else { self.copy(k) });
        self.dmk(d.kind, kids)
    }
    fn same(&self, a: u32, b: u32) -> bool {
        if a == b { return true; }
        let (x, y) = (self.dnode(a), self.dnode(b));
        x.kind == y.kind && (0..2).all(|q| (x.kids[q] == NONE) == (y.kids[q] == NONE) && (x.kids[q] == NONE || self.same(x.kids[q], y.kids[q])))
    }
    /// What the token carries.
    pub fn payload(&self) -> Term { self.tree(self.payload) }

    fn fits(&self, p: &Pat, n: u32) -> bool {
        let d = self.dnode(n);
        match p {
            Pat::Any | Pat::Var(_) => true,
            Pat::L => d.kind == DK::L,
            Pat::S(a) => d.kind == DK::S && self.fits(a, d.kids[0]),
            Pat::F(a, b) => d.kind == DK::F && self.fits(a, d.kids[0]) && self.fits(b, d.kids[1]),
        }
    }
    /// Take the payload apart by l and put it together by r; the nodes l spells out go, r's are made.
    fn rearrange(&mut self, l: &Pat, r: &Pat) -> Result<(), String> {
        if !self.fits(l, self.payload) { return Err(format!("{} does not fit {}", show(&self.payload()), l.show())); }
        let mut env = [NONE; 26];
        fn take(m: &mut Machine, p: &Pat, n: u32, env: &mut [u32; 26]) {
            match p {
                Pat::Var(v) => env[*v as usize] = n,
                Pat::S(a) => { let [k, _] = m.consume(n); take(m, a, k, env) }
                Pat::F(a, b) => { let [x, y] = m.consume(n); take(m, a, x, env); take(m, b, y, env) }
                Pat::L => { m.consume(n); }
                Pat::Any => unreachable!(),
            }
        }
        fn make(m: &mut Machine, p: &Pat, env: &[u32; 26]) -> u32 {
            match p {
                Pat::Var(v) => env[*v as usize],
                Pat::L => m.dmk(DK::L, [NONE; 2]),
                Pat::S(a) => { let a = make(m, a, env); m.dmk(DK::S, [a, NONE]) }
                Pat::F(a, b) => { let a = make(m, a, env); let b = make(m, b, env); m.dmk(DK::F, [a, b]) }
                Pat::Any => unreachable!(),
            }
        }
        take(self, l, self.payload, &mut env);
        self.payload = make(self, r, &env);
        Ok(())
    }
    fn dup(&mut self, back: bool) -> Result<(), String> {
        let p = self.payload;
        if !back {
            let c = if self.share { self.hold(p); p } else { self.copy(p) };
            self.payload = self.dmk(DK::F, [p, c]);
            return Ok(());
        }
        let d = self.dnode(p);
        if d.kind != DK::F || !self.same(d.kids[0], d.kids[1]) { return Err(format!("un-copy of unequal halves {}", show(&self.payload()))); }
        let [a, b] = self.consume(p);
        self.release(b);
        self.payload = a;
        Ok(())
    }
    /// Park the payload at n with its part at `path` taken out (a hole in its place); the token takes the part.
    fn split(&mut self, n: u32, path: &[Dir]) -> Result<(), String> {
        let q_of = |d: &Dir| if *d == Dir::Right { 1 } else { 0 };
        let mut at = self.payload;
        for (k, d) in path.iter().enumerate() {
            let x = self.dnode(at);
            if !matches!((d, x.kind), (Dir::Left, DK::F) | (Dir::Under, DK::S) | (Dir::Right, DK::F)) {
                return Err(format!("no {} part in {}", show_path(&path[..=k]), show(&self.payload())));
            }
            at = x.kids[q_of(d)];
        }
        // The path down is about to change: made this payload's alone.
        self.payload = self.own(self.payload);
        let mut at = self.payload;
        for d in &path[..path.len() - 1] {
            let c = self.own(self.dnode(at).kids[q_of(d)]);
            self.data[at as usize].as_mut().unwrap().kids[q_of(d)] = c;
            at = c;
        }
        let q = q_of(path.last().expect("a path"));
        let part = self.dnode(at).kids[q];
        let hole = self.dmk(DK::Hole, [NONE; 2]);
        self.data[at as usize].as_mut().unwrap().kids[q] = hole;
        self.pmut(n).park = Some(self.payload);
        self.payload = part;
        Ok(())
    }
    /// The token's part goes back into the tree parked at n, which the token takes again.
    fn join(&mut self, n: u32, path: &[Dir]) {
        let whole = self.pmut(n).park.take().expect("a tree parked here");
        let mut at = whole;
        for (k, d) in path.iter().enumerate() {
            let q = if *d == Dir::Right { 1 } else { 0 };
            if k + 1 == path.len() {
                let hole = self.dnode(at).kids[q];
                debug_assert_eq!(self.dnode(hole).kind, DK::Hole);
                self.ddrop(hole);
                self.data[at as usize].as_mut().unwrap().kids[q] = self.payload;
            } else { at = self.dnode(at).kids[q]; }
        }
        self.payload = whole;
    }
    /// Lay out what n runs: a definition's copy, or the program the payload holds (parked at n).
    fn open(&mut self, n: u32) -> Result<(), String> {
        let iso = match self.pnode(n).op.clone() {
            Op::Call(d, v) => self.defs.body(d, v).clone(),
            Op::Eval(v) => {
                let p = self.payload;
                let d = self.dnode(p);
                if d.kind != DK::F { return Err(format!("run needs a program and its input, not {}", show(&self.payload()))); }
                let prog = decode(&self.tree(d.kids[0])).ok_or_else(|| format!("{} is not a program", show(&self.tree(d.kids[0]))))?;
                let [code, x] = self.consume(p);
                self.pmut(n).park = Some(code);
                self.payload = x;
                if v { prog.dagger() } else { prog }
            }
            _ => unreachable!(),
        };
        let k = self.lay(&iso, Some((n, 0)));
        self.pmut(n).kids = vec![k];
        self.depth += 1;
        Ok(())
    }
    fn close(&mut self, n: u32) {
        let k = self.pnode(n).kids[0];
        self.unlay(k);
        self.pmut(n).kids.clear();
        self.depth -= 1;
        if let Op::Eval(_) = self.pnode(n).op {
            let p = self.pmut(n).park.take().expect("the program parked here");
            self.payload = self.dmk(DK::F, [p, self.payload]);
        }
    }
    fn forget(&mut self) -> Result<(), String> {
        let p = self.payload;
        let d = self.dnode(p);
        if d.kind != DK::F { return Err(format!("forget needs a pair, not {}", show(&self.payload()))); }
        let [a, t] = self.consume(p);
        self.release(t);
        self.payload = a;
        self.forgotten += 1;
        Ok(())
    }

    /// Whether the token has reached the end it is heading for.
    pub fn done(&self) -> bool { self.at == (self.root, if self.forward { Way::Up } else { Way::Down }) }

    /// Turn the token around: stepping then takes back the steps taken, one by one.
    pub fn flip(&mut self) { self.forward = !self.forward; }

    /// One step. Ok(false) if the token is at its end.
    pub fn step(&mut self) -> Result<bool, String> {
        if self.done() { return Ok(false); }
        if self.forward { self.fwd()? } else { self.bwd()? }
        self.clock += if self.forward { 1 } else { -1 };
        Ok(true)
    }
    pub fn run(&mut self, most: u64) -> Result<u64, String> {
        let mut n = 0;
        while n < most && self.step()? { n += 1; }
        Ok(n)
    }

    fn fwd(&mut self) -> Result<(), String> {
        use Way::*;
        let (n, way) = self.at;
        if way == Down {
            let kids = self.pnode(n).kids.clone();
            self.at = match self.pnode(n).op.clone() {
                Op::Seq | Op::Note => if kids.is_empty() { (n, Up) } else { (kids[0], Down) },
                Op::Re(l, r) => { self.rearrange(&l, &r)?; (n, Up) }
                Op::Dup(v) => { self.dup(v)?; (n, Up) }
                Op::On(p) => { self.split(n, &p)?; (kids[0], Down) }
                Op::Case(cs) => {
                    let k = cs.iter().position(|(w, _)| self.fits(w, self.payload)).ok_or_else(|| format!("no arm takes {}", show(&self.payload())))?;
                    (kids[k], Down)
                }
                Op::Loop(from, _) => { if !self.fits(&from, self.payload) { return Err(format!("{} does not start the loop", show(&self.payload()))); } (kids[0], Down) }
                Op::Call(..) | Op::Eval(_) => { self.open(n)?; (self.pnode(n).kids[0], Down) }
                Op::Forget => { self.forget()?; (n, Up) }
            };
        } else {
            let (p, k) = self.pnode(n).up.expect("not past the end");
            let kids = self.pnode(p).kids.clone();
            self.at = match self.pnode(p).op.clone() {
                Op::Seq | Op::Note => if (k as usize) + 1 < kids.len() { (kids[k as usize + 1], Down) } else { (p, Up) },
                Op::On(path) => { self.join(p, &path); (p, Up) }
                Op::Case(cs) => {
                    let g = &cs[k as usize].1;
                    if !self.fits(g, self.payload) || cs[..k as usize].iter().any(|(_, g)| self.fits(g, self.payload)) {
                        return Err(format!("arm {k} gave {}, which would go back down another arm", show(&self.payload())));
                    }
                    (p, Up)
                }
                Op::Loop(from, until) => {
                    if k == 0 { if self.fits(&until, self.payload) { (p, Up) } else { (kids[1], Down) } }
                    else { if self.fits(&from, self.payload) { return Err(format!("{} would start the loop again", show(&self.payload()))); } (kids[0], Down) }
                }
                Op::Call(..) | Op::Eval(_) => { self.close(p); (p, Up) }
                _ => unreachable!("only these have parts"),
            };
        }
        Ok(())
    }

    fn bwd(&mut self) -> Result<(), String> {
        use Way::*;
        let (n, way) = self.at;
        if way == Up {
            let kids = self.pnode(n).kids.clone();
            self.at = match self.pnode(n).op.clone() {
                Op::Seq | Op::Note => if kids.is_empty() { (n, Down) } else { (kids[kids.len() - 1], Up) },
                Op::Re(l, r) => { self.rearrange(&r, &l)?; (n, Down) }
                Op::Dup(v) => { self.dup(!v)?; (n, Down) }
                Op::On(p) => { self.split(n, &p)?; (kids[0], Up) }
                Op::Case(cs) => {
                    let k = cs.iter().position(|(_, g)| self.fits(g, self.payload)).ok_or_else(|| format!("no arm gives {}", show(&self.payload())))?;
                    (kids[k], Up)
                }
                Op::Loop(_, until) => { if !self.fits(&until, self.payload) { return Err(format!("{} does not end the loop", show(&self.payload()))); } (kids[0], Up) }
                Op::Call(..) | Op::Eval(_) => { self.open(n)?; (self.pnode(n).kids[0], Up) }
                Op::Forget => return Err("what forget threw away cannot come back".into()),
            };
        } else {
            let (p, k) = self.pnode(n).up.expect("not before the beginning");
            let kids = self.pnode(p).kids.clone();
            self.at = match self.pnode(p).op.clone() {
                Op::Seq | Op::Note => if k > 0 { (kids[k as usize - 1], Up) } else { (p, Down) },
                Op::On(path) => { self.join(p, &path); (p, Down) }
                Op::Case(cs) => {
                    let w = &cs[k as usize].0;
                    if !self.fits(w, self.payload) || cs[..k as usize].iter().any(|(w, _)| self.fits(w, self.payload)) {
                        return Err(format!("arm {k} came back to {}, which would have gone down another arm", show(&self.payload())));
                    }
                    (p, Down)
                }
                Op::Loop(from, until) => {
                    if k == 0 { if self.fits(&from, self.payload) { (p, Down) } else { (kids[1], Up) } }
                    else { if self.fits(&until, self.payload) { return Err(format!("{} would have ended the loop", show(&self.payload()))); } (kids[0], Up) }
                }
                Op::Call(..) | Op::Eval(_) => { self.close(p); (p, Down) }
                _ => unreachable!("only these have parts"),
            };
        }
        Ok(())
    }

    /// Instructions laid out now.
    pub fn live(&self) -> usize { self.prog.iter().flatten().count() }
    /// Tree nodes carried or parked now.
    pub fn nodes(&self) -> usize { self.data.iter().flatten().count() }
}

// ---- reading calls -----------------------------------------------------------------------------------

/// What to run, as the player's input box says it, and on what:
/// - `name args`: a library program (`name'` or `name⁻¹` its inverse) on one argument, or two as a fork;
/// - `tc f args`: the tree calculus, f applied to the arguments one after the other, with a trace
///   (`tck f args`: keeping f; `jet f args`: the same, but a program with a jet runs as its definition);
/// - `bennett f args`: the same with the trace cleaned up (the answer comes with f and its arguments);
/// - `forget f args`: the same with the trace thrown away at the end;
/// - `bijection f g x`: f a bijection and g its inverse, f x with nothing left over (and f, g handed back);
/// - `run name args`: a library program handed over as a tree and run from there;
/// - `dagger name`: a library program's inverse, worked out as a tree by `dagger`;
/// - `both f g x y`: library programs f on x and g on y, at once.
/// Arguments are numbers, lists of them, trees written L, S(…), F(…, …), or `#` and ternary digits
/// (disp's programs as the player has them).
pub fn parse_call(defs: &Defs, src: &str) -> Result<(Iso, Term), String> {
    let src = src.trim();
    let (word, rest) = src.split_once(char::is_whitespace).unwrap_or((src, ""));
    let args = |s: &str| literals(s);
    let one_or_two = |vals: Vec<Term>| match vals.len() { 1 => Ok(vals[0].clone()), 2 => Ok(fork(vals[0].clone(), vals[1].clone())), n => Err(format!("one or two arguments, not {n}")) };
    match word {
        "tck" | "jet" => {
            let vals = args(rest)?;
            let (f, xs) = vals.split_first().ok_or("tck needs a program and its arguments")?;
            if xs.is_empty() { return Err("tck needs at least one argument".into()); }
            let main = if word == "jet" { jet_apply(defs, xs.len()) } else { tck_apply(defs, xs.len()) };
            Ok((main, fork(xs.iter().fold(f.clone(), |a, x| fork(a, x.clone())), Term::L)))
        }
        "tc" | "bennett" | "forget" => {
            let vals = args(rest)?;
            let (f, xs) = vals.split_first().ok_or("tc needs a program and its arguments")?;
            if xs.is_empty() { return Err("tc needs at least one argument".into()); }
            let app = xs.iter().fold(f.clone(), |a, x| fork(a, x.clone()));
            let main = tc_apply(defs, xs.len());
            Ok(match word {
                "tc" => (main, fork(app, Term::L)),
                "bennett" => (bennett(main), app),
                _ => (Iso::Seq(vec![main, Iso::Forget]), fork(app, Term::L)),
            })
        }
        "bijection" => {
            let vals = args(rest)?;
            let [f, g, x] = vals.as_slice() else { return Err("bijection takes a program, its inverse and an argument".into()) };
            Ok((bijection(defs), fork(fork(f.clone(), g.clone()), x.clone())))
        }
        "run" => {
            let (name, rest) = rest.trim().split_once(char::is_whitespace).unwrap_or((rest.trim(), ""));
            let (prog, inv) = lookup(defs, name)?;
            let p = encode(&if inv { defs.body(prog, true).clone() } else { defs.body(prog, false).clone() });
            Ok((Iso::Eval(false), fork(p, one_or_two(args(rest)?)?)))
        }
        "both" => {
            let mut words = rest.trim().splitn(3, char::is_whitespace);
            let (f, g) = (lookup(defs, words.next().unwrap_or(""))?, lookup(defs, words.next().unwrap_or(""))?);
            let [x, y] = <[Term; 2]>::try_from(args(words.next().unwrap_or(""))?).map_err(|_| "both takes two programs and an argument for each")?;
            Ok((Iso::Both(Box::new(Iso::Call(f.0, f.1)), Box::new(Iso::Call(g.0, g.1))), fork(x, y)))
        }
        "dagger" => {
            let (prog, inv) = lookup(defs, rest.trim())?;
            Ok((Iso::Call(defs.find("dagger").unwrap(), false), encode(defs.body(prog, inv))))
        }
        _ => {
            let (d, inv) = lookup(defs, word)?;
            Ok((Iso::Call(d, inv), one_or_two(args(rest)?)?))
        }
    }
}
fn lookup(defs: &Defs, name: &str) -> Result<(usize, bool), String> {
    let (name, inv) = match name.strip_suffix('\'').or_else(|| name.strip_suffix("⁻¹")) { Some(n) => (n, true), None => (name, false) };
    Ok((defs.find(name).ok_or_else(|| format!("no two-way program {name} (there are {})", defs.names.join(", ")))?, inv))
}

/// Disp literals: numbers (n forks over a leaf), lists of them, trees written L, S(…), F(…, …), and
/// `#` then ternary digits.
fn literals(src: &str) -> Result<Vec<Term>, String> {
    let b: Vec<char> = src.chars().collect();
    let mut i = 0;
    let mut out = vec![];
    fn skip(b: &[char], i: &mut usize) { while *i < b.len() && (b[*i].is_whitespace() || b[*i] == ',') { *i += 1; } }
    fn value(b: &[char], i: &mut usize) -> Result<Term, String> {
        skip(b, i);
        match b.get(*i) {
            Some('[') => {
                *i += 1;
                let mut items = vec![];
                loop { skip(b, i); if b.get(*i) == Some(&']') { *i += 1; break; } if *i >= b.len() { return Err("a [ is never closed".into()); } items.push(value(b, i)?); }
                Ok(list(items))
            }
            Some('#') => {
                *i += 1;
                let s = *i;
                while *i < b.len() && matches!(b[*i], '0' | '1' | '2') { *i += 1; }
                rust_ic_mesh::term::ternary(&b[s..*i].iter().collect::<String>())
            }
            Some(c) if c.is_ascii_digit() => {
                let s = *i;
                while *i < b.len() && b[*i].is_ascii_digit() { *i += 1; }
                Ok(nat(b[s..*i].iter().collect::<String>().parse().map_err(|_| "a number too big")?))
            }
            Some('L' | 'S' | 'F') => {
                let s = *i;
                *i += 1;
                if b[s] != 'L' {
                    let mut depth = 0;
                    while *i < b.len() {
                        match b[*i] { '(' => depth += 1, ')' => depth -= 1, _ => {} }
                        *i += 1;
                        if depth == 0 { break; }
                    }
                }
                rust_ic_mesh::term::parse(&b[s..*i].iter().collect::<String>())
            }
            _ => Err(format!("can't read {:?}", b[*i..].iter().take(10).collect::<String>())),
        }
    }
    loop { skip(&b, &mut i); if i >= b.len() { break; } out.push(value(&b, &mut i)?); }
    Ok(out)
}
