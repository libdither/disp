//! Keeping history on the strand lattice (`Params::resid`, rules.rs `RESID_RULES`), as a Python model
//! of the net had it (research/reversible-core/residual.py, in git at 8a287ab). At rest the lattice
//! holds one tree at its root:
//! the term (each value v as △v, each application f x as △⌜f⌝⌜x⌝), or the answer and the history
//! bundled in a fork. A click turns the root into `Go`, which runs the term (the walker `Ev` makes an
//! apply of each application, each leaving what it would have dropped), or into `Back`, which runs the
//! answer and history back to the term (`UEv`). The term loads with `Go` at its root.
//!
//! With forgetting on (`Lattice::forget`), what the applies drop is erased as they go and holes are
//! left: run back, the term comes back with holes just where nothing in the run looked. With the
//! history erased instead (`resid=2`), each residual goes to an eraser as it is made and the answer to
//! `Out`, and nothing runs back.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::Term;
use rust_ca_lattice::rules::Tag;
use std::rc::Rc;

/// A tree that may have holes: what the root holds once a run back is over.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Tree { L, S(Box<Tree>), F(Box<Tree>, Box<Tree>), Hole }

impl Tree {
    pub fn from(t: &Term) -> Tree {
        match t {
            Term::L => Tree::L,
            Term::S(a) => Tree::S(Box::new(Tree::from(a))),
            Term::F(a, b) => Tree::F(Box::new(Tree::from(a)), Box::new(Tree::from(b))),
            Term::Ap(..) => unreachable!("a tree holds no application"),
        }
    }

    pub fn show(&self) -> String {
        match self {
            Tree::L => "L".into(),
            Tree::S(a) => format!("S({})", a.show()),
            Tree::F(a, b) => format!("F({},{})", a.show(), b.show()),
            Tree::Hole => "?".into(),
        }
    }

    pub fn holes(&self) -> usize {
        match self { Tree::L => 0, Tree::S(a) => a.holes(), Tree::F(a, b) => a.holes() + b.holes(), Tree::Hole => 1 }
    }

    /// Whether this is `full` with some of its subtrees holes.
    pub fn within(&self, full: &Tree) -> bool {
        match (self, full) {
            (Tree::Hole, _) | (Tree::L, Tree::L) => true,
            (Tree::S(a), Tree::S(b)) => a.within(b),
            (Tree::F(a, b), Tree::F(c, d)) => a.within(c) && b.within(d),
            _ => false,
        }
    }

    /// The term this tree stands for, each hole filled by `hole`.
    pub fn unquote(&self, hole: &Term) -> Term {
        match self {
            Tree::S(v) => v.filled(hole),
            Tree::F(f, x) => Term::Ap(Rc::new(f.unquote(hole)), Rc::new(x.unquote(hole))),
            _ => unreachable!("not a term as a tree"),
        }
    }

    fn filled(&self, hole: &Term) -> Term {
        match self {
            Tree::L => Term::L,
            Tree::S(a) => Term::S(Rc::new(a.filled(hole))),
            Tree::F(a, b) => Term::F(Rc::new(a.filled(hole)), Rc::new(b.filled(hole))),
            Tree::Hole => hole.clone(),
        }
    }

    /// The term's values as this tree has them, in order.
    pub fn values(&self) -> Vec<&Tree> {
        match self { Tree::S(v) => vec![&**v], Tree::F(f, x) => [f.values(), x.values()].concat(), _ => vec![] }
    }
}

/// A click, at a clock (`Lattice::edits`): the root turned into `Back` or `Go`, forgetting on or off.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Edit { Back, Go, Forget(bool) }

/// The term as loaded, as a tree, to compare what comes back with.
#[derive(Clone, Debug)]
pub struct Parts { pub term: Tree }

impl Default for Parts { fn default() -> Self { Parts { term: Tree::L } } }

fn plain(t: &Term) -> bool {
    match t { Term::L => true, Term::S(a) => plain(a), Term::F(a, b) => plain(a) && plain(b), Term::Ap(..) => false }
}

/// t as a tree: a value v is △v, an application f x is △⌜f⌝⌜x⌝, and a stem or fork that holds
/// applications is △ applied to its children.
pub fn quote(t: &Term) -> Term {
    let ap = |f: Term, x: Term| Term::Ap(Rc::new(f), Rc::new(x));
    match t {
        t if plain(t) => Term::S(Rc::new(t.clone())),
        Term::Ap(f, x) => Term::F(Rc::new(quote(f)), Rc::new(quote(x))),
        Term::S(c) => quote(&ap(Term::L, (**c).clone())),
        Term::F(a, b) => quote(&ap(ap(Term::L, (**a).clone()), (**b).clone())),
        Term::L => unreachable!("a leaf is plain"),
    }
}

/// The net for t, with the history kept (the term as a tree under `Go`) or erased as it is made.
/// Returns the net, the root to lay it out from, and the term as loaded.
pub fn net(t: &Term, keep: bool) -> (Net, u32, Parts) {
    let mut net = Net::new();
    if keep {
        let q = quote(t);
        let top = net.build(&q);
        let go = net.mk(Tag::Go);
        net.link(go, 0, top, 0);
        return (net, go, Parts { term: Tree::from(&q) });
    }
    let at = erasing(&mut net, t);
    let (nrm, out) = (net.mk(Tag::Nrm), net.mk(Tag::Out));
    net.link(nrm, 0, at.0, at.1);
    net.link(nrm, 1, out, 0);
    (net, out, Parts::default())
}

/// t built with every application an apply whose residual goes to an eraser; the port giving its value.
fn erasing(net: &mut Net, t: &Term) -> (u32, u8) {
    if plain(t) { return (net.build(t), 0); }
    let kid = |net: &mut Net, t: &Term, into: (u32, u8)| { let a = erasing(net, t); net.link(into.0, into.1, a.0, a.1); };
    match t {
        Term::Ap(f, x) => {
            let (ra, e) = (net.mk(Tag::RA), net.mk(Tag::Eps));
            kid(net, f, (ra, 0));
            kid(net, x, (ra, 1));
            net.link(ra, 3, e, 0);
            (ra, 2)
        }
        Term::S(c) => { let s = net.mk(Tag::S); kid(net, c, (s, 1)); (s, 0) }
        Term::F(l, r) => { let f = net.mk(Tag::F); kid(net, l, (f, 1)); kid(net, r, (f, 2)); (f, 0) }
        Term::L => unreachable!("a leaf is plain"),
    }
}
