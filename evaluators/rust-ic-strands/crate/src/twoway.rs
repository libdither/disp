//! Two-way programs on the strand lattice (`Params::two`, rules.rs `TWO_RULES`): a program is a tree, each
//! of its instructions a bijection on trees, and `Run` runs it on a tree, reading it a node at a time and
//! handing it back beside the result. Running back is running the inverse program (`dagger`) forward.
//!
//! An instruction is k stems on a leaf (one with no parts) or on a fork of its two parts (the table is at
//! rules.rs `TWO_RULES`): a leaf is the identity, a fork (a, b) runs a then b, a stem on a fork runs a and b
//! side by side, and so on. Programs being trees, a program can build, take apart (`triage`), copy and
//! compare programs as it does any tree, and run one it made (`eval`).
//!
//! A choice is a sum, as in James and Sabry's Π: (△, v) its left side, (△△, v) its right. `cond` is Π's
//! ⊕ (the side kept, a program for each), `flip` its swap, and `loop` its trace: x goes in as (△△, x),
//! the body runs while it gives a left side (△, s), and its right side (△△, y) gives y.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::Term;
use rust_ca_lattice::rules::Tag;
use std::rc::Rc;

/// A two-way program as written; `code` is the tree that runs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Prog {
    Id, Swap, Flip, AssocL, AssocR, Uniti, Unite, Copy, Merge, Eval, Uneval, Triage, Untriage, StemFork, ForkStem,
    Seq(Rc<Prog>, Rc<Prog>), Par(Rc<Prog>, Rc<Prog>), Cond(Rc<Prog>, Rc<Prog>), Loop(Rc<Prog>),
}

/// The instructions with no parts, each as many stems on a leaf as its place here.
const STEPS: [Prog; 15] = [
    Prog::Id, Prog::Swap, Prog::Flip, Prog::AssocL, Prog::AssocR, Prog::Uniti, Prog::Unite, Prog::Copy, Prog::Merge,
    Prog::Eval, Prog::Uneval, Prog::Triage, Prog::Untriage, Prog::StemFork, Prog::ForkStem,
];

/// A tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Val { L, S(Rc<Val>), F(Rc<Val>, Rc<Val>) }

pub fn seq(a: Prog, b: Prog) -> Prog { Prog::Seq(Rc::new(a), Rc::new(b)) }
pub fn par(a: Prog, b: Prog) -> Prog { Prog::Par(Rc::new(a), Rc::new(b)) }
pub fn cond(a: Prog, b: Prog) -> Prog { Prog::Cond(Rc::new(a), Rc::new(b)) }
pub fn lp(c: Prog) -> Prog { Prog::Loop(Rc::new(c)) }
/// Steps one after another, as a balanced tree of `Seq`, so that the steps unfold in log many rounds.
pub fn seqs(ps: impl IntoIterator<Item = Prog>) -> Prog {
    fn half(v: &[Prog]) -> Prog {
        match v.len() { 0 => Prog::Id, 1 => v[0].clone(), n => seq(half(&v[..n / 2]), half(&v[n / 2..])) }
    }
    half(&ps.into_iter().collect::<Vec<_>>())
}
/// The left half by c.
pub fn first(c: Prog) -> Prog { par(c, Prog::Id) }
/// The right half by c.
pub fn second(c: Prog) -> Prog { par(Prog::Id, c) }

impl Val {
    pub fn f(a: Val, b: Val) -> Val { Val::F(Rc::new(a), Rc::new(b)) }
    pub fn s(a: Val) -> Val { Val::S(Rc::new(a)) }
    /// The left side of a choice holding v.
    pub fn inl(v: Val) -> Val { Val::f(Val::L, v) }
    /// The right side.
    pub fn inr(v: Val) -> Val { Val::f(Val::s(Val::L), v) }
    /// n stems on v.
    pub fn stems(n: usize, v: Val) -> Val { (0..n).fold(v, |t, _| Val::s(t)) }
    pub fn from(t: &Term) -> Val {
        match t { Term::L => Val::L, Term::S(a) => Val::s(Val::from(a)), Term::F(a, b) => Val::f(Val::from(a), Val::from(b)), Term::Ap(..) => unreachable!("not a tree") }
    }
    pub fn show(&self) -> String {
        match self { Val::L => "△".into(), Val::S(a) => format!("△{}", a.show()), Val::F(a, b) => format!("({}, {})", a.show(), b.show()) }
    }
    pub fn size(&self) -> usize {
        match self { Val::L => 1, Val::S(a) => 1 + a.size(), Val::F(a, b) => 1 + a.size() + b.size() }
    }
    /// An instruction: how many stems, and the leaf or fork under them.
    fn instruction(&self) -> (usize, &Val) {
        let (mut k, mut v) = (0, self);
        while let Val::S(a) = v { k += 1; v = a; }
        (k, v)
    }
    fn fork(&self) -> Option<(Val, Val)> { match self { Val::F(a, b) => Some(((**a).clone(), (**b).clone())), _ => None } }
}

impl Prog {
    /// The tree that runs: an instruction with no parts as that many stems on a leaf (`STEPS`), `a; b` as
    /// the fork (a, b), `(a | b)` a stem on it, `cond(a, b)` two, and `loop(a)` three on (a, △).
    pub fn code(&self) -> Val {
        use Prog::*;
        match self {
            Seq(a, b) => Val::f(a.code(), b.code()),
            Par(a, b) => Val::stems(1, Val::f(a.code(), b.code())),
            Cond(a, b) => Val::stems(2, Val::f(a.code(), b.code())),
            Loop(c) => Val::stems(3, Val::f(c.code(), Val::L)),
            step => Val::stems(STEPS.iter().position(|s| s == step).unwrap(), Val::L),
        }
    }

    /// The program a tree is, if it is one through and through (a loop's second part is not read).
    pub fn of(v: &Val) -> Option<Prog> {
        match v.instruction() {
            (k, Val::F(a, b)) => {
                let (a, b) = (Prog::of(a), || Prog::of(b));
                Some(match k { 0 => seq(a?, b()?), 1 => par(a?, b()?), 2 => cond(a?, b()?), 3 => lp(a?), _ => return None })
            }
            (k, _) => STEPS.get(k).cloned(),
        }
    }

    /// The inverse, read off the text.
    pub fn dagger(&self) -> Prog {
        use Prog::*;
        match self {
            AssocL => AssocR, AssocR => AssocL, StemFork => ForkStem, ForkStem => StemFork, Uniti => Unite, Unite => Uniti,
            Copy => Merge, Merge => Copy, Eval => Uneval, Uneval => Eval, Triage => Untriage, Untriage => Triage,
            Seq(a, b) => seq(b.dagger(), a.dagger()),
            Par(a, b) => par(a.dagger(), b.dagger()),
            Cond(a, b) => cond(a.dagger(), b.dagger()),
            Loop(c) => lp(c.dagger()),
            step => step.clone(),
        }
    }

    /// What the program gives for x (`run`).
    pub fn run(&self, x: &Val, fuel: &mut u64) -> Option<Val> { run(&self.code(), x, fuel) }

    pub fn size(&self) -> usize { self.code().size() }

    pub fn show(&self) -> String {
        use Prog::*;
        match self {
            Id => "id".into(), Swap => "swap".into(), Flip => "flip".into(), AssocL => "assocl".into(), AssocR => "assocr".into(),
            Uniti => "uniti".into(), Unite => "unite".into(), Copy => "copy".into(), Merge => "merge".into(), Eval => "eval".into(),
            Uneval => "uneval".into(), Triage => "triage".into(), Untriage => "untriage".into(), StemFork => "s2f".into(),
            ForkStem => "f2s".into(),
            Seq(a, b) => format!("{}; {}", a.show(), b.show()),
            Par(a, b) => format!("({} | {})", a.show(), b.show()),
            Cond(a, b) => format!("cond({}, {})", a.show(), b.show()),
            Loop(c) => format!("loop({})", c.show()),
        }
    }
}

/// What the program p (a tree) gives for x, reading p a node at a time as `Run` does: None where it is
/// stuck (no rule fits, p is no program there) or runs past `fuel` rounds of a loop.
pub fn run(p: &Val, x: &Val, fuel: &mut u64) -> Option<Val> {
    use Prog::*;
    use Val as V;
    let (k, end) = p.instruction();
    if let V::F(a, b) = end {
        return match k {
            0 => run(b, &run(a, x, fuel)?, fuel),
            1 => { let (u, v) = x.fork()?; Some(V::f(run(a, &u, fuel)?, run(b, &v, fuel)?)) }
            2 => match x.fork()? { (V::L, y) => Some(V::f(V::L, run(a, &y, fuel)?)), (t, y) => Some(V::f(t, run(b, &y, fuel)?)) },
            3 => {
                let mut s = V::inr(x.clone());
                loop {
                    if *fuel == 0 { return None; }
                    *fuel -= 1;
                    s = run(a, &s, fuel)?;
                    match s.fork()? {
                        (V::L, _) => {}
                        (V::S(z), y) if *z == V::L => break Some(y),
                        _ => return None,
                    }
                }
            }
            _ => None,
        };
    }
    let leaf_and = |x: &Val| match x.fork()? { (V::L, t) => Some(t), _ => None };
    Some(match STEPS.get(k)? {
        Id => x.clone(),
        Swap => { let (u, v) = x.fork()?; V::f(v, u) }
        Flip => match x.fork()? { (V::L, v) => V::inr(v), (V::S(z), v) if *z == V::L => V::inl(v), _ => return None },
        AssocL => { let (u, vw) = x.fork()?; let (v, w) = vw.fork()?; V::f(V::f(u, v), w) }
        AssocR => { let (uv, w) = x.fork()?; let (u, v) = uv.fork()?; V::f(u, V::f(v, w)) }
        Uniti => V::inl(x.clone()),
        Unite => leaf_and(x)?,
        Copy => V::f(x.clone(), x.clone()),
        Merge => { let (a, b) = x.fork()?; if a == b { a } else { return None } }
        Eval => { let (q, z) = x.fork()?; let y = run(&q, &z, fuel)?; V::f(q, y) }
        Uneval => { let (q, w) = x.fork()?; let y = run(&dagger(&q)?, &w, fuel)?; V::f(q, y) }
        Triage => match x {
            V::L => V::inl(V::inl(V::L)),
            V::S(t) => V::inl(V::inr((**t).clone())),
            V::F(..) => V::inr(x.clone()),
        },
        Untriage => match x.fork()? {
            (V::L, y) => match y.fork()? { (V::L, V::L) => V::L, (V::S(z), t) if *z == V::L => V::s(t), _ => return None },
            (V::S(z), y @ V::F(..)) if *z == V::L => y,
            _ => return None,
        },
        StemFork => match x { V::S(t) => V::inl((**t).clone()), _ => return None },
        ForkStem => V::s(leaf_and(x)?),
        _ => unreachable!("an instruction with parts"),
    })
}

/// The inverse of a program given as a tree, made as `Dg` makes it, a node at a time: None where it is
/// stuck (a fork under more than 3 stems). A loop's second part is kept as it is.
pub fn dagger(p: &Val) -> Option<Val> {
    let (k, end) = p.instruction();
    Some(match end {
        Val::F(a, b) => match k {
            0 => Val::f(dagger(b)?, dagger(a)?),
            1 | 2 => Val::stems(k, Val::f(dagger(a)?, dagger(b)?)),
            3 => Val::stems(3, Val::f(dagger(a)?, (**b).clone())),
            _ => return None,
        },
        _ => Val::stems(if k < 3 { k } else if k % 2 == 1 { k + 1 } else { k - 1 }, Val::L),
    })
}

impl Val {
    pub fn build(&self, net: &mut Net) -> u32 {
        match self {
            Val::L => net.mk(Tag::L),
            Val::S(a) => { let n = net.mk(Tag::S); let x = a.build(net); net.link(n, 1, x, 0); n }
            Val::F(a, b) => { let n = net.mk(Tag::F); let (x, y) = (a.build(net), b.build(net)); net.link(n, 1, x, 0); net.link(n, 2, y, 0); n }
        }
    }
}

/// The net running p on x: `Run` [p, x, program out, result], its two outputs bundled in a fork at the
/// root. Returns the net and the root.
pub fn net(p: &Prog, x: &Val) -> (Net, u32) {
    let mut net = Net::new();
    let run = net.mk(Tag::Run);
    let (pr, xv) = (p.code().build(&mut net), x.build(&mut net));
    net.link(run, 0, pr, 0);
    net.link(run, 1, xv, 0);
    let (bundle, out) = (net.mk(Tag::F), net.mk(Tag::Out));
    net.link(run, 2, bundle, 1);
    net.link(run, 3, bundle, 2);
    net.link(bundle, 0, out, 0);
    (net, out)
}

/// What a net holds at the agent `id`'s output, if it is a tree through and through.
pub fn read_val(net: &Net, id: u32) -> Option<Val> {
    let a = net.agents.get(id as usize)?.as_ref()?;
    let kid = |q: usize| -> Option<Val> { let (k, p) = a.ports[q]?; if p != 0 { return None; } read_val(net, k) };
    Some(match a.tag {
        Tag::L => Val::L,
        Tag::S => Val::s(kid(1)?),
        Tag::F => Val::f(kid(1)?, kid(2)?),
        _ => return None,
    })
}

/// The program handed back and the result, once the root's bundle holds both whole.
pub fn read(net: &Net, out: u32) -> Option<(Val, Val)> {
    let (b, 0) = net.agents.get(out as usize)?.as_ref()?.ports[0]? else { return None };
    match read_val(net, b)? { Val::F(p, y) => Some(((*p).clone(), (*y).clone())), _ => None }
}

/// The program handed back and the result, once both are whole at the root (the `Out` agent).
pub fn answer(net: &Net) -> Option<(Val, Val)> {
    let out = net.agents.iter().position(|a| a.as_ref().is_some_and(|a| a.tag == Tag::Out))?;
    read(net, out as u32)
}

/// Agents a finished run should not leave: anything but trees and the root. A program stuck where it has
/// no rule (a swap of a leaf, a merge of two different halves) leaves its walker here, though the rest of
/// its result may still reach the root.
pub fn leftovers(net: &Net) -> Vec<Tag> {
    net.agents.iter().flatten().map(|a| a.tag).filter(|&t| !matches!(t, Tag::L | Tag::S | Tag::F | Tag::Out)).collect()
}

// ---- a library ----------------------------------------------------------------------------------

/// n as Π's μx. 1 + x: zero is the left side (△, △), n + 1 the right side (△△, n).
pub fn nat(n: usize) -> Val { (0..n).fold(Val::inl(Val::L), |t, _| Val::inr(t)) }

/// x → (△△, x): n + 1.
pub fn succ() -> Prog { seq(Prog::Uniti, Prog::Flip) }

/// (a, b) → (a, a + b), a loop over a carrying (todo, (done, acc)). Its body: the way in (a, b) becomes
/// (a, (0, b)); the two ways into the loop's state are told apart by done (0 only on the way in), so
/// the side they came by is dropped by merging it into done's own side; then a todo of n + 1 steps to
/// (n, (done + 1, acc + 1)) and goes round, and a todo of 0 leaves with (done, acc).
pub fn add() -> Prog {
    use Prog::*;
    let entry = second(seqs([Uniti, Uniti, AssocL]));
    let drop_side = seqs([
        AssocL, first(Swap), AssocR,
        second(seqs([AssocL, first(seqs([AssocL, first(seq(Flip, Merge))]))])),
    ]);
    let step = seqs([AssocR, cond(Unite, second(par(succ(), succ()))), Flip]);
    lp(seqs([cond(Id, entry), drop_side, step]))
}

/// (p, x) → (p, p (p x)): the program p; p is two copies of p, as a fork, run and merged back into one.
pub fn twice() -> Prog { seqs([first(Prog::Copy), Prog::Eval, first(Prog::Merge)]) }

/// (f, x) → (f, y) where f gives (f, y) for (f, x): f run on itself and x, so that it can call itself
/// the same way. Its copy, run, is merged back into it.
pub fn call() -> Prog { seqs([first(Prog::Copy), Prog::AssocR, Prog::Eval, Prog::AssocL, first(Prog::Merge)]) }

/// (f, (t, y)) ↔ (t, (f, y)): a tag brought out to choose by, and put back.
fn tag_out() -> Prog { seqs([Prog::AssocL, first(Prog::Swap), Prog::AssocR]) }

/// ((f, g), (a, b)) ↔ ((f, a), (g, b)).
fn transpose() -> Prog {
    use Prog::*;
    seqs([AssocR, second(AssocL), second(first(Swap)), second(AssocR), AssocL])
}

/// The body of mirror, which swaps the two parts of every fork in a tree: (m, x) → (m, x mirrored), m
/// being this program, so that `call` on (m, x) runs it (`mirror X` as text). It tells a leaf, stem and
/// fork apart by `triage`, calls itself on a stem's child, and on a fork's two parts at once with m
/// copied, merging the copies back. Run on a program, it gives another program.
pub fn mirror() -> Prog {
    use Prog::*;
    let stem = seqs([tag_out(), cond(Id, call()), tag_out()]);
    let fork = seqs([first(Copy), transpose(), par(call(), call()), transpose(), first(Merge), second(Swap)]);
    seqs([second(Triage), tag_out(), cond(stem, fork), tag_out(), second(Untriage)])
}

// ---- as text ------------------------------------------------------------------------------------

/// A run as typed: `PROG @ VALUE`, or a library call `add A B` (`sub A C` its inverse, `succ N`, `pred N`), numbers
/// being Π's (`nat`), or `mirror VALUE` (`call @ ([mirror], VALUE)`). PROG is as `Prog::show` writes it: the
/// instructions by name, `a; b`, `(a | b)`, `cond(a, b)`, `loop(a)`, `~a` for a's inverse and the library's
/// `succ`, `pred`, `add`, `sub`, `twice`, `call` and `mirror`. VALUE: `△` (or `t`) a leaf, `△v` a stem,
/// `(u, v)` a fork, a number, `[PROG]` a program, as the tree it is.
pub fn parse_run(src: &str) -> Result<(Prog, Val), String> {
    let words: Vec<&str> = src.split_whitespace().collect();
    let num = |w: &str| w.parse::<usize>().map_err(|_| format!("{w}: not a number"));
    match words[..] {
        ["add", a, b] => return Ok((add(), Val::f(nat(num(a)?), nat(num(b)?)))),
        ["sub", a, c] => return Ok((add().dagger(), Val::f(nat(num(a)?), nat(num(c)?)))),
        ["succ", n] => return Ok((succ(), nat(num(n)?))),
        ["pred", n] => return Ok((succ().dagger(), nat(num(n)?))),
        _ => {}
    }
    let value = |s: &str| -> Result<Val, String> {
        let mut r = Reader { s: s.chars().filter(|c| !c.is_whitespace()).collect(), i: 0 };
        let v = r.val()?;
        r.end()?;
        Ok(v)
    };
    if let Some(x) = src.trim().strip_prefix("mirror ") { return Ok((call(), Val::f(mirror().code(), value(x)?))); }
    let (p, x) = src.split_once('@').ok_or("PROG @ VALUE, or add A B, sub A C, succ N, pred N, mirror VALUE")?;
    let mut r = Reader { s: p.chars().filter(|c| !c.is_whitespace()).collect(), i: 0 };
    let prog = r.prog()?;
    r.end()?;
    Ok((prog, value(x)?))
}

struct Reader { s: Vec<char>, i: usize }

impl Reader {
    fn peek(&self) -> Option<char> { self.s.get(self.i).copied() }
    fn eat(&mut self, c: char) -> bool { let y = self.peek() == Some(c); if y { self.i += 1; } y }
    fn want(&mut self, c: char) -> Result<(), String> { if self.eat(c) { Ok(()) } else { Err(format!("expected {c} at {}", self.i)) } }
    fn end(&self) -> Result<(), String> { if self.i == self.s.len() { Ok(()) } else { Err(format!("unexpected {} at {}", self.s[self.i], self.i)) } }
    fn name(&mut self) -> String {
        let from = self.i;
        while self.peek().is_some_and(|c| c.is_ascii_alphanumeric()) { self.i += 1; }
        self.s[from..self.i].iter().collect()
    }
    /// Steps one after another, `a; b; c`.
    fn prog(&mut self) -> Result<Prog, String> {
        let a = self.step()?;
        if self.eat(';') { Ok(seq(a, self.prog()?)) } else { Ok(a) }
    }
    fn step(&mut self) -> Result<Prog, String> {
        if self.eat('~') { return Ok(self.step()?.dagger()); }
        if self.eat('(') {
            let a = self.prog()?;
            let p = if self.eat('|') { par(a, self.prog()?) } else { a };
            self.want(')')?;
            return Ok(p);
        }
        let n = self.name();
        let two = |r: &mut Reader| -> Result<(Prog, Prog), String> { r.want('(')?; let a = r.prog()?; r.want(',')?; let b = r.prog()?; r.want(')')?; Ok((a, b)) };
        Ok(match n.as_str() {
            "cond" => { let (a, b) = two(self)?; cond(a, b) }
            "loop" => { self.want('(')?; let c = self.prog()?; self.want(')')?; lp(c) }
            "succ" => succ(), "pred" => succ().dagger(), "add" => add(), "sub" => add().dagger(),
            "twice" => twice(), "call" => call(), "mirror" => mirror(),
            "" => return Err(format!("expected a program at {}", self.i)),
            w => match STEPS.iter().find(|s| s.show() == w) { Some(s) => s.clone(), None => return Err(format!("{w}: no such instruction")) },
        })
    }
    fn val(&mut self) -> Result<Val, String> {
        if self.eat('△') || self.eat('t') {
            return Ok(if self.peek().is_some_and(|c| matches!(c, '△' | 't' | '(' | '[') || c.is_ascii_digit()) { Val::s(self.val()?) } else { Val::L });
        }
        if self.eat('(') { let a = self.val()?; self.want(',')?; let b = self.val()?; self.want(')')?; return Ok(Val::f(a, b)); }
        if self.eat('[') { let p = self.prog()?; self.want(']')?; return Ok(p.code()); }
        let n = self.name();
        n.parse::<usize>().map(nat).map_err(|_| format!("expected a value at {}", self.i))
    }
}

impl Val {
    /// Π's natural number, if this is one.
    pub fn as_nat(&self) -> Option<usize> {
        let mut v = self;
        for n in 0.. {
            let Val::F(tag, rest) = v else { return None };
            match &**tag { Val::L => return (**rest == Val::L).then_some(n), Val::S(z) if **z == Val::L => v = rest, _ => return None }
        }
        unreachable!()
    }
    /// As numbers where it holds them: `5`, `(2, 5)`; else as `show` writes it.
    pub fn decoded(&self) -> String {
        if let Some(n) = self.as_nat() { return n.to_string(); }
        match self { Val::F(a, b) => format!("({}, {})", a.decoded(), b.decoded()), _ => self.show() }
    }
}
