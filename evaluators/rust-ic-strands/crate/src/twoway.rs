//! Two-way programs on the strand lattice (`Params::two`, rules.rs `TWO_RULES`): a program is a tree of
//! opcodes, each a bijection on trees, and `Run` runs it on a tree, handing the program back beside the
//! result. Running back is running the inverse program (`Prog::dagger`, read off the text) forward.
//!
//! A choice is a sum, as in James and Sabry's Π: (△, v) its left side, (△△, v) its right. `Cond` is Π's
//! ⊕ (the side kept, a program for each), `Flip` its swap, and `Loop` its trace: x goes in as (△△, x),
//! the body runs while it gives a left side (△, s), and its right side (△△, y) gives y.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::Term;
use rust_ca_lattice::rules::Tag;
use std::rc::Rc;

/// A two-way program.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Prog {
    Id, Swap, AssocL, AssocR, StemFork, ForkStem, Uniti, Unite, Copy, Merge, Eval, Flip,
    Seq(Rc<Prog>, Rc<Prog>), Par(Rc<Prog>, Rc<Prog>), Cond(Rc<Prog>, Rc<Prog>), Loop(Rc<Prog>),
}

/// A tree that may hold programs (as data, for `Eval`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Val { L, S(Rc<Val>), F(Rc<Val>, Rc<Val>), P(Rc<Prog>) }

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
    pub fn from(t: &Term) -> Val {
        match t { Term::L => Val::L, Term::S(a) => Val::s(Val::from(a)), Term::F(a, b) => Val::f(Val::from(a), Val::from(b)), Term::Ap(..) => unreachable!("not a tree") }
    }
    pub fn show(&self) -> String {
        match self { Val::L => "△".into(), Val::S(a) => format!("△{}", a.show()), Val::F(a, b) => format!("({}, {})", a.show(), b.show()), Val::P(p) => format!("[{}]", p.show()) }
    }
    pub fn size(&self) -> usize {
        match self { Val::L => 1, Val::S(a) => 1 + a.size(), Val::F(a, b) => 1 + a.size() + b.size(), Val::P(p) => p.size() }
    }
}

impl Prog {
    /// The inverse, read off the text; `Eval` has none here.
    pub fn dagger(&self) -> Option<Prog> {
        use Prog::*;
        Some(match self {
            Id => Id, Swap => Swap, AssocL => AssocR, AssocR => AssocL, StemFork => ForkStem, ForkStem => StemFork,
            Uniti => Unite, Unite => Uniti, Copy => Merge, Merge => Copy, Flip => Flip, Eval => return None,
            Seq(a, b) => seq(b.dagger()?, a.dagger()?),
            Par(a, b) => par(a.dagger()?, b.dagger()?),
            Cond(a, b) => cond(a.dagger()?, b.dagger()?),
            Loop(c) => lp(c.dagger()?),
        })
    }

    /// What the program gives for x: None where it is stuck (no rule fits) or runs past `fuel` rounds of a loop.
    pub fn run(&self, x: &Val, fuel: &mut u64) -> Option<Val> {
        use Prog::*;
        use Val as V;
        let fork = |x: &Val| match x { V::F(a, b) => Some(((**a).clone(), (**b).clone())), _ => None };
        Some(match self {
            Id => x.clone(),
            Swap => { let (u, v) = fork(x)?; V::f(v, u) }
            AssocL => { let (u, vw) = fork(x)?; let (v, w) = fork(&vw)?; V::f(V::f(u, v), w) }
            AssocR => { let (uv, w) = fork(x)?; let (u, v) = fork(&uv)?; V::f(u, V::f(v, w)) }
            StemFork => match x { V::S(t) => V::f(V::L, (**t).clone()), _ => return None },
            ForkStem => match fork(x)? { (V::L, t) => V::s(t), _ => return None },
            Uniti => V::f(V::L, x.clone()),
            Unite => match fork(x)? { (V::L, t) => t, _ => return None },
            Copy => V::f(x.clone(), x.clone()),
            Merge => { let (a, b) = fork(x)?; if a == b { a } else { return None } }
            Eval => match fork(x)? { (V::P(q), z) => V::f(V::P(q.clone()), q.run(&z, fuel)?), _ => return None },
            Flip => match fork(x)? { (V::L, v) => V::inr(v), (V::S(z), v) if *z == V::L => V::inl(v), _ => return None },
            Seq(a, b) => b.run(&a.run(x, fuel)?, fuel)?,
            Par(a, b) => { let (u, v) = fork(x)?; V::f(a.run(&u, fuel)?, b.run(&v, fuel)?) }
            Cond(a, b) => match fork(x)? {
                (V::L, y) => V::f(V::L, a.run(&y, fuel)?),
                (t @ (V::S(_) | V::F(..)), y) => V::f(t, b.run(&y, fuel)?),
                _ => return None,
            },
            Loop(c) => {
                let mut s = V::inr(x.clone());
                loop {
                    if *fuel == 0 { return None; }
                    *fuel -= 1;
                    s = c.run(&s, fuel)?;
                    match fork(&s)? {
                        (V::L, _) => {}
                        (V::S(z), y) if *z == V::L => break y,
                        _ => return None,
                    }
                }
            }
        })
    }

    pub fn size(&self) -> usize {
        use Prog::*;
        match self { Seq(a, b) | Par(a, b) | Cond(a, b) => 1 + a.size() + b.size(), Loop(c) => 1 + c.size(), _ => 1 }
    }

    pub fn show(&self) -> String {
        use Prog::*;
        match self {
            Id => "id".into(), Swap => "swap".into(), AssocL => "assocl".into(), AssocR => "assocr".into(), StemFork => "s2f".into(),
            ForkStem => "f2s".into(), Uniti => "uniti".into(), Unite => "unite".into(), Copy => "copy".into(), Merge => "merge".into(),
            Eval => "eval".into(), Flip => "flip".into(),
            Seq(a, b) => format!("{}; {}", a.show(), b.show()),
            Par(a, b) => format!("({} | {})", a.show(), b.show()),
            Cond(a, b) => format!("cond({}, {})", a.show(), b.show()),
            Loop(c) => format!("loop({})", c.show()),
        }
    }

    /// The program as agents (a leaf for the identity); its root.
    pub fn build(&self, net: &mut Net) -> u32 {
        use Prog::*;
        let leaf = |net: &mut Net, t: Tag| net.mk(t);
        match self {
            Id => leaf(net, Tag::L), Swap => leaf(net, Tag::Swap), AssocL => leaf(net, Tag::AssocL), AssocR => leaf(net, Tag::AssocR),
            StemFork => leaf(net, Tag::StemFork), ForkStem => leaf(net, Tag::ForkStem), Uniti => leaf(net, Tag::Uniti),
            Unite => leaf(net, Tag::Unite), Copy => leaf(net, Tag::Copy), Merge => leaf(net, Tag::Merge), Eval => leaf(net, Tag::Eval),
            Flip => leaf(net, Tag::Flip),
            Seq(a, b) | Par(a, b) | Cond(a, b) => {
                let t = match self { Seq(..) => Tag::Seq, Par(..) => Tag::Par, _ => Tag::Cond };
                let n = net.mk(t);
                let (x, y) = (a.build(net), b.build(net));
                net.link(n, 1, x, 0);
                net.link(n, 2, y, 0);
                n
            }
            Loop(c) => { let n = net.mk(Tag::Loop); let x = c.build(net); net.link(n, 1, x, 0); n }
        }
    }
}

impl Val {
    pub fn build(&self, net: &mut Net) -> u32 {
        match self {
            Val::L => net.mk(Tag::L),
            Val::S(a) => { let n = net.mk(Tag::S); let x = a.build(net); net.link(n, 1, x, 0); n }
            Val::F(a, b) => { let n = net.mk(Tag::F); let (x, y) = (a.build(net), b.build(net)); net.link(n, 1, x, 0); net.link(n, 2, y, 0); n }
            Val::P(p) => p.build(net),
        }
    }
}

/// The net running p on x: `Run` [p, x, program out, result], its two outputs bundled in a fork at the
/// root. Returns the net and the root.
pub fn net(p: &Prog, x: &Val) -> (Net, u32) {
    let mut net = Net::new();
    let run = net.mk(Tag::Run);
    let (pr, xv) = (p.build(&mut net), x.build(&mut net));
    net.link(run, 0, pr, 0);
    net.link(run, 1, xv, 0);
    let (bundle, out) = (net.mk(Tag::F), net.mk(Tag::Out));
    net.link(run, 2, bundle, 1);
    net.link(run, 3, bundle, 2);
    net.link(bundle, 0, out, 0);
    (net, out)
}

/// What a net holds at the agent `id`'s output, as a program, if it is one through and through.
pub fn read_prog(net: &Net, id: u32) -> Option<Prog> {
    use Prog::*;
    let a = net.agents.get(id as usize)?.as_ref()?;
    let kid = |q: usize| -> Option<Prog> { let (k, p) = a.ports[q]?; if p != 0 { return None; } read_prog(net, k) };
    Some(match a.tag {
        Tag::L => Id, Tag::Swap => Swap, Tag::AssocL => AssocL, Tag::AssocR => AssocR, Tag::StemFork => StemFork,
        Tag::ForkStem => ForkStem, Tag::Uniti => Uniti, Tag::Unite => Unite, Tag::Copy => Copy, Tag::Merge => Merge,
        Tag::Eval => Eval, Tag::Flip => Flip,
        Tag::Seq => seq(kid(1)?, kid(2)?), Tag::Par => par(kid(1)?, kid(2)?), Tag::Cond => cond(kid(1)?, kid(2)?),
        Tag::Loop => lp(kid(1)?),
        _ => return None,
    })
}

/// ... as a value, programs in it read as programs.
pub fn read_val(net: &Net, id: u32) -> Option<Val> {
    let a = net.agents.get(id as usize)?.as_ref()?;
    let kid = |q: usize| -> Option<Val> { let (k, p) = a.ports[q]?; if p != 0 { return None; } read_val(net, k) };
    Some(match a.tag {
        Tag::L => Val::L,
        Tag::S => Val::s(kid(1)?),
        Tag::F => Val::f(kid(1)?, kid(2)?),
        t if t.is_two() && t.is_producer() => Val::P(Rc::new(read_prog(net, id)?)),
        _ => return None,
    })
}

/// The program handed back and the result, once the root's bundle holds both whole.
pub fn read(net: &Net, out: u32) -> Option<(Prog, Val)> {
    let (b, 0) = net.agents.get(out as usize)?.as_ref()?.ports[0]? else { return None };
    let bundle = net.agents.get(b as usize)?.as_ref()?;
    if bundle.tag != Tag::F { return None; }
    let side = |q: usize| -> Option<u32> { let (k, p) = bundle.ports[q]?; (p == 0).then_some(k) };
    Some((read_prog(net, side(1)?)?, read_val(net, side(2)?)?))
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

/// The program handed back and the result, once both are whole at the root (the `Out` agent).
pub fn answer(net: &Net) -> Option<(Prog, Val)> {
    let out = net.agents.iter().position(|a| a.as_ref().is_some_and(|a| a.tag == Tag::Out))?;
    read(net, out as u32)
}

/// Agents a finished run should not leave: anything but trees, programs and the root. A program stuck
/// where it has no rule (a swap of a leaf, a merge of two different halves) leaves its walker here,
/// though the rest of its result may still reach the root.
pub fn leftovers(net: &Net) -> Vec<Tag> {
    net.agents.iter().flatten().map(|a| a.tag)
        .filter(|&t| !(matches!(t, Tag::L | Tag::S | Tag::F | Tag::Out) || t.is_two() && t.is_producer())).collect()
}

// ---- as text ------------------------------------------------------------------------------------

/// A run as typed: `PROG @ VALUE`, or a library call `add A B` (`sub A C` its inverse, `succ N`, `pred N`), numbers
/// being Π's (`nat`). PROG is as `Prog::show` writes it: the opcodes by name, `a; b`, `(a | b)`, `cond(a, b)`,
/// `loop(a)`, `~a` for a's inverse and the library's `succ`, `pred`, `add` and `sub`. VALUE: `△` (or `t`) a leaf,
/// `△v` a stem, `(u, v)` a fork, a number, `[PROG]` a program as data.
pub fn parse_run(src: &str) -> Result<(Prog, Val), String> {
    let words: Vec<&str> = src.split_whitespace().collect();
    let num = |w: &str| w.parse::<usize>().map_err(|_| format!("{w}: not a number"));
    match words[..] {
        ["add", a, b] => return Ok((add(), Val::f(nat(num(a)?), nat(num(b)?)))),
        ["sub", a, c] => return Ok((add().dagger().unwrap(), Val::f(nat(num(a)?), nat(num(c)?)))),
        ["succ", n] => return Ok((succ(), nat(num(n)?))),
        ["pred", n] => return Ok((succ().dagger().unwrap(), nat(num(n)?))),
        _ => {}
    }
    let (p, x) = src.split_once('@').ok_or("PROG @ VALUE, or add A B, sub A C, succ N, pred N")?;
    let mut r = Reader { s: p.chars().filter(|c| !c.is_whitespace()).collect(), i: 0 };
    let prog = r.prog()?;
    r.end()?;
    let mut r = Reader { s: x.chars().filter(|c| !c.is_whitespace()).collect(), i: 0 };
    let val = r.val()?;
    r.end()?;
    Ok((prog, val))
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
        use Prog::*;
        if self.eat('~') { return self.step()?.dagger().ok_or_else(|| "eval has no inverse".into()); }
        if self.eat('(') {
            let a = self.prog()?;
            let p = if self.eat('|') { par(a, self.prog()?) } else { a };
            self.want(')')?;
            return Ok(p);
        }
        let n = self.name();
        let two = |r: &mut Reader| -> Result<(Prog, Prog), String> { r.want('(')?; let a = r.prog()?; r.want(',')?; let b = r.prog()?; r.want(')')?; Ok((a, b)) };
        Ok(match n.as_str() {
            "id" => Id, "swap" => Swap, "assocl" => AssocL, "assocr" => AssocR, "s2f" => StemFork, "f2s" => ForkStem,
            "uniti" => Uniti, "unite" => Unite, "copy" => Copy, "merge" => Merge, "eval" => Eval, "flip" => Flip,
            "succ" => succ(), "pred" => succ().dagger().unwrap(), "add" => add(), "sub" => add().dagger().unwrap(),
            "cond" => { let (a, b) = two(self)?; cond(a, b) }
            "loop" => { self.want('(')?; let c = self.prog()?; self.want(')')?; lp(c) }
            "" => return Err(format!("expected a program at {}", self.i)),
            w => return Err(format!("{w}: no such opcode")),
        })
    }
    fn val(&mut self) -> Result<Val, String> {
        if self.eat('△') || self.eat('t') {
            return Ok(if self.peek().is_some_and(|c| matches!(c, '△' | 't' | '(' | '[') || c.is_ascii_digit()) { Val::s(self.val()?) } else { Val::L });
        }
        if self.eat('(') { let a = self.val()?; self.want(',')?; let b = self.val()?; self.want(')')?; return Ok(Val::f(a, b)); }
        if self.eat('[') { let p = self.prog()?; self.want(']')?; return Ok(Val::P(Rc::new(p))); }
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
