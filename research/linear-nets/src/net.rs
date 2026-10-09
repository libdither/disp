//! EXPERIMENTAL: disp's tree-calculus interaction net (evaluators/rust-ca-lattice rules.rs) with
//! two additions, each a strict superset of the 26 rules, for research/LINEAR_TREE_NETS.md:
//! - movement agents: views of fixed tree shapes the loader finds in code, each with a one-step
//!   apply rule: `K y` = △△y, `Sx s b` = △(△s)b (the S rule), `B f g` = △(△(△△f))g (moves the
//!   argument right), `C f g` = △(△f)(△△g) (moves it left), `I` = △(△△△)△, and `W s` = △(△s)s
//!   (the S rule with both halves the same code, so `s c` runs once). Anything but an apply,
//!   duplicator or eraser reading a view spells it out as its tree first, so triage still sees
//!   the tree and reflection is unchanged.
//! - code in a read-only table: `Ref k` stands for node k of a hash-consed table of the loaded
//!   values. A duplicator copies it in one step and an eraser deletes it in one step; anything
//!   else reading it gets node k with its children as fresh `Ref`s (an "unfold", free here, as
//!   it is a table lookup inside the rewrite), so the table is never consumed.

use rust_ca_lattice::oracle::Term;
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub enum Tag { L, S, F, P, Pair, A, T1, Sel, Unp, Dn, Eps, Nrm, Out, K, Sx, B, C, I, W, Ref }
use Tag::*;
pub const N_TAGS: usize = 20;
pub const VIEWS: [Tag; 6] = [K, Sx, B, C, I, W];

impl Tag {
    pub fn arity(self) -> usize { match self { L | Eps | Out | I | Ref => 1, S | Nrm | K | W => 2, _ => 3 } }
    pub fn is_producer(self) -> bool { matches!(self, L | S | F | P | Pair | K | Sx | B | C | I | W | Ref) }
    pub fn is_consumer(self) -> bool { matches!(self, A | T1 | Sel | Unp | Dn | Eps | Nrm) }
    /// Port p carries a value out (rust-ic-mesh polarity.rs, extended to the new agents).
    pub fn is_source(self, p: usize) -> bool {
        match self {
            A | T1 | Sel => p == 2,
            Nrm => p == 1,
            Unp | Dn => p == 1 || p == 2,
            Eps | Out => false,
            _ => p == 0,
        }
    }
    pub fn name(self) -> &'static str {
        match self {
            L => "L", S => "S", F => "F", P => "P", Pair => "Pair", A => "A", T1 => "T1", Sel => "Sel", Unp => "Unp",
            Dn => "Dn", Eps => "Eps", Nrm => "Nrm", Out => "Out", K => "K", Sx => "Sx", B => "B", C => "C", I => "I", W => "W", Ref => "Ref",
        }
    }
    fn from_base(t: rust_ca_lattice::rules::Tag) -> Tag {
        use rust_ca_lattice::rules::Tag as T;
        match t {
            T::L => L, T::S => S, T::F => F, T::P => P, T::Pair => Pair, T::A => A, T::T1 => T1, T::Sel => Sel, T::Unp => Unp,
            T::Dn => Dn, T::Eps => Eps, T::Nrm => Nrm, T::Out => Out, T::Sup => unreachable!("superpositions are not modelled here"),
        }
    }
}

/// A template wire end: the consumer's aux port i, the producer's aux port j, or port p of fresh agent k.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum End { C(u8), P(u8), X(u8, u8) }
use End::{C as Cx, P as Px, X};

pub struct Rule {
    pub consumer: Tag,
    pub producer: Tag,
    pub fresh: Vec<Tag>,
    pub wires: Vec<(End, End)>,
    /// The fresh values are the producer's own nodes (a copy, a normalizer re-emitting it, a view
    /// spelled out), so they keep its origin; otherwise they are built by the run.
    pub keep: bool,
}

impl Rule {
    pub fn name(&self) -> String { format!("{}·{}", self.consumer.name(), self.producer.name()) }
}

fn rule(consumer: Tag, producer: Tag, fresh: &[Tag], wires: &[(End, End)], keep: bool) -> Rule {
    Rule { consumer, producer, fresh: fresh.to_vec(), wires: wires.to_vec(), keep }
}

/// The 26 rules of rules.rs, then the movement agents' rules. `Ref` has no template rules:
/// `Net::fire` handles it.
pub fn all_rules() -> Vec<Rule> {
    let mut out: Vec<Rule> = rust_ca_lattice::rules::RULES.iter().map(|r| {
        use rust_ca_lattice::rules::End as E;
        let end = |e: &E| match *e { E::CAux(i) => Cx(i), E::PAux(j) => Px(j), E::Fresh(k, p) => X(k, p) };
        let (c, p) = (Tag::from_base(r.consumer), Tag::from_base(r.producer));
        Rule { consumer: c, producer: p, fresh: r.fresh.iter().map(|t| Tag::from_base(*t)).collect(),
            wires: r.wires.iter().map(|(a, b)| (end(a), end(b))).collect(), keep: matches!(c, Dn | Nrm) && p != P }
    }).collect();
    // Applying a view: one rewrite each. The apply's argument is C(1), its result C(2).
    out.push(rule(A, K, &[Eps], &[(Px(1), Cx(2)), (X(0, 0), Cx(1))], false)); // K y c = y
    out.push(rule(A, Sx, &[Dn, A, A, A], &[(X(0, 0), Cx(1)), (X(1, 0), Px(1)), (X(1, 1), X(0, 2)), (X(2, 0), Px(2)), (X(2, 1), X(0, 1)),
        (X(3, 0), X(1, 2)), (X(3, 1), X(2, 2)), (X(3, 2), Cx(2))], false)); // S s b c = s c (b c)
    out.push(rule(A, B, &[A, A], &[(X(1, 0), Px(2)), (X(1, 1), Cx(1)), (X(0, 0), Px(1)), (X(0, 1), X(1, 2)), (X(0, 2), Cx(2))], false)); // B f g c = f (g c)
    out.push(rule(A, C, &[A, A], &[(X(0, 0), Px(1)), (X(0, 1), Cx(1)), (X(1, 0), X(0, 2)), (X(1, 1), Px(2)), (X(1, 2), Cx(2))], false)); // C f g c = f c g
    out.push(rule(A, I, &[], &[(Cx(1), Cx(2))], false)); // I c = c
    out.push(rule(A, W, &[A, Dn, A], &[(X(0, 0), Px(1)), (X(0, 1), Cx(1)), (X(1, 0), X(0, 2)), (X(2, 0), X(1, 1)), (X(2, 1), X(1, 2)), (X(2, 2), Cx(2))], false)); // W s c = t t, t = s c once
    // Copying and erasing a view: one agent, as a fork is.
    for v in VIEWS {
        match v.arity() {
            1 => {
                out.push(rule(Dn, v, &[v, v], &[(X(0, 0), Cx(1)), (X(1, 0), Cx(2))], true));
                out.push(rule(Eps, v, &[], &[], false));
            }
            2 => {
                out.push(rule(Dn, v, &[Dn, v, v], &[(X(0, 0), Px(1)), (X(0, 1), X(1, 1)), (X(0, 2), X(2, 1)), (X(1, 0), Cx(1)), (X(2, 0), Cx(2))], true));
                out.push(rule(Eps, v, &[Eps], &[(X(0, 0), Px(1))], false));
            }
            _ => {
                out.push(rule(Dn, v, &[Dn, Dn, v, v], &[(X(0, 0), Px(1)), (X(1, 0), Px(2)), (X(0, 1), X(2, 1)), (X(1, 1), X(2, 2)),
                    (X(0, 2), X(3, 1)), (X(1, 2), X(3, 2)), (X(2, 0), Cx(1)), (X(3, 0), Cx(2))], true));
                out.push(rule(Eps, v, &[Eps, Eps], &[(X(0, 0), Px(1)), (X(1, 0), Px(2))], false));
            }
        }
    }
    // Anything else reading a view spells it out (fresh agent 0 is the reader again, 1 the tree's root).
    for q in [T1, Sel, Nrm] {
        let reader: Vec<(End, End)> = if q == Nrm { vec![(X(0, 1), Cx(1))] } else { vec![(X(0, 1), Cx(1)), (X(0, 2), Cx(2))] };
        let spell = |v: Tag, fresh: &[Tag], wires: &[(End, End)]| {
            let mut f = vec![q];
            f.extend_from_slice(fresh);
            let mut w = vec![(X(0, 0), X(1, 0))];
            w.extend_from_slice(wires);
            w.extend_from_slice(&reader);
            rule(q, v, &f, &w, true)
        };
        out.push(spell(K, &[F, L], &[(X(1, 1), X(2, 0)), (X(1, 2), Px(1))])); // F(L, y)
        out.push(spell(Sx, &[F, S], &[(X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(1, 2), Px(2))])); // F(S(s), b)
        out.push(spell(B, &[F, S, F, L], &[(X(1, 1), X(2, 0)), (X(2, 1), X(3, 0)), (X(3, 1), X(4, 0)), (X(3, 2), Px(1)), (X(1, 2), Px(2))])); // F(S(F(L,f)), g)
        out.push(spell(C, &[F, S, F, L], &[(X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(1, 2), X(3, 0)), (X(3, 1), X(4, 0)), (X(3, 2), Px(2))])); // F(S(f), F(L,g))
        out.push(spell(I, &[F, F, L, L, L], &[(X(1, 1), X(2, 0)), (X(2, 1), X(3, 0)), (X(2, 2), X(4, 0)), (X(1, 2), X(5, 0))])); // F(F(L,L),L)
        out.push(spell(W, &[F, S, Dn], &[(X(1, 1), X(2, 0)), (X(2, 1), X(3, 1)), (X(1, 2), X(3, 2)), (X(3, 0), Px(1))])); // F(S(s), s)
    }
    out
}

/// Every rule's wires are a perfect matching of the live ports, and every wire joins a port that
/// supplies a value reference to one that needs it (rust-ic-mesh polarity.rs's lemma).
pub fn validate(rules: &[Rule]) -> Result<(), String> {
    for r in rules {
        let n = r.name();
        let mut expected: Vec<End> = vec![];
        for i in 1..r.consumer.arity() { expected.push(Cx(i as u8)); }
        for j in 1..r.producer.arity() { expected.push(Px(j as u8)); }
        for (k, t) in r.fresh.iter().enumerate() { for p in 0..t.arity() { expected.push(X(k as u8, p as u8)); } }
        let mut seen: Vec<End> = vec![];
        for (a, b) in &r.wires {
            for e in [a, b] {
                if seen.contains(e) { return Err(format!("{n}: {e:?} used twice")); }
                if !expected.contains(e) { return Err(format!("{n}: {e:?} is not a live port")); }
                seen.push(*e);
            }
            let have = |e: &End| match *e {
                X(k, p) => r.fresh[k as usize].is_source(p as usize),
                Cx(i) => !r.consumer.is_source(i as usize),
                Px(j) => !r.producer.is_source(j as usize),
            };
            if have(a) == have(b) { return Err(format!("{n}: wire {a:?}–{b:?} joins two of a kind")); }
        }
        if seen.len() != expected.len() { return Err(format!("{n}: {} of {} ports wired", seen.len(), expected.len())); }
        if rules.iter().filter(|o| (o.consumer, o.producer) == (r.consumer, r.producer)).count() > 1 { return Err(format!("{n}: two rules")); }
    }
    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Origin { Code, Data, Built }

pub type Port = (u32, u8);

#[derive(Clone)]
pub struct Agent { pub tag: Tag, pub ports: [Option<Port>; 3], pub rom: u32, pub origin: Origin }

/// The read-only table: hash-consed nodes, each a tag and up to two children.
#[derive(Default)]
pub struct Rom { pub nodes: Vec<(Tag, [u32; 2])>, index: HashMap<(Tag, [u32; 2]), u32> }

/// How the loader builds a value: as its tree, or with the movement views (and `W` too).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Shape { pub views: bool, pub w: bool, pub bc: bool }

/// The agent a value's root becomes, and its children.
fn shape<'a>(t: &'a Term, s: Shape) -> (Tag, Vec<&'a Term>) {
    match t {
        Term::L => (L, vec![]),
        Term::S(a) => (S, vec![&**a]),
        Term::F(a, b) if s.views => match (&**a, &**b) {
            (Term::L, _) => (K, vec![&**b]),
            (Term::S(x), _) => match (&**x, &**b) {
                (Term::F(l, f), _) if s.bc && **l == Term::L => (B, vec![&**f, &**b]),
                (_, Term::F(l, g)) if s.bc && **l == Term::L => (C, vec![&**x, &**g]),
                _ if s.w && x == b => (W, vec![&**x]),
                _ => (Sx, vec![&**x, &**b]),
            },
            (Term::F(l, r), Term::L) if **l == Term::L && **r == Term::L => (I, vec![]),
            _ => (F, vec![&**a, &**b]),
        },
        Term::F(a, b) => (F, vec![&**a, &**b]),
        Term::Ap(..) => panic!("a value holds no application"),
    }
}

impl Rom {
    pub fn intern(&mut self, t: &Term, s: Shape) -> u32 {
        let (tag, kids) = shape(t, s);
        let mut k = [0u32; 2];
        for (i, c) in kids.iter().enumerate() { k[i] = self.intern(c, s); }
        let n = self.nodes.len() as u32;
        *self.index.entry((tag, k)).or_insert_with(|| { self.nodes.push((tag, k)); n })
    }
}

pub struct Net {
    pub agents: Vec<Option<Agent>>,
    pub rules: Vec<Rule>,
    table: Vec<Option<u16>>,
    pub rom: Rom,
    pub live: usize,
    pub peak: usize,
    pub unfolds: u64,
    pub made: u64,
}

/// What a rewrite made: the rule, its fresh agents in template order, and the agents an unfold
/// of a `Ref` made first.
pub struct Fired { pub rule: usize, pub fresh: Vec<u32>, pub unfolded: Vec<u32>, pub origin: Origin }

impl Net {
    pub fn new() -> Net {
        let rules = all_rules();
        validate(&rules).expect("rules validate");
        let mut table = vec![None; N_TAGS * N_TAGS];
        for (i, r) in rules.iter().enumerate() { table[r.consumer as usize * N_TAGS + r.producer as usize] = Some(i as u16); }
        Net { agents: vec![], rules, table, rom: Rom::default(), live: 0, peak: 0, unfolds: 0, made: 0 }
    }

    pub fn mk(&mut self, tag: Tag, origin: Origin) -> u32 {
        self.agents.push(Some(Agent { tag, ports: [None; 3], rom: 0, origin }));
        self.live += 1;
        self.made += 1;
        self.peak = self.peak.max(self.live);
        (self.agents.len() - 1) as u32
    }
    pub fn kill(&mut self, id: u32) { self.agents[id as usize] = None; self.live -= 1; }
    pub fn get(&self, id: u32) -> &Agent { self.agents[id as usize].as_ref().expect("live agent") }
    fn get_mut(&mut self, id: u32) -> &mut Agent { self.agents[id as usize].as_mut().expect("live agent") }
    pub fn tag(&self, id: u32) -> Option<Tag> { self.agents[id as usize].as_ref().map(|a| a.tag) }
    pub fn link(&mut self, a: u32, i: u8, b: u32, j: u8) {
        self.get_mut(a).ports[i as usize] = Some((b, j));
        self.get_mut(b).ports[j as usize] = Some((a, i));
    }
    pub fn rule_for(&self, c: Tag, p: Tag) -> Option<usize> { self.table[c as usize * N_TAGS + p as usize].map(|i| i as usize) }

    /// A value as agents: its tree (with views if asked), or one `Ref` into the table.
    pub fn value(&mut self, t: &Term, origin: Origin, s: Shape, rom: bool) -> u32 {
        if rom {
            let k = self.rom.intern(t, s);
            let r = self.mk(Ref, origin);
            self.get_mut(r).rom = k;
            return r;
        }
        let (tag, kids) = shape(t, s);
        let a = self.mk(tag, origin);
        for (i, c) in kids.into_iter().enumerate() {
            let k = self.value(c, origin, s, false);
            self.link(a, i as u8 + 1, k, 0);
        }
        a
    }

    /// `f x y …` as suspensions over values; the root's port 0 is the output.
    pub fn call(&mut self, head: u32, args: &[u32]) -> u32 {
        args.iter().fold(head, |f, &x| {
            let p = self.mk(P, Origin::Built);
            self.link(p, 1, f, 0);
            self.link(p, 2, x, 0);
            p
        })
    }

    /// A normalizer on the root and the sink it fills: returns the sink.
    pub fn drive(&mut self, root: u32) -> u32 {
        let n = self.mk(Nrm, Origin::Built);
        let o = self.mk(Out, Origin::Built);
        self.link(n, 0, root, 0);
        self.link(n, 1, o, 0);
        o
    }

    /// Replace `Ref k` by table node k, its children fresh `Ref`s. Returns the node and all it made.
    fn unfold(&mut self, r: u32) -> (u32, Vec<u32>) {
        let a = self.get(r).clone();
        let (tag, kids) = self.rom.nodes[a.rom as usize];
        let n = self.mk(tag, a.origin);
        let mut made = vec![n];
        for q in 1..tag.arity() {
            let k = self.mk(Ref, a.origin);
            self.get_mut(k).rom = kids[q - 1];
            self.link(n, q as u8, k, 0);
            made.push(k);
        }
        let (t, tp) = a.ports[0].expect("wired");
        self.kill(r);
        self.link(n, 0, t, tp);
        self.unfolds += 1;
        (n, made)
    }

    /// One interaction between a consumer and the producer its principal faces.
    pub fn fire(&mut self, consumer: u32, producer: u32) -> Fired {
        let (mut producer, mut unfolded) = (producer, vec![]);
        let ct = self.get(consumer).tag;
        if self.get(producer).tag == Ref {
            match ct {
                Dn => {
                    let (r, (a, ap), (b, bp)) = (self.get(producer).clone(), self.get(consumer).ports[1].unwrap(), self.get(consumer).ports[2].unwrap());
                    self.kill(consumer);
                    self.kill(producer);
                    let (x, y) = (self.mk(Ref, r.origin), self.mk(Ref, r.origin));
                    self.get_mut(x).rom = r.rom;
                    self.get_mut(y).rom = r.rom;
                    self.link(x, 0, a, ap);
                    self.link(y, 0, b, bp);
                    return Fired { rule: usize::MAX, fresh: vec![x, y], unfolded, origin: r.origin };
                }
                Eps => {
                    let o = self.get(producer).origin;
                    self.kill(consumer);
                    self.kill(producer);
                    return Fired { rule: usize::MAX - 1, fresh: vec![], unfolded, origin: o };
                }
                _ => { let (n, made) = self.unfold(producer); producer = n; unfolded = made; }
            }
        }
        let (c, p) = (self.get(consumer).clone(), self.get(producer).clone());
        let ri = self.rule_for(c.tag, p.tag).unwrap_or_else(|| panic!("no rule for {}·{}", c.tag.name(), p.tag.name()));
        self.kill(consumer);
        self.kill(producer);
        let rule = &self.rules[ri];
        let (fresh_tags, wires, keep) = (rule.fresh.clone(), rule.wires.clone(), rule.keep);
        let fresh: Vec<u32> = fresh_tags.iter().map(|&t| {
            let o = if keep && t.is_producer() { p.origin } else { Origin::Built };
            self.mk(t, o)
        }).collect();
        #[derive(Clone, Copy)]
        enum R { Fresh(u32, u8), Ext(Port) }
        let partner = |e: End| -> End {
            for (a, b) in &wires { if *a == e { return *b; } if *b == e { return *a; } }
            unreachable!()
        };
        let resolve = |start: End| -> R {
            let mut e = start;
            for _ in 0..64 {
                let t = match e {
                    X(k, q) => return R::Fresh(fresh[k as usize], q),
                    Cx(i) => c.ports[i as usize].expect("wired"),
                    Px(j) => p.ports[j as usize].expect("wired"),
                };
                if t.0 == consumer { e = partner(Cx(t.1)); } else if t.0 == producer { e = partner(Px(t.1)); } else { return R::Ext(t); }
            }
            panic!("vicious circle");
        };
        for &(a, b) in &wires {
            match (resolve(a), resolve(b)) {
                (R::Fresh(x, i), R::Fresh(y, j)) => self.link(x, i, y, j),
                (R::Fresh(x, i), R::Ext(t)) | (R::Ext(t), R::Fresh(x, i)) => self.link(x, i, t.0, t.1),
                (R::Ext(t1), R::Ext(t2)) => self.link(t1.0, t1.1, t2.0, t2.1),
            }
        }
        Fired { rule: ri, fresh, unfolded, origin: p.origin }
    }

    /// The plain tree hanging off a port, or None while anything in it is not a leaf, stem or fork.
    pub fn readback(&self, r: Option<Port>) -> Option<Term> {
        let (id, _) = r?;
        let a = self.agents.get(id as usize)?.as_ref()?;
        let kid = |q: usize| self.readback(a.ports[q]).map(std::rc::Rc::new);
        match a.tag {
            L => Some(Term::L),
            S => Some(Term::S(kid(1)?)),
            F => Some(Term::F(kid(1)?, kid(2)?)),
            _ => None,
        }
    }
}
