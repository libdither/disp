//! EXPERIMENTAL: disp's tree-calculus interaction net (evaluators/tree-net rules.rs) with
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

use tree_net::oracle::Term;
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub enum Tag { L, S, F, P, Pair, A, T1, Sel, Unp, Dn, Eps, Nrm, Out, K, Sx, B, C, I, W, Ref, SelRef, Unw }
use Tag::*;
pub const N_TAGS: usize = 22;
pub const VIEWS: [Tag; 6] = [K, Sx, B, C, I, W];

impl Tag {
    pub fn arity(self) -> usize { match self { L | Eps | Out | I | Ref => 1, S | Nrm | K | W | SelRef | Unw => 2, _ => 3 } }
    pub fn is_producer(self) -> bool { matches!(self, L | S | F | P | Pair | K | Sx | B | C | I | W | Ref) }
    pub fn is_consumer(self) -> bool { matches!(self, A | T1 | Sel | Unp | Dn | Eps | Nrm | SelRef | Unw) }
    /// Port p carries a value out (tree-net polarity.rs, extended to the new agents).
    pub fn is_source(self, p: usize) -> bool {
        match self {
            A | T1 | Sel => p == 2,
            Nrm | SelRef | Unw => p == 1,
            Unp | Dn => p == 1 || p == 2,
            Eps | Out => false,
            _ => p == 0,
        }
    }
    pub fn name(self) -> &'static str {
        match self {
            L => "L", S => "S", F => "F", P => "P", Pair => "Pair", A => "A", T1 => "T1", Sel => "Sel", Unp => "Unp",
            Dn => "Dn", Eps => "Eps", Nrm => "Nrm", Out => "Out", K => "K", Sx => "Sx", B => "B", C => "C", I => "I", W => "W", Ref => "Ref", SelRef => "SelRef", Unw => "Unw",
        }
    }
    fn from_base(t: tree_net::rules::Tag) -> Tag {
        use tree_net::rules::Tag as T;
        match t {
            T::L => L, T::S => S, T::F => F, T::P => P, T::Pair => Pair, T::A => A, T::T1 => T1, T::Sel => Sel, T::Unp => Unp,
            T::Dn => Dn, T::Eps => Eps, T::Nrm => Nrm, T::Out => Out, T::Sup => unreachable!("superpositions are not modelled here"),
            T::K => K, T::Sx => Sx, T::B => B, T::C => C, T::I => I, T::W => W,
            T::Seam => unreachable!("seams are reversible rewriting's, not modelled here"),
            t => unreachable!("keeping history's {} is not modelled here", t.name()),
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
    let mut out: Vec<Rule> = tree_net::rules::RULES.iter().map(|r| {
        use tree_net::rules::End as E;
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
/// supplies a value reference to one that needs it (tree-net polarity.rs's lemma).
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
pub struct Agent { pub tag: Tag, pub ports: [Option<Port>; 3], pub rom: u32, pub origin: Origin, pub code: u32, pub hint: Tag }

/// The read-only table: hash-consed nodes, each a tag and up to two children.
#[derive(Default)]
pub struct Rom { pub nodes: Vec<(Tag, [u32; 2])>, index: HashMap<(Tag, [u32; 2]), u32> }

/// How the loader builds a value: as its tree, or with the movement views (and `W` too).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Shape { pub views: bool, pub w: bool, pub bc: bool, pub pp: bool, pub nok: bool }

/// The agent a value's root becomes, and its children.
fn shape<'a>(t: &'a Term, s: Shape) -> (Tag, Vec<&'a Term>) {
    match t {
        Term::L => (L, vec![]),
        Term::S(a) => (S, vec![&**a]),
        Term::F(a, b) if s.views => match (&**a, &**b) {
            (Term::L, _) if !s.nok => (K, vec![&**b]),
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
    /// The S rule sends its argument only to the sides that are not K-marked constants (`△△f`):
    /// 1 when the constant is there itself, 2 also when it is behind a duplicator copying it.
    pub native: u8,
    /// Which piece of loaded code each subtree is (hash-consed, for following its copies).
    pub ids: Rom,
    /// Native S-rule firings that routed (B or C shapes), erased (both constant), unwrapped a constant in place.
    pub routed: [u64; 3],
    /// Latent labels: loaded forks carry their view's kind; an apply uses it (`hinted_apply`).
    pub hints: bool,
    /// Ablations of the table's apply: 1 no K shortcut, 2 no routing (every S shape copies).
    pub opts: u8,
    /// Check every invariant after every round.
    pub check: bool,
    /// The rewrite count when each agent was made (0: loaded).
    pub born: Vec<u64>,
    pub clock: u64,
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
        Net { agents: vec![], rules, table, rom: Rom::default(), live: 0, peak: 0, unfolds: 0, made: 0, native: 0, routed: [0; 3], hints: false, ids: Rom::default(), opts: 0, check: false, born: vec![], clock: 0 }
    }

    pub fn birth(&self, id: u32) -> u64 { self.born.get(id as usize).copied().unwrap_or(0) }
    pub fn mk(&mut self, tag: Tag, origin: Origin) -> u32 {
        self.born.push(self.clock);
        self.agents.push(Some(Agent { tag, ports: [None; 3], rom: 0, origin, code: u32::MAX, hint: Out }));
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
        if s.views && s.pp && !rom { return self.value_pp(t, origin, s, true); }
        if self.hints && !rom {
            // A plain tree whose forks carry their view's kind as a hint (latent labels).
            let hint = shape(t, Shape { views: true, w: true, bc: true, pp: false, nok: false }).0;
            let (tag, kids) = shape(t, s);
            let a = self.mk(tag, origin);
            if tag == F && hint != F { self.get_mut(a).hint = hint; }
            let s = if tag == F && hint == K { s } else { s };
            if origin == Origin::Code { let id = self.ids.intern(t, s); self.get_mut(a).code = id; }
            for (i, c) in kids.into_iter().enumerate() { let k = self.value(c, origin, s, false); self.link(a, i as u8 + 1, k, 0); }
            return a;
        }
        if rom {
            let k = self.rom.intern(t, s);
            let r = self.mk(Ref, origin);
            self.get_mut(r).rom = k;
            return r;
        }
        let (tag, kids) = shape(t, s);
        let a = self.mk(tag, origin);
        if origin == Origin::Code { let id = self.ids.intern(t, Shape { views: false, w: false, bc: false, pp: false, nok: false }); self.get_mut(a).code = id; }
        for (i, c) in kids.into_iter().enumerate() {
            let k = self.value(c, origin, s, false);
            self.link(a, i as u8 + 1, k, 0);
        }
        a
    }

    /// Views only along the principal path: a value that will be applied gets its view, and of
    /// its parts only those that will be applied in turn (B's f and g, S's s and b, C's f, W's s, a
    /// triage's x and y arms); what is only passed along (K's payload, C's second argument, a
    /// triage's leaf arm, anything a stem or plain node holds) stays a plain tree.
    fn value_pp(&mut self, t: &Term, origin: Origin, s: Shape, fp: bool) -> u32 {
        let plain = Shape { views: false, w: false, bc: false, pp: false, nok: false };
        let (tag, kids) = shape(t, if fp { s } else { plain });
        let fps: Vec<bool> = match tag {
            K => vec![false], Sx | B => vec![true, true], C => vec![true, false], W => vec![true], I => vec![],
            F if fp => vec![true, true], // a triage: its head holds the arms, its tail is applied
            _ => vec![false; kids.len()],
        };
        let a = self.mk(tag, origin);
        if origin == Origin::Code { let id = self.ids.intern(t, plain); self.get_mut(a).code = id; }
        for (i, c) in kids.into_iter().enumerate() {
            let k = if tag == F && fp && i == 0 {
                // the triage's arms: w is returned, x is applied to the stem's child
                let (ht, hk) = shape(c, plain);
                let h = self.mk(ht, origin);
                for (j, d) in hk.into_iter().enumerate() { let x = self.value_pp(d, origin, s, ht == F && j == 1); self.link(h, j as u8 + 1, x, 0); }
                h
            } else { self.value_pp(c, origin, s, fps[i]) };
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
    pub fn unfold(&mut self, r: u32) -> (u32, Vec<u32>) {
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
            let x = self.mk(t, o);
            if keep && t == p.tag { self.get_mut(x).code = p.code; self.get_mut(x).hint = p.hint; }
            x
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

    /// The S rule read natively (`native`): T1 reading stem `S s`, its arms the pair ⟨b, c⟩. A side
    /// that is a K-marked constant `△△f` does not get `c`; the other does, without a copy. Returns
    /// the fresh agents and the extra rewrites (unwrapping a constant in place, the pair's unpack),
    /// or None for an ordinary S firing.
    pub fn native_s(&mut self, t1: u32, stem: u32) -> Option<(Vec<u32>, u64)> {
        #[derive(Clone, Copy)]
        enum Side { Needs(Port), Here { fork: u32, leaf: u32, f: Port }, Rom { r: u32, f: u32 }, Shared(Port) }
        let side = |net: &Net, p: Port| -> Side {
            let a = net.get(p.0);
            if p.1 == 0 && a.tag == F {
                if let Some((l, 0)) = a.ports[1] { if net.get(l).tag == L { return Side::Here { fork: p.0, leaf: l, f: a.ports[2].unwrap() }; } }
            }
            if p.1 == 0 && a.tag == Ref {
                let (t, k) = net.rom.nodes[a.rom as usize];
                if t == F && net.rom.nodes[k[0] as usize].0 == L { return Side::Rom { r: p.0, f: k[1] }; }
            }
            if net.native >= 2 {
                let mut q = Some(p);
                for _ in 0..10_000 {
                    let Some((id, qq)) = q else { break };
                    let x = net.get(id);
                    if x.tag == Dn && qq > 0 { q = x.ports[0]; continue; }
                    if qq == 0 && x.tag == F { if let Some((l, 0)) = x.ports[1] { if net.get(l).tag == L { return Side::Shared(p); } } }
                    break;
                }
            }
            Side::Needs(p)
        };
        let (tc, ts) = (self.get(t1).clone(), self.get(stem).clone());
        let (pair, 0) = tc.ports[1]? else { return None };
        if self.get(pair).tag != Pair { return None; }
        let pr = self.get(pair).clone();
        let (s, b, c, res) = (ts.ports[1]?, pr.ports[1]?, pr.ports[2]?, tc.ports[2]?);
        let (ss, sb) = (side(self, s), side(self, b));
        if matches!((ss, sb), (Side::Needs(_), Side::Needs(_))) { return None; }
        for x in [t1, stem, pair] { self.kill(x); }
        let (mut fresh, mut extra) = (vec![], 1u64);
        // What each side gives the result: (its output, whether it reads c, the apply it needs c in).
        let mut give = |net: &mut Net, sd: Side, fresh: &mut Vec<u32>| -> (Port, Option<u32>) {
            match sd {
                Side::Needs(p) => { let a = net.mk(A, Origin::Built); fresh.push(a); net.link(a, 0, p.0, p.1); (( a, 2), Some(a)) }
                Side::Here { fork, leaf, f } => { net.kill(fork); net.kill(leaf); extra += 1; net.routed[2] += 1; (f, None) }
                Side::Rom { r, f } => {
                    let o = net.get(r).origin;
                    net.kill(r);
                    let x = net.mk(Ref, o);
                    net.get_mut(x).rom = f;
                    net.unfolds += 1;
                    extra += 1;
                    net.routed[2] += 1;
                    ((x, 0), None)
                }
                Side::Shared(p) => {
                    // A constant still being copied: apply it to a throwaway leaf, so it gives its payload without c.
                    let (a, l) = (net.mk(A, Origin::Built), net.mk(L, Origin::Built));
                    fresh.push(a);
                    net.link(a, 0, p.0, p.1);
                    net.link(a, 1, l, 0);
                    ((a, 2), None)
                }
            }
        };
        let (lo, la) = give(self, ss, &mut fresh);
        let (ro, ra) = give(self, sb, &mut fresh);
        match (la, ra) {
            (Some(x), Some(y)) => {
                let d = self.mk(Dn, Origin::Built);
                fresh.push(d);
                self.link(d, 0, c.0, c.1);
                self.link(d, 1, x, 1);
                self.link(d, 2, y, 1);
            }
            (Some(x), None) | (None, Some(x)) => { self.link(x, 1, c.0, c.1); self.routed[0] += 1; }
            (None, None) => { let e = self.mk(Eps, Origin::Built); fresh.push(e); self.link(e, 0, c.0, c.1); self.routed[1] += 1; }
        }
        let top = self.mk(A, Origin::Built);
        fresh.push(top);
        self.link(top, 0, lo.0, lo.1);
        self.link(top, 1, ro.0, ro.1);
        self.link(top, 2, res.0, res.1);
        Some((fresh, extra))
    }

    /// Applying table code natively (`native` 3): an apply reading `Ref k`, node k a fork, reads
    /// the fork's head from the table too and fires its rule in one step: K gives the tail and
    /// erases the argument; S routes the argument as `native_s` does (K-marked sides read from
    /// the table); a fork head dispatches on the argument. Returns the fresh agents.
    pub fn native_apply(&mut self, a: u32, r: u32) -> Option<Vec<u32>> {
        let k = self.get(r).rom;
        let (t, kids) = self.rom.nodes[k as usize];
        if t != F { return None; }
        let (h, tail) = (kids[0], kids[1]);
        let (ht, hk) = self.rom.nodes[h as usize];
        if ht == L && self.opts & 1 != 0 { return None; }
        let (ac, rc) = (self.get(a).clone(), self.get(r).clone());
        let (z, out) = (ac.ports[1]?, ac.ports[2]?);
        let o = rc.origin;
        self.kill(a);
        self.kill(r);
        self.unfolds += 2;
        let mut fresh = vec![];
        let mut rf = |net: &mut Net, n: u32| { let x = net.mk(Ref, o); net.get_mut(x).rom = n; x };
        let kmark = |net: &Net, n: u32| { let (t, c) = net.rom.nodes[n as usize]; (net.opts & 2 == 0 && t == F && net.rom.nodes[c[0] as usize].0 == L).then_some(c[1]) };
        match ht {
            L => {
                let x = rf(self, tail);
                self.link(x, 0, out.0, out.1);
                let e = self.mk(Eps, Origin::Built);
                fresh.push(e);
                self.link(e, 0, z.0, z.1);
            }
            S => {
                let s = hk[0];
                let mut give = |net: &mut Net, n: u32, fresh: &mut Vec<u32>| -> (Port, Option<u32>) {
                    match kmark(net, n) {
                        Some(f) => { net.unfolds += 1; let x = rf(net, f); ((x, 0), None) }
                        None => { let x = rf(net, n); let a = net.mk(A, Origin::Built); fresh.push(a); net.link(a, 0, x, 0); ((a, 2), Some(a)) }
                    }
                };
                let (lo, la) = give(self, s, &mut fresh);
                let (ro, ra) = give(self, tail, &mut fresh);
                match (la, ra) {
                    (Some(x), Some(y)) => { let d = self.mk(Dn, Origin::Built); fresh.push(d); self.link(d, 0, z.0, z.1); self.link(d, 1, x, 1); self.link(d, 2, y, 1); }
                    (Some(x), None) | (None, Some(x)) => { self.link(x, 1, z.0, z.1); self.routed[0] += 1; }
                    (None, None) => { let e = self.mk(Eps, Origin::Built); fresh.push(e); self.link(e, 0, z.0, z.1); self.routed[1] += 1; }
                }
                let top = self.mk(A, Origin::Built);
                fresh.push(top);
                self.link(top, 0, lo.0, lo.1);
                self.link(top, 1, ro.0, ro.1);
                self.link(top, 2, out.0, out.1);
            }
            F if self.native >= 4 => {
                // Triage reading its arms from the table: only the arm taken is ever made.
                let sel = self.mk(SelRef, Origin::Built);
                self.get_mut(sel).rom = k;
                fresh.push(sel);
                self.link(sel, 0, z.0, z.1);
                self.link(sel, 1, out.0, out.1);
            }
            F => {
                // Triage: Sel on the argument, arms ⟨w, ⟨x, tail⟩⟩.
                let (w, x, b) = (rf(self, hk[0]), rf(self, hk[1]), rf(self, tail));
                let (sel, p1, p2) = (self.mk(Sel, Origin::Built), self.mk(Pair, Origin::Built), self.mk(Pair, Origin::Built));
                fresh.push(sel);
                self.link(sel, 0, z.0, z.1);
                self.link(sel, 1, p1, 0);
                self.link(sel, 2, out.0, out.1);
                self.link(p1, 1, w, 0);
                self.link(p1, 2, p2, 0);
                self.link(p2, 1, x, 0);
                self.link(p2, 2, b, 0);
            }
            _ => unreachable!("table nodes are trees"),
        }
        Some(fresh)
    }

    /// A triage whose arms are in the table (`SelRef`, the applied fork's entry) reads the value
    /// it dispatches on: a leaf takes arm w, a stem u gives `x u`, a fork u v gives `b u v`; a
    /// suspension is forced first, a `Ref` unfolded. Only the arm taken is made. The fresh agents.
    pub fn sel_ref(&mut self, sel: u32, z: u32) -> Vec<u32> {
        if self.get(z).tag == Ref {
            // Unfold the value first (a table read), then dispatch on it.
            let (n, made) = self.unfold(z);
            let mut more = self.sel_ref(sel, n);
            more.extend(made);
            return more;
        }
        let (sa, za) = (self.get(sel).clone(), self.get(z).clone());
        let (h, tail) = { let (_, c) = self.rom.nodes[sa.rom as usize]; (c[0], c[1]) };
        let (w, x) = { let (_, c) = self.rom.nodes[h as usize]; (c[0], c[1]) };
        let out = sa.ports[1].unwrap();
        let rf = |net: &mut Net, n: u32| { let r = net.mk(Ref, Origin::Code); net.get_mut(r).rom = n; r };
        self.kill(sel);
        self.kill(z);
        self.unfolds += 1;
        let mut fresh = vec![];
        match za.tag {
            L => { let r = rf(self, w); self.link(r, 0, out.0, out.1); }
            S => {
                let (r, a) = (rf(self, x), self.mk(A, Origin::Built));
                fresh.push(a);
                self.link(a, 0, r, 0);
                let u = za.ports[1].unwrap();
                self.link(a, 1, u.0, u.1);
                self.link(a, 2, out.0, out.1);
            }
            F => {
                let (r, a1, a2) = (rf(self, tail), self.mk(A, Origin::Built), self.mk(A, Origin::Built));
                fresh.extend([a1, a2]);
                let (u, v) = (za.ports[1].unwrap(), za.ports[2].unwrap());
                self.link(a1, 0, r, 0);
                self.link(a1, 1, u.0, u.1);
                self.link(a2, 0, a1, 2);
                self.link(a2, 1, v.0, v.1);
                self.link(a2, 2, out.0, out.1);
            }
            P => {
                let (a, s2) = (self.mk(A, Origin::Built), self.mk(SelRef, Origin::Built));
                self.get_mut(s2).rom = sa.rom;
                fresh.extend([a, s2]);
                let (f, arg) = (za.ports[1].unwrap(), za.ports[2].unwrap());
                self.link(a, 0, f.0, f.1);
                self.link(a, 1, arg.0, arg.1);
                self.link(a, 2, s2, 0);
                self.link(s2, 1, out.0, out.1);
            }
            t => panic!("a triage met {}", t.name()),
        }
        fresh
    }

    /// T1 reading a head `Ref h` from the table (`native` 5): the code built at run time around
    /// table code. T1's arms are the pair ⟨b, c⟩; an unpair splits it. A leaf head is K (b,
    /// erase c); a stem head `S s` routes as the table apply does when s is K-marked (`f (b c)`,
    /// no copy) and copies c otherwise; a fork head dispatches on c with arms from the table.
    pub fn t1_ref(&mut self, t1: u32, r: u32) -> Vec<u32> {
        let (tc, rc) = (self.get(t1).clone(), self.get(r).clone());
        let (ht, hk) = self.rom.nodes[rc.rom as usize];
        let (args, res) = (tc.ports[1].unwrap(), tc.ports[2].unwrap());
        self.kill(t1);
        self.kill(r);
        self.unfolds += 1;
        let mut fresh = vec![];
        let rf = |net: &mut Net, n: u32| { let x = net.mk(Ref, Origin::Code); net.get_mut(x).rom = n; x };
        let u = self.mk(Unp, Origin::Built);
        fresh.push(u);
        self.link(u, 0, args.0, args.1);
        match ht {
            L => {
                // b is the result; c is erased.
                let e = self.mk(Eps, Origin::Built);
                fresh.push(e);
                self.link(u, 1, res.0, res.1);
                self.link(u, 2, e, 0);
            }
            S => {
                let sn = hk[0];
                let (t, c) = self.rom.nodes[sn as usize];
                if t == F && self.rom.nodes[c[0] as usize].0 == L {
                    self.unfolds += 1;
                    let (fr, a1, a2) = (rf(self, c[1]), self.mk(A, Origin::Built), self.mk(A, Origin::Built));
                    fresh.extend([a1, a2]);
                    self.link(a1, 0, u, 1);
                    self.link(a1, 1, u, 2);
                    self.link(a2, 0, fr, 0);
                    self.link(a2, 1, a1, 2);
                    self.link(a2, 2, res.0, res.1);
                    self.routed[0] += 1;
                } else {
                    let (sr, d, a1, a2, a3) = (rf(self, sn), self.mk(Dn, Origin::Built), self.mk(A, Origin::Built), self.mk(A, Origin::Built), self.mk(A, Origin::Built));
                    fresh.extend([d, a1, a2, a3]);
                    self.link(d, 0, u, 2);
                    self.link(a1, 0, sr, 0);
                    self.link(a1, 1, d, 1);
                    self.link(a2, 0, u, 1);
                    self.link(a2, 1, d, 2);
                    self.link(a3, 0, a1, 2);
                    self.link(a3, 1, a2, 2);
                    self.link(a3, 2, res.0, res.1);
                }
            }
            F => {
                let (w, x) = (rf(self, hk[0]), rf(self, hk[1]));
                let (sel, p1, p2) = (self.mk(Sel, Origin::Built), self.mk(Pair, Origin::Built), self.mk(Pair, Origin::Built));
                fresh.push(sel);
                self.link(sel, 0, u, 2);
                self.link(sel, 1, p1, 0);
                self.link(sel, 2, res.0, res.1);
                self.link(p1, 1, w, 0);
                self.link(p1, 2, p2, 0);
                self.link(p2, 1, x, 0);
                self.link(p2, 2, u, 1);
            }
            _ => unreachable!(),
        }
        fresh
    }

    /// An apply meeting a fork whose hint names its shape: the shape's rule in one step, the
    /// fork's own children rewired, and an `Unw` agent fetching each payload that sits deeper
    /// (rom 1: a stem's child; 2: a K fork's payload, its leaf erased; 3: a stem's child, then 2).
    pub fn hinted_apply(&mut self, a: u32, f: u32) -> Option<Vec<u32>> {
        let (fa, aa) = (self.get(f).clone(), self.get(a).clone());
        if fa.tag != F || matches!(fa.hint, Out | F) { return None; }
        let (head, tail) = (fa.ports[1].unwrap(), fa.ports[2].unwrap());
        let (c, r) = (aa.ports[1].unwrap(), aa.ports[2].unwrap());
        self.kill(a);
        self.kill(f);
        let mut fresh = vec![];
        let unw = |net: &mut Net, from: Port, mode: u32, fresh: &mut Vec<u32>| -> Port {
            let u = net.mk(Unw, Origin::Built);
            net.get_mut(u).rom = mode;
            fresh.push(u);
            net.link(u, 0, from.0, from.1);
            (u, 1)
        };
        let ap = |net: &mut Net, fun: Port, arg: Port, fresh: &mut Vec<u32>| -> Port {
            let x = net.mk(A, Origin::Built);
            fresh.push(x);
            net.link(x, 0, fun.0, fun.1);
            net.link(x, 1, arg.0, arg.1);
            (x, 2)
        };
        let eps = |net: &mut Net, at: Port, fresh: &mut Vec<u32>| { let e = net.mk(Eps, Origin::Built); fresh.push(e); net.link(e, 0, at.0, at.1); };
        let out = match fa.hint {
            K => { eps(self, head, &mut fresh); eps(self, c, &mut fresh); tail }
            B => { let fq = unw(self, head, 3, &mut fresh); let gc = ap(self, tail, c, &mut fresh); ap(self, fq, gc, &mut fresh) }
            C => { let fq = unw(self, head, 1, &mut fresh); let gq = unw(self, tail, 2, &mut fresh); let fc = ap(self, fq, c, &mut fresh); ap(self, fc, gq, &mut fresh) }
            Sx => {
                let sq = unw(self, head, 1, &mut fresh);
                let d = self.mk(Dn, Origin::Built);
                fresh.push(d);
                self.link(d, 0, c.0, c.1);
                let sc = ap(self, sq, (d, 1), &mut fresh);
                let bc = ap(self, tail, (d, 2), &mut fresh);
                ap(self, sc, bc, &mut fresh)
            }
            W => {
                // t (t s) s': s c once, then t t; the second copy of s goes.
                eps(self, tail, &mut fresh);
                let sq = unw(self, head, 1, &mut fresh);
                let t = ap(self, sq, c, &mut fresh);
                let d = self.mk(Dn, Origin::Built);
                fresh.push(d);
                self.link(d, 0, t.0, t.1);
                ap(self, (d, 1), (d, 2), &mut fresh)
            }
            I => { eps(self, head, &mut fresh); eps(self, tail, &mut fresh); c }
            _ => unreachable!(),
        };
        self.link(out.0, out.1, r.0, r.1);
        Some(fresh)
    }

    /// An unwrap agent reading what it was sent for (see `hinted_apply`).
    pub fn unwrap_step(&mut self, u: u32, v: u32) -> Vec<u32> {
        let (ua, va) = (self.get(u).clone(), self.get(v).clone());
        let out = ua.ports[1].unwrap();
        self.kill(u);
        self.kill(v);
        let mut fresh = vec![];
        match (ua.rom, va.tag) {
            (1, S) => { let x = va.ports[1].unwrap(); self.link(x.0, x.1, out.0, out.1); }
            (3, S) => {
                let n = self.mk(Unw, Origin::Built);
                self.get_mut(n).rom = 2;
                fresh.push(n);
                let x = va.ports[1].unwrap();
                self.link(n, 0, x.0, x.1);
                self.link(n, 1, out.0, out.1);
            }
            (2, F) => {
                let (l, y) = (va.ports[1].unwrap(), va.ports[2].unwrap());
                let e = self.mk(Eps, Origin::Built);
                fresh.push(e);
                self.link(e, 0, l.0, l.1);
                self.link(y.0, y.1, out.0, out.1);
            }
            (m, P) => {
                // a suspension: force it, then unwrap what it gives
                let (a, n) = (self.mk(A, Origin::Built), self.mk(Unw, Origin::Built));
                self.get_mut(n).rom = m;
                fresh.extend([a, n]);
                let (fq, aq) = (va.ports[1].unwrap(), va.ports[2].unwrap());
                self.link(a, 0, fq.0, fq.1);
                self.link(a, 1, aq.0, aq.1);
                self.link(a, 2, n, 0);
                self.link(n, 1, out.0, out.1);
            }
            (m, t) => panic!("unwrap {m} met {}", t.name()),
        }
        fresh
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
