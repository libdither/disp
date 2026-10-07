//! Tree-calculus interaction rules represented as the §5.1 template ROM.
//!
//! The uniformly lowered alphabet gives every agent at most three ports (principal plus at
//! most two auxiliaries). This keeps cells uniform for the microcoded executor, bounds a
//! moving agent's wire bends to the two auxiliaries handled by the local star, and keeps
//! conflict footprints finite.
//!
//! `T1` carries its two triage arms as one nested pair `⟨b, c⟩`. `T2` is represented by
//! `Sel` facing the discriminant `z`, with its three arms carried as `⟨w, ⟨x, b⟩⟩`.
//! `Pair` is the producer constructor and `Unp` the consumer destructor; `Unp·Pair` is a
//! pure double wire-fusion. Projections use `Unp` plus an explicit `Eps` on the discarded
//! output, so erasure stays uniform and the ledger counts it directly.
//!
//! A rule = the fresh agents it creates + a perfect matching (the wiring permutation) over
//! {consumer aux ports} ∪ {producer aux ports} ∪ {fresh agent ports}. `validate()` checks
//! the matching property mechanically: a malformed rule cannot load. The created-short
//! lemma (EMBEDDING_THEOREM.md §6) is visible in the shape itself: no rule ever names a
//! wire endpoint outside the dying pair's ports and O(1) fresh agents.
//!
//! Superpositions (`SUP_RULES`) add labels: every agent carries a small number, 0 for all but
//! superpositions and the duplicators copying for them, and a rule says each fresh agent's.
//!
//! The two-level triage semantics are checked by `tests/stage1.rs`, which compares the ROM
//! engine with an independent recursive normalizer.

/// Agent alphabet. Port 0 is ALWAYS the principal. `Out` is the inert result sink and
/// appears in no rule.
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum Tag {
    // Producers (emit at principal).
    L,    // leaf △
    S,    // stem (△ x): [p, child]
    F,    // fork (△ x y): [p, left, right]
    P,    // suspension (f a), inert until forced: [p, fn, arg]
    Pair, // dispatch-arm pair ⟨fst, snd⟩: [p, fst, snd]
    Sup,  // superposition &ℓ{a,b}, a in one universe and b in the other: [p, a, b]
    // Consumers (consume at principal).
    A,   // apply: [p(operator), arg, res]
    T1,  // triage on a, arms as pair: [p(a), args⟨b,c⟩, res]
    Sel, // second-level dispatch on z, arms as ⟨w,⟨x,b⟩⟩: [p(z), arms, res]
    Unp, // unpair: [p(pair), o1, o2]
    Dn,  // need-duplicator δ: [p(value), c1, c2]
    Eps, // eraser ε: [p]
    Nrm, // normalizer: [p, res]
    // Inert.
    Out, // result sink: [in]
}

/// Every tag, in the order engines number them (from 1, 0 meaning none). `Sup` comes last so
/// the others keep the numbers they had before it.
pub const ALL_TAGS: [Tag; 14] = [
    Tag::L, Tag::S, Tag::F, Tag::P, Tag::Pair,
    Tag::A, Tag::T1, Tag::Sel, Tag::Unp, Tag::Dn, Tag::Eps, Tag::Nrm, Tag::Out, Tag::Sup,
];

impl Tag {
    pub fn arity(self) -> usize {
        match self {
            Tag::L | Tag::Eps | Tag::Out => 1,
            Tag::S | Tag::Nrm => 2,
            _ => 3,
        }
    }
    pub fn is_producer(self) -> bool {
        matches!(self, Tag::L | Tag::S | Tag::F | Tag::P | Tag::Pair | Tag::Sup)
    }
    pub fn is_consumer(self) -> bool {
        matches!(self, Tag::A | Tag::T1 | Tag::Sel | Tag::Unp | Tag::Dn | Tag::Eps | Tag::Nrm)
    }
    pub fn name(self) -> &'static str {
        match self {
            Tag::L => "L", Tag::S => "S", Tag::F => "F", Tag::P => "P", Tag::Pair => "Pair", Tag::Sup => "Sup",
            Tag::A => "A", Tag::T1 => "T1", Tag::Sel => "Sel", Tag::Unp => "Unp",
            Tag::Dn => "Dn", Tag::Eps => "Eps", Tag::Nrm => "Nrm", Tag::Out => "Out",
        }
    }
}

/// One endpoint of a template wire.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum End {
    /// The consumer's aux port i (1-based; 0 is the principal being consumed).
    CAux(u8),
    /// The producer's aux port j (1-based).
    PAux(u8),
    /// Port `1` of fresh agent `0` (indices into `Rule::fresh` and its ports).
    Fresh(u8, u8),
}

/// Where a fresh agent's label comes from: none (0, plain), the consumer's or the producer's.
/// A superposition `&ℓ{a,b}` has label ℓ ≥ 1, and so do the duplicators copying what meets it;
/// every other agent is 0, and a duplicator of label 0 is a plain copy.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Lab { Plain, Consumer, Producer }

/// Which labels a rule is for. Only a duplicator meeting a superposition looks: the same label
/// annihilates, a different one commutes.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum When { Any, Same, Differ }

/// One interaction: consumer×producer → fresh agents + wiring permutation.
#[derive(Debug)]
pub struct Rule {
    pub consumer: Tag,
    pub producer: Tag,
    pub when: When,
    pub fresh: &'static [Tag],
    /// Each fresh agent's label; empty when all are plain.
    pub labels: &'static [Lab],
    pub wires: &'static [(End, End)],
}

impl Rule {
    /// Fresh agent k's label, when the consumer has label `c` and the producer `p`.
    pub fn label(&self, k: usize, c: u8, p: u8) -> u8 {
        match self.labels.get(k) { Some(Lab::Consumer) => c, Some(Lab::Producer) => p, _ => 0 }
    }
    /// Whether it is the rule for these labels.
    pub fn applies(&self, c: u8, p: u8) -> bool {
        match self.when { When::Any => true, When::Same => c == p, When::Differ => c != p }
    }
}

use End::{CAux as C, Fresh as X, PAux as Px};
use Lab::{Consumer as LC, Plain as L0, Producer as LP};
use Tag::*;

/// What every rule of `RULES` has but the three duplicator rules: plain labels, for any labels.
const PLAIN: Rule = Rule { consumer: Out, producer: Out, when: When::Any, fresh: &[], labels: &[], wires: &[] };

/// The ROM of nets without superpositions: 26 interactions. Unlisted (consumer, producer)
/// combinations are UNREACHABLE by construction (pairs only appear on dedicated arms wires
/// consumed by Unp/Sel; general values never meet Unp): hitting one is an invariant violation
/// and the engines panic loudly rather than guess. The cascade's 5-bit rule field, the chip
/// and the GPU hold exactly these.
pub static RULES: &[Rule] = &[
    // ---- A: apply. apply(L,x)=Sx; apply(S a,x)=F a x; apply(F a b,x)=triage(a,⟨b,x⟩) ----
    Rule { consumer: A, producer: L, fresh: &[S],
        wires: &[(X(0, 1), C(1)), (X(0, 0), C(2))], ..PLAIN },
    Rule { consumer: A, producer: S, fresh: &[F],
        wires: &[(X(0, 1), Px(1)), (X(0, 2), C(1)), (X(0, 0), C(2))], ..PLAIN },
    Rule { consumer: A, producer: F, fresh: &[T1, Pair],
        wires: &[(X(0, 0), Px(1)), (X(1, 1), Px(2)), (X(1, 2), C(1)), (X(0, 1), X(1, 0)), (X(0, 2), C(2))], ..PLAIN },
    Rule { consumer: A, producer: P, fresh: &[A, A], // force (f a), then apply
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1)), (X(1, 2), C(2))], ..PLAIN },
    // ---- T1: triage on a with args ⟨b,c⟩ ----
    // a=L (K): res ← b = fst, erase c = snd.
    Rule { consumer: T1, producer: L, fresh: &[Unp, Eps],
        wires: &[(X(0, 0), C(1)), (X(0, 1), C(2)), (X(0, 2), X(1, 0))], ..PLAIN },
    // a=S s: (s c)(b c), sharing c through Dn. fresh: unp, dn, a2(b side), a1(s side), a3.
    Rule { consumer: T1, producer: S, fresh: &[Unp, Dn, A, A, A],
        wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 0)), (X(0, 2), X(1, 0)),
                 (X(1, 1), X(2, 1)), (X(1, 2), X(3, 1)), (X(3, 0), Px(1)),
                 (X(2, 2), X(4, 1)), (X(3, 2), X(4, 0)), (X(4, 2), C(2))], ..PLAIN },
    // a=F w x: dispatch Sel on c with arms ⟨w,⟨x,b⟩⟩. fresh: unp, sel, p1(outer), p2(inner).
    Rule { consumer: T1, producer: F, fresh: &[Unp, Sel, Pair, Pair],
        wires: &[(X(0, 0), C(1)), (X(0, 1), X(3, 2)), (X(0, 2), X(1, 0)),
                 (X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(2, 2), X(3, 0)),
                 (X(3, 1), Px(2)), (X(1, 2), C(2))], ..PLAIN },
    Rule { consumer: T1, producer: P, fresh: &[A, T1], // force, then re-dispatch
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1)), (X(1, 2), C(2))], ..PLAIN },
    // ---- Sel: dispatch on z with arms ⟨w,⟨x,b⟩⟩ ----
    // z=L: res ← w = fst, erase ⟨x,b⟩ (Eps·Pair cascades).
    Rule { consumer: Sel, producer: L, fresh: &[Unp, Eps],
        wires: &[(X(0, 0), C(1)), (X(0, 1), C(2)), (X(0, 2), X(1, 0))], ..PLAIN },
    // z=S u: (x u); erase w and b. fresh: u1, u2, e1(w), e2(b), a.
    Rule { consumer: Sel, producer: S, fresh: &[Unp, Unp, Eps, Eps, A],
        wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 0)), (X(0, 2), X(1, 0)),
                 (X(1, 1), X(4, 0)), (X(1, 2), X(3, 0)), (X(4, 1), Px(1)), (X(4, 2), C(2))], ..PLAIN },
    // z=F u v: (b u) v; erase w and x. fresh: u1, u2, e1(w), e2(x), a1, a2.
    Rule { consumer: Sel, producer: F, fresh: &[Unp, Unp, Eps, Eps, A, A],
        wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 0)), (X(0, 2), X(1, 0)),
                 (X(1, 1), X(3, 0)), (X(1, 2), X(4, 0)), (X(4, 1), Px(1)),
                 (X(4, 2), X(5, 0)), (X(5, 1), Px(2)), (X(5, 2), C(2))], ..PLAIN },
    Rule { consumer: Sel, producer: P, fresh: &[A, Sel], // force, then re-dispatch
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1)), (X(1, 2), C(2))], ..PLAIN },
    // ---- Unp: the destructor. Pure double fusion, zero fresh agents. ----
    Rule { consumer: Unp, producer: Pair, fresh: &[],
        wires: &[(C(1), Px(1)), (C(2), Px(2))], ..PLAIN },
    // ---- Dn: need-duplicator. The duplicators it makes keep its label. ----
    Rule { consumer: Dn, producer: L, fresh: &[L, L],
        wires: &[(X(0, 0), C(1)), (X(1, 0), C(2))], ..PLAIN },
    Rule { consumer: Dn, producer: S, fresh: &[Dn, S, S], labels: &[LC, L0, L0],
        wires: &[(X(0, 0), Px(1)), (X(0, 1), X(1, 1)), (X(0, 2), X(2, 1)), (X(1, 0), C(1)), (X(2, 0), C(2))], ..PLAIN },
    Rule { consumer: Dn, producer: F, fresh: &[Dn, Dn, F, F], labels: &[LC, LC, L0, L0],
        wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)),
                 (X(0, 1), X(2, 1)), (X(1, 1), X(2, 2)), (X(0, 2), X(3, 1)), (X(1, 2), X(3, 2)),
                 (X(2, 0), C(1)), (X(3, 0), C(2))], ..PLAIN },
    Rule { consumer: Dn, producer: P, fresh: &[A, Dn], labels: &[L0, LC], // force once, copy the value
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1)), (X(1, 2), C(2))], ..PLAIN },
    // ---- Eps: eraser (the death pulse) ----
    Rule { consumer: Eps, producer: L, fresh: &[], wires: &[], ..PLAIN },
    Rule { consumer: Eps, producer: S, fresh: &[Eps], wires: &[(X(0, 0), Px(1))], ..PLAIN },
    Rule { consumer: Eps, producer: F, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))], ..PLAIN },
    Rule { consumer: Eps, producer: P, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))], ..PLAIN },
    Rule { consumer: Eps, producer: Pair, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))], ..PLAIN },
    // ---- Nrm: drive to normal form ----
    Rule { consumer: Nrm, producer: L, fresh: &[L], wires: &[(X(0, 0), C(1))], ..PLAIN },
    Rule { consumer: Nrm, producer: S, fresh: &[Nrm, S],
        wires: &[(X(0, 0), Px(1)), (X(0, 1), X(1, 1)), (X(1, 0), C(1))], ..PLAIN },
    Rule { consumer: Nrm, producer: F, fresh: &[Nrm, Nrm, F],
        wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)), (X(0, 1), X(2, 1)), (X(1, 1), X(2, 2)), (X(2, 0), C(1))], ..PLAIN },
    Rule { consumer: Nrm, producer: P, fresh: &[A, Nrm], // force, keep normalizing
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1))], ..PLAIN },
];

/// Superpositions, as in HVM's Interaction Calculus: whatever reads `&ℓ{a,b}` splits in two, a
/// copy reading a and a copy reading b, and what it reads besides is copied by a duplicator of
/// label ℓ; the result is `&ℓ{…,…}` of the two. A duplicator of label ℓ meeting `&ℓ{a,b}` hands
/// a to its first copy and b to its second (they belong to those universes); any other label
/// copies a and b and superposes the copies. A triage or dispatch on a superposition copies its
/// arms, so a duplicator may now meet a pair.
///
/// `Unp·Sup` cannot happen: an unpair reads an arms wire, which only ever carries a fresh pair or
/// a duplicator's copy of one. Kept apart from `RULES` so the cascade, the chip and the GPU keep
/// their 26; a net with superpositions runs on the abstract net and the strand lattice's CPU.
pub static SUP_RULES: &[Rule] = &[
    // (&ℓ{a,b} x) = &ℓ{a x₀, b x₁}, x copied by Dn_ℓ. fresh: dn, a1, a2, sup.
    Rule { consumer: A, producer: Sup, when: When::Any, fresh: &[Dn, A, A, Sup], labels: &[LP, L0, L0, LP],
        wires: &[(X(0, 0), C(1)), (X(1, 0), Px(1)), (X(1, 1), X(0, 1)), (X(2, 0), Px(2)), (X(2, 1), X(0, 2)),
                 (X(1, 2), X(3, 1)), (X(2, 2), X(3, 2)), (X(3, 0), C(2))] },
    // triage(&ℓ{a,b}, ⟨b,c⟩) = &ℓ{triage(a, copy₀), triage(b, copy₁)}.
    Rule { consumer: T1, producer: Sup, when: When::Any, fresh: &[Dn, T1, T1, Sup], labels: &[LP, L0, L0, LP],
        wires: &[(X(0, 0), C(1)), (X(1, 0), Px(1)), (X(1, 1), X(0, 1)), (X(2, 0), Px(2)), (X(2, 1), X(0, 2)),
                 (X(1, 2), X(3, 1)), (X(2, 2), X(3, 2)), (X(3, 0), C(2))] },
    Rule { consumer: Sel, producer: Sup, when: When::Any, fresh: &[Dn, Sel, Sel, Sup], labels: &[LP, L0, L0, LP],
        wires: &[(X(0, 0), C(1)), (X(1, 0), Px(1)), (X(1, 1), X(0, 1)), (X(2, 0), Px(2)), (X(2, 1), X(0, 2)),
                 (X(1, 2), X(3, 1)), (X(2, 2), X(3, 2)), (X(3, 0), C(2))] },
    Rule { consumer: Nrm, producer: Sup, when: When::Any, fresh: &[Nrm, Nrm, Sup], labels: &[L0, L0, LP],
        wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)), (X(0, 1), X(2, 1)), (X(1, 1), X(2, 2)), (X(2, 0), C(1))] },
    Rule { consumer: Eps, producer: Sup, when: When::Any, fresh: &[Eps, Eps], labels: &[],
        wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))] },
    // Dn_ℓ·Sup_ℓ: copy 1 ← a, copy 2 ← b.
    Rule { consumer: Dn, producer: Sup, when: When::Same, fresh: &[], labels: &[],
        wires: &[(C(1), Px(1)), (C(2), Px(2))] },
    // Dn_d·Sup_ℓ, d ≠ ℓ: copy 1 ← &ℓ{a₀,b₀}, copy 2 ← &ℓ{a₁,b₁}, as Dn·F copies a fork.
    Rule { consumer: Dn, producer: Sup, when: When::Differ, fresh: &[Dn, Dn, Sup, Sup], labels: &[LC, LC, LP, LP],
        wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)),
                 (X(0, 1), X(2, 1)), (X(1, 1), X(2, 2)), (X(0, 2), X(3, 1)), (X(1, 2), X(3, 2)),
                 (X(2, 0), C(1)), (X(3, 0), C(2))] },
    Rule { consumer: Dn, producer: Pair, when: When::Any, fresh: &[Dn, Dn, Pair, Pair], labels: &[LC, LC, L0, L0],
        wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)),
                 (X(0, 1), X(2, 1)), (X(1, 1), X(2, 2)), (X(0, 2), X(3, 1)), (X(1, 2), X(3, 2)),
                 (X(2, 0), C(1)), (X(3, 0), C(2))] },
];

/// How many rules there are in all.
pub const N_RULES: usize = RULES.len() + SUP_RULES.len();

/// Rule i of `RULES` followed by `SUP_RULES`: the numbering `find_index` gives.
pub fn rule(i: usize) -> &'static Rule {
    if i < RULES.len() { &RULES[i] } else { &SUP_RULES[i - RULES.len()] }
}

/// Every rule, in `rule`'s numbering.
pub fn all_rules() -> impl Iterator<Item = &'static Rule> { RULES.iter().chain(SUP_RULES) }

/// The rule for a plain consumer meeting a plain producer.
pub fn find(consumer: Tag, producer: Tag) -> Option<&'static Rule> { find_labelled(consumer, 0, producer, 0) }

/// The rule for a consumer of label `cl` meeting a producer of label `pl`.
pub fn find_labelled(consumer: Tag, cl: u8, producer: Tag, pl: u8) -> Option<&'static Rule> {
    find_index_labelled(consumer, cl, producer, pl).map(rule)
}

/// The number of a plain consumer×producer pair's rule (used as the packed `RuleId`).
pub fn find_index(consumer: Tag, producer: Tag) -> Option<usize> { find_index_labelled(consumer, 0, producer, 0) }

pub fn find_index_labelled(consumer: Tag, cl: u8, producer: Tag, pl: u8) -> Option<usize> {
    all_rules().position(|r| r.consumer == consumer && r.producer == producer && r.applies(cl, pl))
}

/// Mechanical well-formedness: for every rule, the wire endpoints form a PERFECT MATCHING
/// over consumer aux ∪ producer aux ∪ all fresh ports — each appears exactly once; labels are
/// given to duplicators and superpositions only, a superposition always has one, and every pair
/// of tags has at most one rule for any two labels.
pub fn validate() -> Result<(), String> {
    for r in all_rules() {
        let rn = format!("{}·{}", r.consumer.name(), r.producer.name());
        if !r.consumer.is_consumer() { return Err(format!("{rn}: consumer tag isn't a consumer")); }
        if !r.producer.is_producer() { return Err(format!("{rn}: producer tag isn't a producer")); }
        let mut expected: Vec<End> = vec![];
        for i in 1..r.consumer.arity() { expected.push(C(i as u8)); }
        for j in 1..r.producer.arity() { expected.push(Px(j as u8)); }
        for (k, t) in r.fresh.iter().enumerate() {
            if !matches!(t, L | S | F | Tag::P | Pair | Sup | A | T1 | Sel | Unp | Dn | Eps | Nrm) {
                return Err(format!("{rn}: fresh[{k}] = {} not spawnable", t.name()));
            }
            for p in 0..t.arity() { expected.push(X(k as u8, p as u8)); }
        }
        let mut seen: Vec<End> = vec![];
        for (a, b) in r.wires {
            for e in [a, b] {
                if seen.contains(e) { return Err(format!("{rn}: endpoint {e:?} used twice")); }
                if !expected.contains(e) { return Err(format!("{rn}: endpoint {e:?} not a live port")); }
                seen.push(*e);
            }
        }
        if seen.len() != expected.len() {
            return Err(format!("{rn}: {} endpoints wired, {} live ports", seen.len(), expected.len()));
        }
        if !r.labels.is_empty() && r.labels.len() != r.fresh.len() { return Err(format!("{rn}: labels for some fresh agents only")); }
        for (k, t) in r.fresh.iter().enumerate() {
            let lab = r.labels.get(k).copied().unwrap_or(L0);
            if lab != L0 && !matches!(t, Dn | Sup) { return Err(format!("{rn}: fresh[{k}] = {} labelled", t.name())); }
            if lab == LC && r.consumer != Dn || lab == LP && r.producer != Sup { return Err(format!("{rn}: fresh[{k}] labelled from a plain agent")); }
            if *t == Sup && lab != LP { return Err(format!("{rn}: a fresh Sup without the superposition's label")); }
        }
        if r.when != When::Any && (r.consumer, r.producer) != (Dn, Sup) { return Err(format!("{rn}: only Dn·Sup looks at labels")); }
        for o in all_rules() {
            if std::ptr::eq(o, r) || (o.consumer, o.producer) != (r.consumer, r.producer) { continue; }
            if r.when == When::Any || o.when == When::Any || r.when == o.when { return Err(format!("{rn}: two rules for the same labels")); }
        }
    }
    // Coverage of the reachable consumer×producer plane (a superposition's label is never 0).
    for c in ALL_TAGS.iter().filter(|t| t.is_consumer() && **t != Unp) { // Unp principals only ever face Pair
        for p in [L, S, F, Tag::P, Sup] {
            let labels: &[(u8, u8)] = if p == Sup { &[(0, 1), (1, 1), (2, 1)] } else { &[(0, 0)] };
            for &(cl, pl) in labels {
                if find_labelled(*c, cl, p, pl).is_none() { return Err(format!("missing rule {}·{} for labels {cl}, {pl}", c.name(), p.name())); }
            }
        }
    }
    for c in [Unp, Eps, Dn] {
        if find(c, Pair).is_none() { return Err(format!("missing rule {}·Pair", c.name())); }
    }
    Ok(())
}

/// The ROM of nets without superpositions as JSON, for the JS oracle/visualizer side (derived
/// artifact; the canonical table is this module).
pub fn to_json() -> String {
    let mut s = String::from("{\n  \"note\": \"generated from rust-ca-lattice rules.rs — do not edit\",\n  \"agents\": {\n");
    let tags: Vec<String> = ALL_TAGS.iter().filter(|t| **t != Sup).map(|t| format!(
        "    \"{}\": {{ \"arity\": {}, \"producer\": {} }}", t.name(), t.arity(), t.is_producer())).collect();
    s.push_str(&tags.join(",\n"));
    s.push_str("\n  },\n  \"rules\": [\n");
    let end = |e: &End| match e {
        End::CAux(i) => format!("[\"c\",{i}]"),
        End::PAux(j) => format!("[\"p\",{j}]"),
        End::Fresh(k, p) => format!("[\"f\",{k},{p}]"),
    };
    let rules: Vec<String> = RULES.iter().map(|r| format!(
        "    {{ \"consumer\": \"{}\", \"producer\": \"{}\", \"fresh\": [{}], \"wires\": [{}] }}",
        r.consumer.name(), r.producer.name(),
        r.fresh.iter().map(|t| format!("\"{}\"", t.name())).collect::<Vec<_>>().join(","),
        r.wires.iter().map(|(a, b)| format!("[{},{}]", end(a), end(b))).collect::<Vec<_>>().join(","))).collect();
    s.push_str(&rules.join(",\n"));
    s.push_str("\n  ]\n}\n");
    s
}

#[cfg(test)]
mod tests {
    #[test]
    fn rom_is_well_formed() {
        super::validate().expect("rule ROM must validate");
        assert_eq!(super::RULES.len(), 26);
        assert_eq!(super::SUP_RULES.len(), 8);
    }
}
