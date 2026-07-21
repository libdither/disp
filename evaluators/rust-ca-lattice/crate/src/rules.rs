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

pub const ALL_TAGS: [Tag; 13] = [
    Tag::L, Tag::S, Tag::F, Tag::P, Tag::Pair,
    Tag::A, Tag::T1, Tag::Sel, Tag::Unp, Tag::Dn, Tag::Eps, Tag::Nrm, Tag::Out,
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
        matches!(self, Tag::L | Tag::S | Tag::F | Tag::P | Tag::Pair)
    }
    pub fn is_consumer(self) -> bool {
        matches!(self, Tag::A | Tag::T1 | Tag::Sel | Tag::Unp | Tag::Dn | Tag::Eps | Tag::Nrm)
    }
    pub fn name(self) -> &'static str {
        match self {
            Tag::L => "L", Tag::S => "S", Tag::F => "F", Tag::P => "P", Tag::Pair => "Pair",
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

/// One interaction: consumer×producer → fresh agents + wiring permutation.
#[derive(Debug)]
pub struct Rule {
    pub consumer: Tag,
    pub producer: Tag,
    pub fresh: &'static [Tag],
    pub wires: &'static [(End, End)],
}

use End::{CAux as C, Fresh as X, PAux as Px};
use Tag::*;

/// The complete ROM: 26 interactions. Unlisted (consumer, producer) combinations are
/// UNREACHABLE by construction (pairs only appear on dedicated arms wires consumed by
/// Unp/Sel; general values never meet Unp): hitting one is an invariant violation and the
/// engines panic loudly rather than guess.
pub static RULES: &[Rule] = &[
    // ---- A: apply. apply(L,x)=Sx; apply(S a,x)=F a x; apply(F a b,x)=triage(a,⟨b,x⟩) ----
    Rule { consumer: A, producer: L, fresh: &[S],
        wires: &[(X(0, 1), C(1)), (X(0, 0), C(2))] },
    Rule { consumer: A, producer: S, fresh: &[F],
        wires: &[(X(0, 1), Px(1)), (X(0, 2), C(1)), (X(0, 0), C(2))] },
    Rule { consumer: A, producer: F, fresh: &[T1, Pair],
        wires: &[(X(0, 0), Px(1)), (X(1, 1), Px(2)), (X(1, 2), C(1)), (X(0, 1), X(1, 0)), (X(0, 2), C(2))] },
    Rule { consumer: A, producer: P, fresh: &[A, A], // force (f a), then apply
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1)), (X(1, 2), C(2))] },
    // ---- T1: triage on a with args ⟨b,c⟩ ----
    // a=L (K): res ← b = fst, erase c = snd.
    Rule { consumer: T1, producer: L, fresh: &[Unp, Eps],
        wires: &[(X(0, 0), C(1)), (X(0, 1), C(2)), (X(0, 2), X(1, 0))] },
    // a=S s: (s c)(b c), sharing c through Dn. fresh: unp, dn, a2(b side), a1(s side), a3.
    Rule { consumer: T1, producer: S, fresh: &[Unp, Dn, A, A, A],
        wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 0)), (X(0, 2), X(1, 0)),
                 (X(1, 1), X(2, 1)), (X(1, 2), X(3, 1)), (X(3, 0), Px(1)),
                 (X(2, 2), X(4, 1)), (X(3, 2), X(4, 0)), (X(4, 2), C(2))] },
    // a=F w x: dispatch Sel on c with arms ⟨w,⟨x,b⟩⟩. fresh: unp, sel, p1(outer), p2(inner).
    Rule { consumer: T1, producer: F, fresh: &[Unp, Sel, Pair, Pair],
        wires: &[(X(0, 0), C(1)), (X(0, 1), X(3, 2)), (X(0, 2), X(1, 0)),
                 (X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(2, 2), X(3, 0)),
                 (X(3, 1), Px(2)), (X(1, 2), C(2))] },
    Rule { consumer: T1, producer: P, fresh: &[A, T1], // force, then re-dispatch
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1)), (X(1, 2), C(2))] },
    // ---- Sel: dispatch on z with arms ⟨w,⟨x,b⟩⟩ ----
    // z=L: res ← w = fst, erase ⟨x,b⟩ (Eps·Pair cascades).
    Rule { consumer: Sel, producer: L, fresh: &[Unp, Eps],
        wires: &[(X(0, 0), C(1)), (X(0, 1), C(2)), (X(0, 2), X(1, 0))] },
    // z=S u: (x u); erase w and b. fresh: u1, u2, e1(w), e2(b), a.
    Rule { consumer: Sel, producer: S, fresh: &[Unp, Unp, Eps, Eps, A],
        wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 0)), (X(0, 2), X(1, 0)),
                 (X(1, 1), X(4, 0)), (X(1, 2), X(3, 0)), (X(4, 1), Px(1)), (X(4, 2), C(2))] },
    // z=F u v: (b u) v; erase w and x. fresh: u1, u2, e1(w), e2(x), a1, a2.
    Rule { consumer: Sel, producer: F, fresh: &[Unp, Unp, Eps, Eps, A, A],
        wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 0)), (X(0, 2), X(1, 0)),
                 (X(1, 1), X(3, 0)), (X(1, 2), X(4, 0)), (X(4, 1), Px(1)),
                 (X(4, 2), X(5, 0)), (X(5, 1), Px(2)), (X(5, 2), C(2))] },
    Rule { consumer: Sel, producer: P, fresh: &[A, Sel], // force, then re-dispatch
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1)), (X(1, 2), C(2))] },
    // ---- Unp: the destructor. Pure double fusion, zero fresh agents. ----
    Rule { consumer: Unp, producer: Pair, fresh: &[],
        wires: &[(C(1), Px(1)), (C(2), Px(2))] },
    // ---- Dn: need-duplicator ----
    Rule { consumer: Dn, producer: L, fresh: &[L, L],
        wires: &[(X(0, 0), C(1)), (X(1, 0), C(2))] },
    Rule { consumer: Dn, producer: S, fresh: &[Dn, S, S],
        wires: &[(X(0, 0), Px(1)), (X(0, 1), X(1, 1)), (X(0, 2), X(2, 1)), (X(1, 0), C(1)), (X(2, 0), C(2))] },
    Rule { consumer: Dn, producer: F, fresh: &[Dn, Dn, F, F],
        wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)),
                 (X(0, 1), X(2, 1)), (X(1, 1), X(2, 2)), (X(0, 2), X(3, 1)), (X(1, 2), X(3, 2)),
                 (X(2, 0), C(1)), (X(3, 0), C(2))] },
    Rule { consumer: Dn, producer: P, fresh: &[A, Dn], // force once, copy the value
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1)), (X(1, 2), C(2))] },
    // ---- Eps: eraser (the death pulse) ----
    Rule { consumer: Eps, producer: L, fresh: &[], wires: &[] },
    Rule { consumer: Eps, producer: S, fresh: &[Eps], wires: &[(X(0, 0), Px(1))] },
    Rule { consumer: Eps, producer: F, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))] },
    Rule { consumer: Eps, producer: P, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))] },
    Rule { consumer: Eps, producer: Pair, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))] },
    // ---- Nrm: drive to normal form ----
    Rule { consumer: Nrm, producer: L, fresh: &[L], wires: &[(X(0, 0), C(1))] },
    Rule { consumer: Nrm, producer: S, fresh: &[Nrm, S],
        wires: &[(X(0, 0), Px(1)), (X(0, 1), X(1, 1)), (X(1, 0), C(1))] },
    Rule { consumer: Nrm, producer: F, fresh: &[Nrm, Nrm, F],
        wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)), (X(0, 1), X(2, 1)), (X(1, 1), X(2, 2)), (X(2, 0), C(1))] },
    Rule { consumer: Nrm, producer: P, fresh: &[A, Nrm], // force, keep normalizing
        wires: &[(X(0, 0), Px(1)), (X(0, 1), Px(2)), (X(0, 2), X(1, 0)), (X(1, 1), C(1))] },
];

pub fn find(consumer: Tag, producer: Tag) -> Option<&'static Rule> {
    RULES.iter().find(|r| r.consumer == consumer && r.producer == producer)
}

/// The ROM index of a consumer×producer pair (used as the packed `RuleId`).
pub fn find_index(consumer: Tag, producer: Tag) -> Option<usize> {
    RULES.iter().position(|r| r.consumer == consumer && r.producer == producer)
}

/// Mechanical well-formedness: for every rule, the wire endpoints form a PERFECT MATCHING
/// over consumer aux ∪ producer aux ∪ all fresh ports — each appears exactly once.
pub fn validate() -> Result<(), String> {
    for r in RULES {
        let rn = format!("{}·{}", r.consumer.name(), r.producer.name());
        if !r.consumer.is_consumer() { return Err(format!("{rn}: consumer tag isn't a consumer")); }
        if !r.producer.is_producer() { return Err(format!("{rn}: producer tag isn't a producer")); }
        let mut expected: Vec<End> = vec![];
        for i in 1..r.consumer.arity() { expected.push(C(i as u8)); }
        for j in 1..r.producer.arity() { expected.push(Px(j as u8)); }
        for (k, t) in r.fresh.iter().enumerate() {
            if !matches!(t, L | S | F | Tag::P | Pair | A | T1 | Sel | Unp | Dn | Eps | Nrm) {
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
    }
    // Coverage of the reachable consumer×producer plane.
    for c in ALL_TAGS.iter().filter(|t| t.is_consumer()) {
        for p in [L, S, F, Tag::P] {
            let reachable = *c != Unp; // Unp principals only ever face Pair
            if reachable && find(*c, p).is_none() {
                return Err(format!("missing rule {}·{}", c.name(), p.name()));
            }
        }
    }
    if find(Unp, Pair).is_none() { return Err("missing rule Unp·Pair".into()); }
    if find(Eps, Pair).is_none() { return Err("missing rule Eps·Pair".into()); }
    Ok(())
}

/// The ROM as JSON, for the JS oracle/visualizer side (derived artifact; the canonical
/// table is this module).
pub fn to_json() -> String {
    let mut s = String::from("{\n  \"note\": \"generated from rust-ca-lattice rules.rs — do not edit\",\n  \"agents\": {\n");
    let tags: Vec<String> = ALL_TAGS.iter().map(|t| format!(
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
    }
}
