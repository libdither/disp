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
//! Movement agents (`VIEW_RULES`) add six values, each a view of a fixed tree shape found in loaded
//! code, with plain template rules: no label, no payload.
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
    // Movement agents (`VIEW_RULES`), after the others so their numbers stay as they were: each
    // stands for its tree.
    K,  // △△y: [p, y]
    Sx, // △(△s)b, the S rule: [p, s, b]
    B,  // △(△(△△f))g, the argument going right only: [p, f, g]
    C,  // △(△f)(△△g), the argument going left only: [p, f, g]
    I,  // △(△△△)△: [p]
    W,  // △(△s)s, both halves the same code: [p, s]
    /// A mark on a wire that reversible rewriting (rust-ic-strands rev.rs) leaves where a rule joins two
    /// wires into one: interactions see straight through it. [a, b]
    Seam,
    // Keeping history (`RESID_RULES`, research/reversible-core/residual.py): each apply leaves what it
    // would have dropped, its residual, on a fourth port, and a residual applied backward to its result
    // gives back the function and the argument.
    RA,    // apply: [p(function), arg, out, residual]
    RT1,   // triage on a: [p(a), ⟨b,c⟩, out, residual]
    RSel,  // dispatch on the argument: [p(c), Arms3(w,x,b), out, residual]
    SL,    // dispatch, the argument a leaf, reading the arms: [p(arms), out, residual]
    SS,    // the argument a stem: [p(arms), u, out, residual]
    SF,    // the argument a fork: [p(arms), ⟨u,v⟩, out, residual]
    T1s,   // the S rule looking at s for B: [p(s), ⟨b,c⟩, out, residual]
    T1sf,  // at s's left child: [p, ⟨s's right child, ⟨b,c⟩⟩, out, residual]
    T1sc0, // opening ⟨b,c⟩ to look at b for C: [p(⟨b,c⟩), s, out, residual]
    T1sc,  // at b: [p(b), ⟨s,c⟩, out, residual]
    T1scf, // at b's left child: [p, ⟨b's right child, ⟨s,c⟩⟩, out, residual]
    TSb,   // the S rule proper, once ⟨s,c⟩ is read: [p(⟨s,c⟩), b, out, residual]
    UA,    // un-apply: [p(residual), result, argument out, function out]
    UAr,   // un-apply of a constructor, reading the result: [p(result), argument out, function out]
    USel,  // dispatch run back: [p(argument's head), Arms3(d, r, result), argument out, function out]
    USL,   // [p(arms), argument out, function out]
    USS,   // [p(arms), j, argument out, function out]
    USF,   // [p(arms), ⟨j₁,j₂⟩, argument out, function out]
    Mg,    // merge, a copy run back: [p(copy), copy, value out]
    MgL,   // having read a leaf: [p(other copy), value out]
    MgS,   // having read a stem: [p(other copy), child, value out]
    MgF,   // having read a fork: [p(other copy), left, right, value out]
    Arms3, // a triage's three arms: [p, w, x, b]
    Rk,    // residual △△c: [p, c]
    Rs,    // residual △(△j₁)(△j₂j₃): [p, j₁, j₂, j₃]
    Rt,    // residual △(△hd)r: [p, h, d, r]
    Rb,    // residual △(△j₁j₂): [p, j₁, j₂]
    Rc,    // residual △(△(△j₁j₂)): [p, j₁, j₂]
    /// Holds a wire shut until opened (`Lattice::open_gates`): what runs back waits behind gates
    /// while the answer is worked out. [in, out]
    Gate,
    /// Running back, a fork or stem of the loaded term that holds applications, taken apart so each
    /// part runs back on its own (resid.rs). [p, left, right] and [p, child]
    Fm,
    Sm,
}

/// Every tag, in the order engines number them (from 1, 0 meaning none). `Sup` comes last so
/// the others keep the numbers they had before it.
pub const ALL_TAGS: [Tag; 14] = [
    Tag::L, Tag::S, Tag::F, Tag::P, Tag::Pair,
    Tag::A, Tag::T1, Tag::Sel, Tag::Unp, Tag::Dn, Tag::Eps, Tag::Nrm, Tag::Out, Tag::Sup,
];

/// The movement agents' tags, numbered after `ALL_TAGS` (15 to 20) where an engine runs them: the
/// abstract net and the strand lattice, on its CPU and GPU (the cascade and the chip keep 4 bits).
pub const VIEW_TAGS: [Tag; 6] = [Tag::K, Tag::Sx, Tag::B, Tag::C, Tag::I, Tag::W];

/// The tags of keeping history (`RESID_RULES`), numbered after the movement agents' where the strand
/// lattice runs them (its CPU only).
pub const RESID_TAGS: [Tag; 31] = [
    Tag::RA, Tag::RT1, Tag::RSel, Tag::SL, Tag::SS, Tag::SF, Tag::T1s, Tag::T1sf, Tag::T1sc0, Tag::T1sc, Tag::T1scf, Tag::TSb,
    Tag::UA, Tag::UAr, Tag::USel, Tag::USL, Tag::USS, Tag::USF, Tag::Mg, Tag::MgL, Tag::MgS, Tag::MgF,
    Tag::Arms3, Tag::Rk, Tag::Rs, Tag::Rt, Tag::Rb, Tag::Rc, Tag::Gate, Tag::Fm, Tag::Sm,
];

impl Tag {
    pub fn arity(self) -> usize {
        match self {
            Tag::L | Tag::Eps | Tag::Out | Tag::I => 1,
            Tag::S | Tag::Nrm | Tag::K | Tag::W | Tag::Seam | Tag::MgL | Tag::Rk | Tag::Gate | Tag::Sm => 2,
            Tag::RA | Tag::RT1 | Tag::RSel | Tag::SS | Tag::SF | Tag::T1s | Tag::T1sf | Tag::T1sc0 | Tag::T1sc | Tag::T1scf
            | Tag::TSb | Tag::UA | Tag::USel | Tag::USS | Tag::USF | Tag::MgF | Tag::Arms3 | Tag::Rs | Tag::Rt => 4,
            _ => 3,
        }
    }
    pub fn is_producer(self) -> bool {
        matches!(self, Tag::L | Tag::S | Tag::F | Tag::P | Tag::Pair | Tag::Sup | Tag::Arms3 | Tag::Rk | Tag::Rs | Tag::Rt | Tag::Rb | Tag::Rc)
            || self.is_view()
    }
    pub fn is_view(self) -> bool { matches!(self, Tag::K | Tag::Sx | Tag::B | Tag::C | Tag::I | Tag::W) }
    /// Keeping history's (`RESID_RULES`).
    pub fn is_resid(self) -> bool { RESID_TAGS.contains(&self) }
    pub fn is_consumer(self) -> bool {
        matches!(self, Tag::A | Tag::T1 | Tag::Sel | Tag::Unp | Tag::Dn | Tag::Eps | Tag::Nrm)
            || self.is_resid() && !self.is_producer() && self != Tag::Gate
    }
    pub fn name(self) -> &'static str {
        match self {
            Tag::L => "L", Tag::S => "S", Tag::F => "F", Tag::P => "P", Tag::Pair => "Pair", Tag::Sup => "Sup",
            Tag::A => "A", Tag::T1 => "T1", Tag::Sel => "Sel", Tag::Unp => "Unp",
            Tag::Dn => "Dn", Tag::Eps => "Eps", Tag::Nrm => "Nrm", Tag::Out => "Out",
            Tag::K => "K", Tag::Sx => "Sx", Tag::B => "B", Tag::C => "C", Tag::I => "I", Tag::W => "W",
            Tag::Seam => "Seam",
            Tag::RA => "A+", Tag::RT1 => "T1+", Tag::RSel => "Sel+", Tag::SL => "SL+", Tag::SS => "SS+", Tag::SF => "SF+",
            Tag::T1s => "T1s+", Tag::T1sf => "T1sf+", Tag::T1sc0 => "T1sc0+", Tag::T1sc => "T1sc+", Tag::T1scf => "T1scf+", Tag::TSb => "TSb+",
            Tag::UA => "A-", Tag::UAr => "Ar-", Tag::USel => "Sel-", Tag::USL => "SL-", Tag::USS => "SS-", Tag::USF => "SF-",
            Tag::Mg => "Dn-", Tag::MgL => "DnL-", Tag::MgS => "DnS-", Tag::MgF => "DnF-",
            Tag::Arms3 => "Arms3", Tag::Rk => "Rk", Tag::Rs => "Rs", Tag::Rt => "Rt", Tag::Rb => "Rb", Tag::Rc => "Rc", Tag::Gate => "Gate",
            Tag::Fm => "F-", Tag::Sm => "S-",
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

/// How many rules there are in all, the movement agents' apart (`N_ALL_RULES`).
pub const N_RULES: usize = RULES.len() + SUP_RULES.len();

/// Every rule but keeping history's, the movement agents' included.
pub const N_ALL_RULES: usize = N_RULES + VIEW_RULES.len();

/// Every rule: `rule`'s numbering runs to here.
pub const N_EVERY_RULE: usize = N_ALL_RULES + RESID_RULES.len();

/// Rule i of `RULES` followed by `SUP_RULES`, `VIEW_RULES` and `RESID_RULES`: the numbering `find_index` gives.
pub fn rule(i: usize) -> &'static Rule {
    if i < RULES.len() { &RULES[i] } else if i < N_RULES { &SUP_RULES[i - RULES.len()] }
    else if i < N_ALL_RULES { &VIEW_RULES[i - N_RULES] } else { &RESID_RULES[i - N_ALL_RULES] }
}

/// Keeping history (research/reversible-core/residual.py, where these are checked against an oracle and
/// run back to the very agents loaded; `python residual.py --rust` writes them): an apply leaves on its fourth port what it
/// would have dropped, as a tree whose layout says which case fired (the j's are the residuals of the
/// applies a step starts):
///
/// ```text
/// △ z         = △z         residual △
/// △x z        = △xz        residual △
/// △△b c       = b          residual Rk(c)          = △△c
/// △(△s)b c    = s c (b c)  residual Rs(j₁, j₂, j₃)  = △(△j₁)(△j₂j₃)
/// △(△wx)b △   = w          residual Rt(△, x, b)    = △(△△x)b
/// △(△wx)b △u  = x u        residual Rt(△j, w, b)   = △(△(△j)w)b
/// △(△wx)b △uv = b u v      residual Rt(△j₁j₂, w, x) = △(△(△j₁j₂)w)x
/// B f g c     = f (g c)    residual Rb(j₁, j₂)      = △(△j₁j₂)        B f g = △(△(△△f))g
/// C f g c     = f c g      residual Rc(j₁, j₂)      = △(△(△j₁j₂))     C f g = △(△f)(△△g)
/// ```
///
/// The S rule looks at its function for B and C first and sends the argument only to the side that
/// uses it. Running back is done by the same walkers turned round: `UA` un-applies (a residual read as
/// the function, the result as the argument), `USel` dispatches back and `Mg` merges two copies; `UAr`
/// and `MgL`/`MgS`/`MgF` are the second reads undoing a creation needs; `Fm` and `Sm` take apart a
/// fork or stem of the loaded term that holds applications (the last two rules, not the model's). Three moves that are not
/// interactions finish the job (`Lattice::collect`): an un-apply meeting an apply that never ran, a
/// merge meeting the duplicator its two copies came from, and an eraser on both outputs of an apply
/// that never ran. Duplicators, unpairs and erasers are `RULES`'.
pub static RESID_RULES: &[Rule] = &[
    // ---- forward ----
    Rule { consumer: RA, producer: L, fresh: &[S, L], wires: &[(X(0, 1), C(1)), (X(0, 0), C(2)), (X(1, 0), C(3))], ..PLAIN },
    Rule { consumer: RA, producer: S, fresh: &[F, L], wires: &[(X(0, 1), Px(1)), (X(0, 2), C(1)), (X(0, 0), C(2)), (X(1, 0), C(3))], ..PLAIN },
    Rule { consumer: RA, producer: F, fresh: &[RT1, Pair], wires: &[(X(0, 0), Px(1)), (X(1, 1), Px(2)), (X(1, 2), C(1)), (X(0, 1), X(1, 0)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: RT1, producer: L, fresh: &[Unp, Rk], wires: &[(X(0, 0), C(1)), (X(0, 1), C(2)), (X(0, 2), X(1, 1)), (X(1, 0), C(3))], ..PLAIN },
    Rule { consumer: RT1, producer: F, fresh: &[Unp, RSel, Arms3], wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 3)), (X(0, 2), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(2, 2), Px(2)), (X(1, 2), C(2)), (X(1, 3), C(3))], ..PLAIN },
    Rule { consumer: RSel, producer: L, fresh: &[SL], wires: &[(X(0, 0), C(1)), (X(0, 1), C(2)), (X(0, 2), C(3))], ..PLAIN },
    Rule { consumer: SL, producer: Arms3, fresh: &[Rt, L], wires: &[(Px(1), C(1)), (X(0, 1), X(1, 0)), (X(0, 2), Px(2)), (X(0, 3), Px(3)), (X(0, 0), C(2))], ..PLAIN },
    Rule { consumer: RSel, producer: S, fresh: &[SS], wires: &[(X(0, 0), C(1)), (X(0, 1), Px(1)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: SS, producer: Arms3, fresh: &[RA, Rt, S], wires: &[(X(0, 0), Px(2)), (X(0, 1), C(1)), (X(0, 2), C(2)), (X(0, 3), X(2, 1)), (X(1, 1), X(2, 0)), (X(1, 2), Px(1)), (X(1, 3), Px(3)), (X(1, 0), C(3))], ..PLAIN },
    Rule { consumer: RSel, producer: F, fresh: &[SF, Pair], wires: &[(X(1, 1), Px(1)), (X(1, 2), Px(2)), (X(0, 0), C(1)), (X(0, 1), X(1, 0)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: SF, producer: Arms3, fresh: &[Unp, RA, RA, Rt, F], wires: &[(X(0, 0), C(1)), (X(1, 0), Px(3)), (X(1, 1), X(0, 1)), (X(1, 2), X(2, 0)), (X(2, 1), X(0, 2)), (X(2, 2), C(2)), (X(1, 3), X(4, 1)), (X(2, 3), X(4, 2)), (X(3, 1), X(4, 0)), (X(3, 2), Px(1)), (X(3, 3), Px(2)), (X(3, 0), C(3))], ..PLAIN },
    Rule { consumer: RT1, producer: S, fresh: &[T1s], wires: &[(X(0, 0), Px(1)), (X(0, 1), C(1)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: T1s, producer: L, fresh: &[L, T1sc0], wires: &[(X(1, 0), C(1)), (X(1, 1), X(0, 0)), (X(1, 2), C(2)), (X(1, 3), C(3))], ..PLAIN },
    Rule { consumer: T1s, producer: S, fresh: &[S, T1sc0], wires: &[(X(0, 1), Px(1)), (X(1, 0), C(1)), (X(1, 1), X(0, 0)), (X(1, 2), C(2)), (X(1, 3), C(3))], ..PLAIN },
    Rule { consumer: T1s, producer: F, fresh: &[T1sf, Pair], wires: &[(X(0, 0), Px(1)), (X(1, 1), Px(2)), (X(1, 2), C(1)), (X(0, 1), X(1, 0)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: T1sf, producer: L, fresh: &[Unp, Unp, RA, RA, Rb], wires: &[(X(0, 0), C(1)), (X(0, 1), X(3, 0)), (X(0, 2), X(1, 0)), (X(1, 1), X(2, 0)), (X(1, 2), X(2, 1)), (X(2, 2), X(3, 1)), (X(3, 2), C(2)), (X(2, 3), X(4, 1)), (X(3, 3), X(4, 2)), (X(4, 0), C(3))], ..PLAIN },
    Rule { consumer: T1sf, producer: S, fresh: &[Unp, S, F, T1sc0], wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 2)), (X(0, 2), X(3, 0)), (X(1, 1), Px(1)), (X(2, 1), X(1, 0)), (X(3, 1), X(2, 0)), (X(3, 2), C(2)), (X(3, 3), C(3))], ..PLAIN },
    Rule { consumer: T1sf, producer: F, fresh: &[Unp, F, F, T1sc0], wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 2)), (X(0, 2), X(3, 0)), (X(1, 1), Px(1)), (X(1, 2), Px(2)), (X(2, 1), X(1, 0)), (X(3, 1), X(2, 0)), (X(3, 2), C(2)), (X(3, 3), C(3))], ..PLAIN },
    Rule { consumer: T1sc0, producer: Pair, fresh: &[T1sc, Pair], wires: &[(X(0, 0), Px(1)), (X(1, 1), C(1)), (X(1, 2), Px(2)), (X(0, 1), X(1, 0)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: T1sc, producer: L, fresh: &[L, TSb], wires: &[(X(1, 0), C(1)), (X(1, 1), X(0, 0)), (X(1, 2), C(2)), (X(1, 3), C(3))], ..PLAIN },
    Rule { consumer: T1sc, producer: S, fresh: &[S, TSb], wires: &[(X(0, 1), Px(1)), (X(1, 0), C(1)), (X(1, 1), X(0, 0)), (X(1, 2), C(2)), (X(1, 3), C(3))], ..PLAIN },
    Rule { consumer: T1sc, producer: F, fresh: &[T1scf, Pair], wires: &[(X(0, 0), Px(1)), (X(1, 1), Px(2)), (X(1, 2), C(1)), (X(0, 1), X(1, 0)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: T1scf, producer: L, fresh: &[Unp, Unp, RA, RA, Rc], wires: &[(X(0, 0), C(1)), (X(0, 1), X(3, 1)), (X(0, 2), X(1, 0)), (X(1, 1), X(2, 0)), (X(1, 2), X(2, 1)), (X(2, 2), X(3, 0)), (X(3, 2), C(2)), (X(2, 3), X(4, 1)), (X(3, 3), X(4, 2)), (X(4, 0), C(3))], ..PLAIN },
    Rule { consumer: T1scf, producer: S, fresh: &[Unp, S, F, TSb], wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 2)), (X(0, 2), X(3, 0)), (X(1, 1), Px(1)), (X(2, 1), X(1, 0)), (X(3, 1), X(2, 0)), (X(3, 2), C(2)), (X(3, 3), C(3))], ..PLAIN },
    Rule { consumer: T1scf, producer: F, fresh: &[Unp, F, F, TSb], wires: &[(X(0, 0), C(1)), (X(0, 1), X(2, 2)), (X(0, 2), X(3, 0)), (X(1, 1), Px(1)), (X(1, 2), Px(2)), (X(2, 1), X(1, 0)), (X(3, 1), X(2, 0)), (X(3, 2), C(2)), (X(3, 3), C(3))], ..PLAIN },
    Rule { consumer: TSb, producer: Pair, fresh: &[Dn, RA, RA, RA, Rs], wires: &[(X(2, 0), Px(1)), (X(1, 0), C(1)), (X(0, 0), Px(2)), (X(0, 1), X(1, 1)), (X(0, 2), X(2, 1)), (X(1, 2), X(3, 1)), (X(2, 2), X(3, 0)), (X(3, 2), C(2)), (X(4, 1), X(2, 3)), (X(4, 2), X(1, 3)), (X(4, 3), X(3, 3)), (X(4, 0), C(3))], ..PLAIN },
    // ---- backward ----
    Rule { consumer: UA, producer: L, fresh: &[UAr], wires: &[(X(0, 0), C(1)), (X(0, 1), C(2)), (X(0, 2), C(3))], ..PLAIN },
    Rule { consumer: UAr, producer: S, fresh: &[L], wires: &[(Px(1), C(1)), (X(0, 0), C(2))], ..PLAIN },
    Rule { consumer: UAr, producer: F, fresh: &[S], wires: &[(X(0, 1), Px(1)), (Px(2), C(1)), (X(0, 0), C(2))], ..PLAIN },
    Rule { consumer: UA, producer: Rk, fresh: &[F, L], wires: &[(Px(1), C(2)), (X(0, 1), X(1, 0)), (X(0, 2), C(1)), (X(0, 0), C(3))], ..PLAIN },
    Rule { consumer: UA, producer: Rs, fresh: &[UA, UA, UA, Mg, F, S], wires: &[(X(0, 0), Px(3)), (X(0, 1), C(1)), (X(0, 2), X(2, 1)), (X(0, 3), X(1, 1)), (X(1, 0), Px(1)), (X(2, 0), Px(2)), (X(1, 2), X(3, 0)), (X(2, 2), X(3, 1)), (X(3, 2), C(2)), (X(1, 3), X(5, 1)), (X(2, 3), X(4, 2)), (X(4, 1), X(5, 0)), (X(4, 0), C(3))], ..PLAIN },
    Rule { consumer: UA, producer: Rt, fresh: &[USel, Arms3], wires: &[(X(0, 0), Px(1)), (X(0, 1), X(1, 0)), (X(1, 1), Px(2)), (X(1, 2), Px(3)), (X(1, 3), C(1)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: USel, producer: L, fresh: &[USL], wires: &[(X(0, 0), C(1)), (X(0, 1), C(2)), (X(0, 2), C(3))], ..PLAIN },
    Rule { consumer: USL, producer: Arms3, fresh: &[L, F, F], wires: &[(X(0, 0), C(1)), (X(2, 1), Px(3)), (X(2, 2), Px(1)), (X(1, 1), X(2, 0)), (X(1, 2), Px(2)), (X(1, 0), C(2))], ..PLAIN },
    Rule { consumer: USel, producer: S, fresh: &[USS], wires: &[(X(0, 0), C(1)), (X(0, 1), Px(1)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: USS, producer: Arms3, fresh: &[UA, S, F, F], wires: &[(X(0, 0), C(1)), (X(0, 1), Px(3)), (X(0, 2), X(1, 1)), (X(0, 3), X(3, 2)), (X(1, 0), C(2)), (X(3, 1), Px(1)), (X(2, 1), X(3, 0)), (X(2, 2), Px(2)), (X(2, 0), C(3))], ..PLAIN },
    Rule { consumer: USel, producer: F, fresh: &[USF, Pair], wires: &[(X(1, 1), Px(1)), (X(1, 2), Px(2)), (X(0, 0), C(1)), (X(0, 1), X(1, 0)), (X(0, 2), C(2)), (X(0, 3), C(3))], ..PLAIN },
    Rule { consumer: USF, producer: Arms3, fresh: &[Unp, UA, UA, F, F, F], wires: &[(X(0, 0), C(1)), (X(2, 0), X(0, 2)), (X(2, 1), Px(3)), (X(2, 2), X(3, 2)), (X(2, 3), X(1, 1)), (X(1, 0), X(0, 1)), (X(1, 2), X(3, 1)), (X(1, 3), X(4, 2)), (X(3, 0), C(2)), (X(5, 1), Px(1)), (X(5, 2), Px(2)), (X(4, 1), X(5, 0)), (X(4, 0), C(3))], ..PLAIN },
    Rule { consumer: UA, producer: Rb, fresh: &[UA, UA, F, S, F, L], wires: &[(X(0, 0), Px(2)), (X(0, 1), C(1)), (X(0, 2), X(1, 1)), (X(0, 3), X(4, 2)), (X(1, 0), Px(1)), (X(1, 2), C(2)), (X(1, 3), X(2, 2)), (X(2, 1), X(3, 0)), (X(3, 1), X(4, 0)), (X(4, 1), X(5, 0)), (X(2, 0), C(3))], ..PLAIN },
    Rule { consumer: UA, producer: Rc, fresh: &[UA, UA, F, S, F, L], wires: &[(X(0, 0), Px(2)), (X(0, 1), C(1)), (X(0, 2), X(4, 2)), (X(0, 3), X(1, 1)), (X(1, 0), Px(1)), (X(1, 2), C(2)), (X(1, 3), X(3, 1)), (X(2, 1), X(3, 0)), (X(2, 2), X(4, 0)), (X(4, 1), X(5, 0)), (X(2, 0), C(3))], ..PLAIN },
    Rule { consumer: Mg, producer: L, fresh: &[MgL], wires: &[(X(0, 0), C(1)), (X(0, 1), C(2))], ..PLAIN },
    Rule { consumer: MgL, producer: L, fresh: &[L], wires: &[(X(0, 0), C(1))], ..PLAIN },
    Rule { consumer: Mg, producer: S, fresh: &[MgS], wires: &[(X(0, 0), C(1)), (X(0, 1), Px(1)), (X(0, 2), C(2))], ..PLAIN },
    Rule { consumer: MgS, producer: S, fresh: &[Mg, S], wires: &[(X(0, 0), C(1)), (X(0, 1), Px(1)), (X(0, 2), X(1, 1)), (X(1, 0), C(2))], ..PLAIN },
    Rule { consumer: Mg, producer: F, fresh: &[MgF], wires: &[(X(0, 0), C(1)), (X(0, 1), Px(1)), (X(0, 2), Px(2)), (X(0, 3), C(2))], ..PLAIN },
    Rule { consumer: MgF, producer: F, fresh: &[Mg, Mg, F], wires: &[(X(0, 0), C(1)), (X(0, 1), Px(1)), (X(1, 0), C(2)), (X(1, 1), Px(2)), (X(0, 2), X(2, 1)), (X(1, 2), X(2, 2)), (X(2, 0), C(3))], ..PLAIN },
    // ---- erasing history ----
    Rule { consumer: Eps, producer: Arms3, fresh: &[Eps, Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)), (X(2, 0), Px(3))], ..PLAIN },
    Rule { consumer: Eps, producer: Rk, fresh: &[Eps], wires: &[(X(0, 0), Px(1))], ..PLAIN },
    Rule { consumer: Eps, producer: Rs, fresh: &[Eps, Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)), (X(2, 0), Px(3))], ..PLAIN },
    Rule { consumer: Eps, producer: Rt, fresh: &[Eps, Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2)), (X(2, 0), Px(3))], ..PLAIN },
    Rule { consumer: Eps, producer: Rb, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))], ..PLAIN },
    Rule { consumer: Eps, producer: Rc, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))], ..PLAIN },
    // ---- a value of the loaded term holding applications, taken apart running back (resid.rs) ----
    Rule { consumer: Fm, producer: F, fresh: &[], wires: &[(C(1), Px(1)), (C(2), Px(2))], ..PLAIN },
    Rule { consumer: Sm, producer: S, fresh: &[], wires: &[(C(1), Px(1))], ..PLAIN },
];

/// Movement agents (research/LINEAR_TREE_NETS.md §4a): six views of fixed tree shapes the loader
/// finds in code (net.rs `build_views`), each standing for its tree: `K y` = △△y, `Sx s b` = △(△s)b,
/// `B f g` = △(△(△△f))g, `C f g` = △(△f)(△△g), `I` = △(△△△)△ and `W s` = △(△s)s. No payload: every
/// rule is fixed by the two kinds meeting, as in `RULES`.
/// - applying one is one rewrite, the tree's rules composed (A·F, T1, Unp·Pair) less the copy and
///   erasure of an argument only one side reads: B and C move it, I passes it on unforced, and W
///   works `s c` out once and copies the result;
/// - a duplicator copies one and an eraser erases one in one rewrite, as they do a fork;
/// - a triage, dispatch or normalizer reading one spells it out: the reader again, facing its tree
///   (fresh agent 1 is the root). So reflection and the answer see plain trees.
pub static VIEW_RULES: &[Rule] = &[
    // ---- A·view: applied to c ----
    // K y c = y, c erased.
    Rule { consumer: A, producer: K, fresh: &[Eps], wires: &[(Px(1), C(2)), (X(0, 0), C(1))], ..PLAIN },
    // Sx s b c = s c (b c), c copied. fresh: dn, a(s c), a(b c), a.
    Rule { consumer: A, producer: Sx, fresh: &[Dn, A, A, A],
        wires: &[(X(0, 0), C(1)), (X(1, 0), Px(1)), (X(1, 1), X(0, 2)), (X(2, 0), Px(2)), (X(2, 1), X(0, 1)),
                 (X(3, 0), X(1, 2)), (X(3, 1), X(2, 2)), (X(3, 2), C(2))], ..PLAIN },
    // B f g c = f (g c). fresh: a(f …), a(g c).
    Rule { consumer: A, producer: B, fresh: &[A, A],
        wires: &[(X(0, 0), Px(1)), (X(0, 1), X(1, 2)), (X(0, 2), C(2)), (X(1, 0), Px(2)), (X(1, 1), C(1))], ..PLAIN },
    // C f g c = f c g. fresh: a(f c), a(… g).
    Rule { consumer: A, producer: Tag::C, fresh: &[A, A],
        wires: &[(X(0, 0), Px(1)), (X(0, 1), C(1)), (X(1, 0), X(0, 2)), (X(1, 1), Px(2)), (X(1, 2), C(2))], ..PLAIN },
    // I c = c.
    Rule { consumer: A, producer: I, fresh: &[], wires: &[(C(1), C(2))], ..PLAIN },
    // W s c = t t, t = s c worked out once. fresh: a(s c), dn, a.
    Rule { consumer: A, producer: W, fresh: &[A, Dn, A],
        wires: &[(X(0, 0), Px(1)), (X(0, 1), C(1)), (X(1, 0), X(0, 2)), (X(2, 0), X(1, 1)), (X(2, 1), X(1, 2)), (X(2, 2), C(2))], ..PLAIN },
    // ---- Dn·view: two copies in one rewrite, duplicators (of its label) on its children ----
    Rule { consumer: Dn, producer: K, fresh: &[Dn, K, K], labels: &[LC, L0, L0], wires: COPY_ONE, ..PLAIN },
    Rule { consumer: Dn, producer: Sx, fresh: &[Dn, Dn, Sx, Sx], labels: &[LC, LC, L0, L0], wires: COPY_TWO, ..PLAIN },
    Rule { consumer: Dn, producer: B, fresh: &[Dn, Dn, B, B], labels: &[LC, LC, L0, L0], wires: COPY_TWO, ..PLAIN },
    Rule { consumer: Dn, producer: Tag::C, fresh: &[Dn, Dn, Tag::C, Tag::C], labels: &[LC, LC, L0, L0], wires: COPY_TWO, ..PLAIN },
    Rule { consumer: Dn, producer: I, fresh: &[I, I], wires: &[(X(0, 0), C(1)), (X(1, 0), C(2))], ..PLAIN },
    Rule { consumer: Dn, producer: W, fresh: &[Dn, W, W], labels: &[LC, L0, L0], wires: COPY_ONE, ..PLAIN },
    // ---- Eps·view: erased in one rewrite, erasers on its children ----
    Rule { consumer: Eps, producer: K, fresh: &[Eps], wires: &[(X(0, 0), Px(1))], ..PLAIN },
    Rule { consumer: Eps, producer: Sx, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))], ..PLAIN },
    Rule { consumer: Eps, producer: B, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))], ..PLAIN },
    Rule { consumer: Eps, producer: Tag::C, fresh: &[Eps, Eps], wires: &[(X(0, 0), Px(1)), (X(1, 0), Px(2))], ..PLAIN },
    Rule { consumer: Eps, producer: I, fresh: &[], wires: &[], ..PLAIN },
    Rule { consumer: Eps, producer: W, fresh: &[Eps], wires: &[(X(0, 0), Px(1))], ..PLAIN },
    // ---- T1·view: spelled out, the triage again on its tree ----
    // △△y = F(L, y).
    Rule { consumer: T1, producer: K, fresh: &[T1, F, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(1, 2), Px(1)), (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    // △(△s)b = F(S(s), b).
    Rule { consumer: T1, producer: Sx, fresh: &[T1, F, S],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(1, 2), Px(2)), (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    // △(△(△△f))g = F(S(F(L, f)), g).
    Rule { consumer: T1, producer: B, fresh: &[T1, F, S, F, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), X(3, 0)), (X(3, 1), X(4, 0)), (X(3, 2), Px(1)), (X(1, 2), Px(2)),
                 (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    // △(△f)(△△g) = F(S(f), F(L, g)).
    Rule { consumer: T1, producer: Tag::C, fresh: &[T1, F, S, F, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(1, 2), X(3, 0)), (X(3, 1), X(4, 0)), (X(3, 2), Px(2)),
                 (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    // △(△△△)△ = F(F(L, L), L).
    Rule { consumer: T1, producer: I, fresh: &[T1, F, F, L, L, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), X(3, 0)), (X(2, 2), X(4, 0)), (X(1, 2), X(5, 0)), (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    // △(△s)s = F(S(s), s), s read through a duplicator.
    Rule { consumer: T1, producer: W, fresh: &[T1, F, S, Dn],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), X(3, 1)), (X(1, 2), X(3, 2)), (X(3, 0), Px(1)), (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    // ---- Sel·view: spelled out the same way ----
    Rule { consumer: Sel, producer: K, fresh: &[Sel, F, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(1, 2), Px(1)), (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    Rule { consumer: Sel, producer: Sx, fresh: &[Sel, F, S],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(1, 2), Px(2)), (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    Rule { consumer: Sel, producer: B, fresh: &[Sel, F, S, F, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), X(3, 0)), (X(3, 1), X(4, 0)), (X(3, 2), Px(1)), (X(1, 2), Px(2)),
                 (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    Rule { consumer: Sel, producer: Tag::C, fresh: &[Sel, F, S, F, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(1, 2), X(3, 0)), (X(3, 1), X(4, 0)), (X(3, 2), Px(2)),
                 (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    Rule { consumer: Sel, producer: I, fresh: &[Sel, F, F, L, L, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), X(3, 0)), (X(2, 2), X(4, 0)), (X(1, 2), X(5, 0)), (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    Rule { consumer: Sel, producer: W, fresh: &[Sel, F, S, Dn],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), X(3, 1)), (X(1, 2), X(3, 2)), (X(3, 0), Px(1)), (X(0, 1), C(1)), (X(0, 2), C(2))], ..PLAIN },
    // ---- Nrm·view: spelled out, to be normalized ----
    Rule { consumer: Nrm, producer: K, fresh: &[Nrm, F, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(1, 2), Px(1)), (X(0, 1), C(1))], ..PLAIN },
    Rule { consumer: Nrm, producer: Sx, fresh: &[Nrm, F, S],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(1, 2), Px(2)), (X(0, 1), C(1))], ..PLAIN },
    Rule { consumer: Nrm, producer: B, fresh: &[Nrm, F, S, F, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), X(3, 0)), (X(3, 1), X(4, 0)), (X(3, 2), Px(1)), (X(1, 2), Px(2)), (X(0, 1), C(1))], ..PLAIN },
    Rule { consumer: Nrm, producer: Tag::C, fresh: &[Nrm, F, S, F, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), Px(1)), (X(1, 2), X(3, 0)), (X(3, 1), X(4, 0)), (X(3, 2), Px(2)), (X(0, 1), C(1))], ..PLAIN },
    Rule { consumer: Nrm, producer: I, fresh: &[Nrm, F, F, L, L, L],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), X(3, 0)), (X(2, 2), X(4, 0)), (X(1, 2), X(5, 0)), (X(0, 1), C(1))], ..PLAIN },
    Rule { consumer: Nrm, producer: W, fresh: &[Nrm, F, S, Dn],
        wires: &[(X(0, 0), X(1, 0)), (X(1, 1), X(2, 0)), (X(2, 1), X(3, 1)), (X(1, 2), X(3, 2)), (X(3, 0), Px(1)), (X(0, 1), C(1))], ..PLAIN },
];

/// How Dn·S copies one child and Dn·F two: a duplicator on each child, its copies under the two copies.
const COPY_ONE: &[(End, End)] = &[(X(0, 0), Px(1)), (X(0, 1), X(1, 1)), (X(0, 2), X(2, 1)), (X(1, 0), C(1)), (X(2, 0), C(2))];
const COPY_TWO: &[(End, End)] = &[(X(0, 0), Px(1)), (X(1, 0), Px(2)), (X(0, 1), X(2, 1)), (X(1, 1), X(2, 2)), (X(0, 2), X(3, 1)), (X(1, 2), X(3, 2)),
                                  (X(2, 0), C(1)), (X(3, 0), C(2))];

/// Every rule but the movement agents', in `rule`'s numbering.
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
    if consumer.is_resid() || producer.is_resid() {
        return RESID_RULES.iter().position(|r| r.consumer == consumer && r.producer == producer).map(|i| N_ALL_RULES + i);
    }
    if producer.is_view() {
        return VIEW_RULES.iter().position(|r| r.consumer == consumer && r.producer == producer && r.applies(cl, pl)).map(|i| N_RULES + i);
    }
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
        if let Some(t) = r.fresh.iter().find(|t| **t == Out || t.is_view()) { return Err(format!("{rn}: fresh {} not spawnable", t.name())); }
        matching(r, &rn)?;
        labelled(r, &rn)?;
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

/// The movement agents' rules: each a perfect matching and labelled as `validate` checks, views made
/// only by copying one, and exactly one rule for every reader of every view.
pub fn validate_views() -> Result<(), String> {
    for r in VIEW_RULES {
        let rn = format!("{}·{}", r.consumer.name(), r.producer.name());
        if !r.consumer.is_consumer() || !r.producer.is_view() { return Err(format!("{rn}: not a view rule")); }
        if r.fresh.contains(&Out) { return Err(format!("{rn}: fresh Out not spawnable")); }
        if r.consumer != Dn && r.fresh.iter().any(|t| t.is_view()) { return Err(format!("{rn}: makes a view without copying one")); }
        if r.when != When::Any { return Err(format!("{rn}: only Dn·Sup looks at labels")); }
        matching(r, &rn)?;
        labelled(r, &rn)?;
    }
    for c in [A, T1, Sel, Nrm, Dn, Eps] {
        for v in VIEW_TAGS {
            let n = VIEW_RULES.iter().filter(|r| (r.consumer, r.producer) == (c, v)).count();
            if n != 1 { return Err(format!("{n} rules for {}·{}", c.name(), v.name())); }
        }
    }
    Ok(())
}

/// Keeping history's rules: each a perfect matching, plain, and one for every pair of kinds.
pub fn validate_resid() -> Result<(), String> {
    for r in RESID_RULES {
        let rn = format!("{}·{}", r.consumer.name(), r.producer.name());
        if !r.consumer.is_consumer() || !r.producer.is_producer() { return Err(format!("{rn}: not a consumer meeting a producer")); }
        if !r.labels.is_empty() || r.when != When::Any { return Err(format!("{rn}: labelled")); }
        if r.fresh.len() > 6 { return Err(format!("{rn}: more than six fresh agents")); }
        if r.fresh.iter().any(|t| matches!(t, Out | Gate) || t.is_view()) { return Err(format!("{rn}: fresh agent not spawnable")); }
        matching(r, &rn)?;
        if RESID_RULES.iter().filter(|o| (o.consumer, o.producer) == (r.consumer, r.producer)).count() != 1 { return Err(format!("{rn}: two rules")); }
    }
    Ok(())
}

/// Labels go to duplicators and superpositions only, from an agent that has one, and a fresh
/// superposition always has its label.
fn labelled(r: &Rule, rn: &str) -> Result<(), String> {
    if !r.labels.is_empty() && r.labels.len() != r.fresh.len() { return Err(format!("{rn}: labels for some fresh agents only")); }
    for (k, t) in r.fresh.iter().enumerate() {
        let lab = r.labels.get(k).copied().unwrap_or(L0);
        if lab != L0 && !matches!(t, Dn | Sup) { return Err(format!("{rn}: fresh[{k}] = {} labelled", t.name())); }
        if lab == LC && r.consumer != Dn || lab == LP && r.producer != Sup { return Err(format!("{rn}: fresh[{k}] labelled from a plain agent")); }
        if *t == Sup && lab != LP { return Err(format!("{rn}: a fresh Sup without the superposition's label")); }
    }
    Ok(())
}

/// The wire endpoints of a rule are a perfect matching over its live ports.
fn matching(r: &Rule, rn: &str) -> Result<(), String> {
    let mut expected: Vec<End> = vec![];
    for i in 1..r.consumer.arity() { expected.push(C(i as u8)); }
    for j in 1..r.producer.arity() { expected.push(Px(j as u8)); }
    for (k, t) in r.fresh.iter().enumerate() {
        if *t == Out { return Err(format!("{rn}: fresh[{k}] = {} not spawnable", t.name())); }
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

    #[test]
    fn view_rules_are_well_formed() {
        super::validate_views().expect("the movement agents' rules must validate");
        assert_eq!(super::VIEW_RULES.len(), 36);
    }

    #[test]
    fn resid_rules_are_well_formed() {
        super::validate_resid().expect("keeping history's rules must validate");
        assert_eq!(super::RESID_RULES.len(), 54);
    }
}
