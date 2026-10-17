//! Port polarity: every wire joins one value SOURCE to one SINK. The mesh stores a wire
//! once, as a reference held by its sink, so polarity decides who holds what.
//!
//! Sources: producer principals, consumer result ports. Sinks: everything else. A sink is
//! the unique reader of the source it references (linearity), which is what makes the
//! whole protocol lock-free: every remote operation on a source comes from one place.

use rust_ca_lattice::rules::{all_rules, End, Rule, Tag, ALL_TAGS, FORGET_RULES, RESID_RULES, RESID_TAGS, TWO_RULES, TWO_TAGS, VIEW_RULES, VIEW_TAGS};

/// Is port `p` of `tag` a source (value flows out of it)?
pub const fn is_source(tag: Tag, p: usize) -> bool {
    match tag {
        Tag::L | Tag::S | Tag::F | Tag::P | Tag::Pair | Tag::Sup => p == 0,
        Tag::K | Tag::Sx | Tag::B | Tag::C | Tag::I | Tag::W => p == 0,
        Tag::A | Tag::T1 | Tag::Sel => p == 2,
        Tag::Nrm => p == 1,
        Tag::Unp | Tag::Dn => p == 1 || p == 2,
        Tag::Eps | Tag::Out | Tag::Seam | Tag::Go | Tag::Back => false,
        Tag::Arms3 | Tag::Rk | Tag::Rs | Tag::Rt | Tag::Rb | Tag::Rc | Tag::Ha | Tag::Hole | Tag::Rh | Tag::Rq => p == 0,
        Tag::RA | Tag::RT1 | Tag::RSel | Tag::SS | Tag::SF | Tag::T1s | Tag::T1sf | Tag::T1sc0 | Tag::T1sc | Tag::T1scf | Tag::TSb
        | Tag::UA | Tag::USel | Tag::USS | Tag::USF => p == 2 || p == 3,
        Tag::SL | Tag::UAr | Tag::USL | Tag::Ev => p == 1 || p == 2,
        Tag::Mg | Tag::MgS | Tag::UEv => p == 2,
        Tag::MgL | Tag::Fg | Tag::UArms => p == 1,
        Tag::MgF => p == 3,
        Tag::Swap | Tag::AssocL | Tag::AssocR | Tag::StemFork | Tag::ForkStem | Tag::Uniti | Tag::Unite | Tag::Copy | Tag::Merge
        | Tag::Eval | Tag::Flip | Tag::Seq | Tag::Par | Tag::Cond | Tag::Loop => p == 0,
        Tag::Run | Tag::Lk | Tag::Lk2 => p == 2 || p == 3,
        Tag::Split => p == 1 || p == 2,
        Tag::SplitS | Tag::MrgL | Tag::Fl => p == 1,
        Tag::Eat => false,
        Tag::Cd | Tag::MrgF => p == 3,
        Tag::Mrg | Tag::MrgS => p == 2,
    }
}

/// How a rule endpoint behaves at fire time: `Have` supplies a source reference, `Need`
/// receives one. A dying sink port supplies the reference it held; a dying source port
/// needs a new source for its outside reader.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Role {
    Have,
    Need,
}

pub fn role(rule: &Rule, e: End) -> Role {
    let src = match e {
        End::Fresh(k, p) => is_source(rule.fresh[k as usize], p as usize),
        End::CAux(i) => !is_source(rule.consumer, i as usize),
        End::PAux(j) => !is_source(rule.producer, j as usize),
    };
    if src { Role::Have } else { Role::Need }
}

/// The polarity lemma, checked over the whole ROM, movement agents included: every template wire pairs a `Have` with
/// a `Need`, so every rewrite keeps "one source, one sink" per wire.
pub fn validate() -> Result<(), String> {
    for r in all_rules().chain(VIEW_RULES).chain(RESID_RULES).chain(FORGET_RULES).chain(TWO_RULES) {
        for (a, b) in r.wires {
            if role(r, *a) == role(r, *b) {
                return Err(format!(
                    "{}·{}: wire {a:?}–{b:?} joins two {:?}",
                    r.consumer.name(), r.producer.name(), role(r, *a)
                ));
            }
        }
    }
    for t in ALL_TAGS.into_iter().chain(VIEW_TAGS).chain(RESID_TAGS).chain(TWO_TAGS) {
        let sinks = (0..t.arity()).filter(|&p| !is_source(t, p)).count();
        if t.is_producer() && sinks != t.arity() - 1 {
            return Err(format!("{}: producers have exactly one source", t.name()));
        }
        if t.is_consumer() && is_source(t, 0) {
            return Err(format!("{}: a consumer principal is a sink", t.name()));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn rom_is_polarized() {
        super::validate().expect("every rule wire joins a source to a sink");
    }
}
