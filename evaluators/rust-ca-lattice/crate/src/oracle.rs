//! The independent oracle: a plain recursive normalizer for the two-level-triage tree
//! calculus, deliberately sharing NO code with the rule ROM or the engines. Differential
//! power comes from this independence (the same discipline as _engine_dev.mjs and the
//! project-wide tree_eq native-vs-reference validation).
//!
//!   apply(L, x)     = S x
//!   apply(S a, x)   = F a x
//!   apply(F a b, x) = triage(a, b, x)
//!   triage(L, b, c)      = b                                   (K)
//!   triage(S s, b, c)    = (b c)(s c)                          (S-rule, shares c)
//!   triage(F w x, b, c)  = c=L → w | c=S u → x u | c=F u v → (b u) v   (T2)

use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    L,
    S(Rc<Term>),
    F(Rc<Term>, Rc<Term>),
    Ap(Rc<Term>, Rc<Term>),
}
use Term::*;

pub fn ap(f: Term, x: Term) -> Term { Ap(Rc::new(f), Rc::new(x)) }
pub fn s(a: Term) -> Term { S(Rc::new(a)) }
pub fn f2(a: Term, b: Term) -> Term { F(Rc::new(a), Rc::new(b)) }

pub struct Fuel(pub u64);
#[derive(Debug)]
pub struct OutOfFuel;

fn whnf(t: Rc<Term>, fuel: &mut Fuel) -> Result<Rc<Term>, OutOfFuel> {
    let mut t = t;
    while let Ap(f, x) = &*t {
        if fuel.0 == 0 { return Err(OutOfFuel); }
        fuel.0 -= 1;
        let fw = whnf(f.clone(), fuel)?;
        t = match &*fw {
            L => Rc::new(S(x.clone())),
            S(a) => Rc::new(F(a.clone(), x.clone())),
            F(a, b) => triage(a.clone(), b.clone(), x.clone(), fuel)?,
            _ => return Ok(t),
        };
    }
    Ok(t)
}

fn triage(a: Rc<Term>, b: Rc<Term>, c: Rc<Term>, fuel: &mut Fuel) -> Result<Rc<Term>, OutOfFuel> {
    let av = whnf(a, fuel)?;
    match &*av {
        L => Ok(b),
        S(s) => Ok(Rc::new(Ap(Rc::new(Ap(b, c.clone())), Rc::new(Ap(s.clone(), c))))),
        F(w, x) => {
            let cv = whnf(c, fuel)?;
            match &*cv {
                L => Ok(w.clone()),
                S(u) => Ok(Rc::new(Ap(x.clone(), u.clone()))),
                F(u, v) => Ok(Rc::new(Ap(Rc::new(Ap(b, u.clone())), v.clone()))),
                _ => unreachable!("whnf returned an application"),
            }
        }
        _ => unreachable!("whnf returned an application"),
    }
}

pub fn nf(t: Term, fuel: &mut Fuel) -> Result<Term, OutOfFuel> {
    let w = whnf(Rc::new(t), fuel)?;
    Ok(match &*w {
        S(a) => S(Rc::new(nf((**a).clone(), fuel)?)),
        F(a, b) => F(Rc::new(nf((**a).clone(), fuel)?), Rc::new(nf((**b).clone(), fuel)?)),
        other => other.clone(),
    })
}

/// Same rendering as the JS harness, for cross-checkable transcripts.
pub fn show(t: &Term) -> String {
    let mut out = String::new();
    fn go(t: &Term, out: &mut String) {
        match t {
            L => out.push('L'),
            S(a) => { out.push_str("S("); go(a, out); out.push(')'); }
            F(a, b) => { out.push_str("F("); go(a, out); out.push(','); go(b, out); out.push(')'); }
            Ap(f, x) => { out.push_str("@("); go(f, out); out.push(','); go(x, out); out.push(')'); }
        }
    }
    go(t, &mut out);
    out
}

/// The shared LCG (same constants as the JS harnesses, for comparable corpora).
pub struct Lcg(pub i64);
impl Lcg {
    pub fn next(&mut self) -> f64 {
        self.0 = (self.0.wrapping_mul(1103515245).wrapping_add(12345)) & 0x7fffffff;
        self.0 as f64 / 0x7fffffff as f64
    }
    pub fn rand_term(&mut self, depth: u32) -> Term {
        if depth == 0 || self.next() < 0.35 { return L; }
        ap(self.rand_term(depth - 1), self.rand_term(depth - 1))
    }
}

/// A few structured builders shared by the pins across engines.
pub fn k() -> Term { s(L) } // K a b → a
pub fn chain_k(n: u32) -> Term { if n == 0 { L } else { ap(ap(k(), chain_k(n - 1)), L) } }
pub fn disp_t() -> Term { ap(ap(f2(f2(L, s(L)), L), s(L)), f2(L, L)) } // drives A→T1→Sel

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn oracle_self_tests() {
        // K false true = false (K = S(L); false = S(L); true = L)
        let t = ap(ap(k(), s(L)), L);
        assert_eq!(show(&nf(t, &mut Fuel(1000)).unwrap()), "S(L)");
        // fork dispatch: (F w x) L → w
        let t = ap(f2(L, L), L);
        assert_eq!(show(&nf(t, &mut Fuel(1000)).unwrap()), "L");
        // chain of K-redexes collapses to L
        assert_eq!(show(&nf(chain_k(6), &mut Fuel(10_000)).unwrap()), "L");
    }
}
