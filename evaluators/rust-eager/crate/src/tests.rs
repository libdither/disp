//! Worked-example reduction tests (tc-net.typ §Worked Example) — exercise the eager
//! reducer, the M1 lazy core, the ternary codec, and eager↔lazy confluence.

use crate::arena::{Arena, LEAF_ID};

// false = △ = L;  true = △△ = S(L)
// not = △ (△ (△△) (△△△)) △ = F(F(S(L), F(L,L)), L)
fn build_not(a: &mut Arena) -> u32 {
    let l = LEAF_ID;
    let sl = a.stem(l); // S(L) = true
    let fll = a.fork(l, l); // F(L,L)
    let inner = a.fork(sl, fll); // F(S(L), F(L,L))
    a.fork(inner, l) // F(.., L) = not
}

#[test]
fn eager_not_false_is_true() {
    let mut a = Arena::new();
    let not = build_not(&mut a);
    let mut budget = 1_000_000i64;
    let r = a.reduce(not, LEAF_ID, &mut budget).ok().unwrap();
    let expect_true = a.stem(LEAF_ID);
    assert_eq!(r, expect_true, "not false = true");
}

#[test]
fn eager_not_true_is_false() {
    let mut a = Arena::new();
    let not = build_not(&mut a);
    let t = a.stem(LEAF_ID); // true
    let mut budget = 1_000_000i64;
    let r = a.reduce(not, t, &mut budget).ok().unwrap();
    assert_eq!(r, LEAF_ID, "not true = false");
}

// Same examples through the LAZY core (susp, then nf forces).
#[test]
fn lazy_not_false_is_true() {
    let mut a = Arena::new();
    let not = build_not(&mut a);
    let app = a.susp(not, LEAF_ID); // apply(not, false), suspended
    let mut budget = 1_000_000i64;
    let r = a.nf(app, &mut budget).ok().unwrap();
    let expect_true = a.stem(LEAF_ID);
    assert_eq!(r, expect_true);
}

#[test]
fn lazy_not_true_is_false() {
    let mut a = Arena::new();
    let not = build_not(&mut a);
    let t = a.stem(LEAF_ID);
    let app = a.susp(not, t);
    let mut budget = 1_000_000i64;
    let r = a.nf(app, &mut budget).ok().unwrap();
    assert_eq!(r, LEAF_ID);
}

// K x y = x ;  K = △△ = S(L)
#[test]
fn lazy_k_discards_second() {
    let mut a = Arena::new();
    let kk = a.stem(LEAF_ID); // K = △△
    let x = a.fork(LEAF_ID, LEAF_ID); // some value
    let y = a.stem(LEAF_ID);
    let kx = a.susp(kk, x); // K x
    let kxy = a.susp(kx, y); // (K x) y → x
    let mut budget = 1_000_000i64;
    let r = a.nf(kxy, &mut budget).ok().unwrap();
    let xnf = a.nf(x, &mut budget).ok().unwrap();
    assert_eq!(r, xnf, "K x y = x");
}

#[test]
fn ternary_roundtrip() {
    let mut a = Arena::new();
    let s = b"210200"; // F(S(L), F(L,L))
    let mut i = 0usize;
    let h = a.parse(s, &mut i);
    assert_eq!(i, s.len());
    let mut out = Vec::new();
    a.encode(h, &mut out);
    assert_eq!(&out, s);
}

// Eager and lazy must agree on NF (confluence).
#[test]
fn eager_lazy_agree() {
    let mut a = Arena::new();
    let not = build_not(&mut a);
    let t = a.stem(LEAF_ID);
    for x in [LEAF_ID, t] {
        let mut b1 = 1_000_000i64;
        let eager = a.reduce(not, x, &mut b1).ok().unwrap();
        let app = a.susp(not, x);
        let mut b2 = 1_000_000i64;
        let lazy = a.nf(app, &mut b2).ok().unwrap();
        assert_eq!(eager, lazy, "eager vs lazy NF disagree");
    }
}
