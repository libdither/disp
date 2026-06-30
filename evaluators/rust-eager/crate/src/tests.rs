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

// Scoped reclamation (Session.beginScope/endScope): keep-nothing fully rolls back; a kept
// survivor and its reachable subtree are preserved (ids stable) while interior+trailing
// garbage is reclaimed; hash-consing stays consistent (re-mint hits the survivor); freed
// slots are reused; and reduction over a survivor still works.
#[test]
fn scoped_reclamation() {
    let mut a = Arena::new();
    let k = a.stem(LEAF_ID); // K = S(L) — permanent base
    let base = a.node_count();

    // (1) keep-nothing → full rollback to the scope base.
    a.begin_scope();
    let t1 = a.stem(k);
    let t2 = a.fork(t1, k);
    let _ = a.fork(t2, t1);
    assert!(a.node_count() > base);
    a.end_scope(&[]);
    assert_eq!(a.node_count(), base, "keep-nothing rolls fully back to base");

    // (2) keep a survivor with a subtree; disconnected interior garbage + trailing garbage.
    a.begin_scope();
    let g1 = a.stem(k); // interior garbage (disconnected from the survivor)
    let _g2 = a.fork(g1, g1); // interior garbage
    let s0 = a.fork(k, LEAF_ID); // survivor's dep
    let s = a.stem(s0); // SURVIVOR root (reaches only s0, k, leaf — not g1/g2)
    let mut junk = a.fork(s, g1);
    for _ in 0..10 {
        junk = a.fork(junk, k); // trailing garbage (highest ids)
    }
    let top_before = a.node_count();
    assert!(top_before > base);
    a.end_scope(&[s]);

    // trailing garbage truncated; survivor + base intact via hash-cons re-mint.
    assert!(a.node_count() < top_before, "trailing garbage truncated");
    assert_eq!(a.fork(k, LEAF_ID), s0, "survivor dep re-mints to its id");
    assert_eq!(a.stem(s0), s, "survivor re-mints to its id");
    assert_eq!(a.stem(LEAF_ID), k, "permanent base intact");

    // freed interior slots are reused: a novel node does not grow the arena.
    let nc = a.node_count();
    let _fresh = a.fork(s, s); // novel — must reuse a freed hole (g1/g2's slot)
    assert_eq!(a.node_count(), nc, "novel alloc reused a freed slot (no growth)");

    // reduction over a survivor still works: I s = s (I rebuilt from base nodes).
    let ll = a.fork(LEAF_ID, LEAF_ID);
    let i = a.fork(ll, LEAF_ID); // I = F(F(L,L), L) — re-mints to the base I
    let mut b = 1_000_000i64;
    assert_eq!(a.reduce(i, s, &mut b).ok().unwrap(), s, "I s = s over a survivor");
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
