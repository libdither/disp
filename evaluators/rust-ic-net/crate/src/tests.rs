//! Worked-example reduction tests (tc-net.typ §Worked Example) + the δˢ/δⁿ work-sharing
//! micro-benchmark + the M2c parallel race detector (1 vs 4 threads must agree on NF AND
//! interaction count — strong confluence, Theorem 2).

use crate::net::{Ctx, Net, Worker};
use crate::port::*;
use std::sync::atomic::Ordering;

// A net + worker for a test, small fixed arena.
fn net() -> Net {
    Net::new(1 << 16, 1 << 16)
}
fn build_not(c: &mut Ctx) -> u32 {
    let sl = c.stem(pack(L, 0)); // S(L) = true
    let fll = c.fork(pack(L, 0), pack(L, 0)); // F(L,L)
    let inner = c.fork(sl, fll); // F(S(L), F(L,L))
    c.fork(inner, pack(L, 0)) // F(.., L) = not
}
fn nf_string(c: &mut Ctx, h: u32) -> String {
    let mut b = 100_000_000i64;
    let nf = c.full_nf(h, &mut b).expect("nf");
    let mut out = Vec::new();
    c.emit(nf, &mut out);
    String::from_utf8(out).unwrap()
}

#[test]
fn not_false_is_true() {
    let n = net();
    let mut w = Worker::new();
    let mut c = n.ctx(&mut w);
    let not = build_not(&mut c);
    let app = c.susp(not, pack(L, 0));
    assert_eq!(nf_string(&mut c, app), "10"); // S(L) = true
}

#[test]
fn not_true_is_false() {
    let n = net();
    let mut w = Worker::new();
    let mut c = n.ctx(&mut w);
    let not = build_not(&mut c);
    let t = c.stem(pack(L, 0));
    let app = c.susp(not, t);
    assert_eq!(nf_string(&mut c, app), "0"); // L = false
}

#[test]
fn k_discards_second() {
    let n = net();
    let mut w = Worker::new();
    let mut c = n.ctx(&mut w);
    let k = c.stem(pack(L, 0));
    let x = c.fork(pack(L, 0), pack(L, 0));
    let y = c.stem(pack(L, 0));
    let kx = c.susp(k, x);
    let kxy = c.susp(kx, y);
    assert_eq!(nf_string(&mut c, kxy), "200"); // F(L,L)
}

#[test]
fn ternary_roundtrip_via_parse() {
    let n = net();
    let mut w = Worker::new();
    let mut c = n.ctx(&mut w);
    let s = b"210200";
    let mut i = 0usize;
    let h = c.parse(s, &mut i);
    assert_eq!(i, s.len());
    let mut b = 1_000_000i64;
    let nf = c.full_nf(h, &mut b).unwrap();
    let mut out = Vec::new();
    c.emit(nf, &mut out);
    assert_eq!(&out, s);
}

// S-rule (T1 ⊗ S → δⁿ): △(△L) L (Stem(L)) → "210110".
#[test]
fn s_rule_dup() {
    let n = net();
    let mut w = Worker::new();
    let mut c = n.ctx(&mut w);
    let op = {
        let sl = c.stem(pack(L, 0));
        c.fork(sl, pack(L, 0))
    };
    let arg = c.stem(pack(L, 0));
    let app = c.susp(op, arg);
    assert_eq!(nf_string(&mut c, app), "210110");
}

// δⁿ ⊗ P (parking): duplicate a SUSPENSION (not false); δⁿ⊗P must fire (dnp ≥ 1).
#[test]
fn dn_parks_on_suspension() {
    let n = net();
    let mut w = Worker::new();
    let mut c = n.ctx(&mut w);
    let not = build_not(&mut c);
    let susp = c.susp(not, pack(L, 0)); // (not false) reduces to true
    let op = {
        let sl = c.stem(pack(L, 0));
        c.fork(sl, pack(L, 0))
    };
    let app = c.susp(op, susp);
    assert_eq!(nf_string(&mut c, app), "210110");
    assert!(n.dnp.load(Ordering::Relaxed) >= 1, "δⁿ⊗P should fire");
}

// Theorem-6 work-sharing: duplicate one expensive suspension; δⁿ < δˢ interactions.
fn dup_cost(species: u32, k: usize) -> u64 {
    let n = net();
    let mut w = Worker::new();
    let mut c = n.ctx(&mut w);
    let mut e = pack(L, 0);
    for _ in 0..k {
        let not = build_not(&mut c); // FRESH not per level — a linear chain
        e = c.susp(not, e);
    }
    let (lv, rv) = (c.new_var(), c.new_var());
    let d = c.alloc(&[pack(VAR, lv), pack(VAR, rv)]);
    c.link(pack(species, d), e);
    for v in [lv, rv] {
        let res = c.new_var();
        let nn = c.alloc(&[pack(VAR, res)]);
        c.link(pack(N, nn), pack(VAR, v));
    }
    let mut b = 1_000_000_000i64;
    c.drain(&mut b);
    n.interactions.load(Ordering::Relaxed)
}

#[test]
fn dn_shares_what_ds_redoes() {
    let dn = dup_cost(DN, 12);
    let ds = dup_cost(DS, 12);
    // (Measured k=12: δⁿ≈111 vs δˢ≈327 interactions, ~2.95×.)
    assert!(dn < ds, "δⁿ ({dn}) should do strictly less work than δˢ ({ds})");
}

#[test]
fn equal_nf() {
    let n = net();
    let mut w = Worker::new();
    let mut c = n.ctx(&mut w);
    let not = build_not(&mut c);
    let nf_app = c.susp(not, pack(L, 0)); // → true
    let tru = c.stem(pack(L, 0));
    let mut b = 1_000_000i64;
    assert_eq!(c.equal(nf_app, tru, &mut b), Some(true));
    let mut b2 = 1_000_000i64;
    assert_eq!(c.equal(nf_app, pack(L, 0), &mut b2), Some(false));
}

// M2c race detector: a balanced fork tree of independent not^4-chains has many
// independent N redexes; reducing with 1 vs 4 threads must give the SAME NF and the
// SAME interaction count (strong confluence). Run many times — concurrency bugs flake.
#[cfg(not(target_arch = "wasm32"))]
#[test]
fn parallel_matches_sequential() {
    fn build(c: &mut Ctx, depth: usize) -> u32 {
        if depth == 0 {
            let mut e = pack(L, 0); // false
            for _ in 0..4 {
                let not = build_not(c);
                e = c.susp(not, e); // not^4 false
            }
            e
        } else {
            let l = build(c, depth - 1);
            let r = build(c, depth - 1);
            c.fork(l, r)
        }
    }
    for _ in 0..20 {
        let nseq = Net::new(1 << 20, 1 << 20);
        let (seq_nf, seq_int) = {
            let mut w = Worker::new();
            let mut c = nseq.ctx(&mut w);
            let h = build(&mut c, 6);
            let mut b = 1_000_000_000i64;
            let nf = c.full_nf(h, &mut b).expect("seq nf");
            let mut o = Vec::new();
            c.emit(nf, &mut o);
            (o, nseq.interactions.load(Ordering::Relaxed))
        };
        let npar = Net::new(1 << 20, 1 << 20);
        let h = {
            let mut w = Worker::new();
            let mut c = npar.ctx(&mut w);
            build(&mut c, 6)
        };
        let nf = npar.full_nf_parallel(h, 4, 1_000_000_000).expect("par nf");
        let par_nf = {
            let mut w = Worker::new();
            let c = npar.ctx(&mut w);
            let mut o = Vec::new();
            c.emit(nf, &mut o);
            o
        };
        let par_int = npar.interactions.load(Ordering::Relaxed);
        assert_eq!(seq_nf, par_nf, "parallel NF must equal sequential");
        assert_eq!(seq_int, par_int, "interaction count deterministic across threads");
    }
}
