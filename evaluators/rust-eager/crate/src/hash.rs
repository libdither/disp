//! Deterministic, dependency-free hashing for the hash-cons + memo tables.
//!
//! FxHash-style (rotate-xor-multiply per word). Two reasons over std's default:
//! wasm32-unknown-unknown has no entropy for SipHash's `RandomState`, and SipHash is
//! ~3-5× slower than this on the small (u32 / `Node`) keys the intern + memo tables
//! hash on *every* reduction — the hottest structures in the evaluator.

use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};

pub(crate) const FX_K: u64 = 0x51_7c_c1_b7_27_22_0a_95;

#[derive(Default)]
pub(crate) struct FxHasher(u64);

impl FxHasher {
    #[inline]
    fn add(&mut self, i: u64) {
        self.0 = (self.0.rotate_left(5) ^ i).wrapping_mul(FX_K);
    }
}

impl Hasher for FxHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }
    // Fallback (e.g. the enum discriminant); the typed paths below cover the keys.
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        for chunk in bytes.chunks(8) {
            let mut b = [0u8; 8];
            b[..chunk.len()].copy_from_slice(chunk);
            self.add(u64::from_le_bytes(b));
        }
    }
    #[inline]
    fn write_u32(&mut self, i: u32) {
        self.add(i as u64)
    }
    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.add(i)
    }
    #[inline]
    fn write_usize(&mut self, i: usize) {
        self.add(i as u64)
    }
}

/// A `HashMap` keyed by `FxHasher` — the memo/forced tables' map type.
pub(crate) type Map<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;
