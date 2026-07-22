//! Persistent reduction cache: the arena's nodes + intern + apply memo in one flat
//! file, loaded back as a FROZEN base tier under a fresh session's overlay. Memo
//! entries are calculus-level facts (`apply(f,x) = r` under the fixed rules), so
//! kernel/library/elaborator changes never invalidate them — only an evaluator
//! change does, which the caller encodes in `stamp` (the harness passes a hash of
//! the addon binary). The frozen tables are probed AS STORED (same FxHash, same
//! open addressing) — no per-entry deserialization or rehash on load. Writers
//! COMPACT before calling `save` (arena.rs remaps live ids densely), so a stored
//! file has no holes.
//!
//! Layout v2 (little-endian, sections 8-byte-aligned):
//!   magic u64 | version u64 | checksum u64 | stamp_len u64 | node_len u64 |
//!   intern_cap u64 | memo_cap u64 | stamp (padded to 8) | nodes u64×node_len |
//!   intern u32×intern_cap | memo_f u32×memo_cap | memo_x u32×memo_cap |
//!   memo_r u32×memo_cap | memo_c u32×memo_cap
//! Intern slots hold id+1 (0 = empty); memo slots are empty when f = 0 (id 0 is
//! the reserved null sentinel, never a valid key); memo_c is the entry's
//! recomputation cost in fork-dispatches (drives save-time thresholds). Both
//! tables are power-of-two capacity at ≤ 0.5 load, linear probing. The checksum
//! is FxHash over every section's bytes (stamp + nodes + tables) — a flipped bit
//! in a "fact" would otherwise be trusted.

use rustc_hash::FxHasher;
use std::fs::{self, File};
use std::hash::{Hash, Hasher};
use std::io::{BufWriter, Read, Write};

pub(crate) const MAGIC: u64 = 0x4f4d454d50534944; // "DISPMEMO" (LE)
pub(crate) const VERSION: u64 = 2;

#[inline]
pub(crate) fn pair_hash(f: u32, x: u32) -> u64 {
    let mut h = FxHasher::default();
    (f, x).hash(&mut h);
    h.finish()
}

fn bytes_of_u64s(v: &[u64]) -> &[u8] {
    unsafe { std::slice::from_raw_parts(v.as_ptr() as *const u8, v.len() * 8) }
}
fn bytes_of_u32s(v: &[u32]) -> &[u8] {
    unsafe { std::slice::from_raw_parts(v.as_ptr() as *const u8, v.len() * 4) }
}

/// The loaded frozen tier. Owned buffers, indexed in place; ids are global arena
/// ids (the base occupies `0..node_len`, hole-free by construction).
pub(crate) struct FrozenBase {
    pub(crate) nodes: Vec<u64>,
    intern: Vec<u32>,
    memo_f: Vec<u32>,
    memo_x: Vec<u32>,
    memo_r: Vec<u32>,
    memo_c: Vec<u32>,
}

impl FrozenBase {
    #[inline]
    pub(crate) fn node_len(&self) -> u32 {
        self.nodes.len() as u32
    }
    /// Find an interned node by its packed word (same probe as the live table).
    #[inline]
    pub(crate) fn intern_find(&self, w: u64, h: u64) -> Option<u32> {
        if self.intern.is_empty() {
            return None;
        }
        let mask = self.intern.len() - 1;
        let mut i = (h as usize) & mask;
        loop {
            let slot = self.intern[i];
            if slot == 0 {
                return None;
            }
            let id = slot - 1;
            if self.nodes[id as usize] == w {
                return Some(id);
            }
            i = (i + 1) & mask;
        }
    }
    #[inline]
    pub(crate) fn memo_get(&self, f: u32, x: u32) -> Option<u32> {
        if self.memo_f.is_empty() {
            return None;
        }
        let mask = self.memo_f.len() - 1;
        let mut i = (pair_hash(f, x) as usize) & mask;
        loop {
            let sf = self.memo_f[i];
            if sf == 0 {
                return None;
            }
            if sf == f && self.memo_x[i] == x {
                return Some(self.memo_r[i]);
            }
            i = (i + 1) & mask;
        }
    }
    pub(crate) fn memo_len(&self) -> u32 {
        self.memo_f.iter().filter(|&&f| f != 0).count() as u32
    }
    /// Iterate frozen memo entries as (f, x, r, cost) — for re-saving.
    pub(crate) fn memo_iter(&self) -> impl Iterator<Item = (u32, u32, u32, u32)> + '_ {
        (0..self.memo_f.len()).filter_map(move |i| {
            let f = self.memo_f[i];
            (f != 0).then(|| (f, self.memo_x[i], self.memo_r[i], self.memo_c[i]))
        })
    }
    /// Iterate frozen interned ids — for re-saving.
    pub(crate) fn intern_iter(&self) -> impl Iterator<Item = u32> + '_ {
        self.intern.iter().filter(|&&s| s != 0).map(|&s| s - 1)
    }
}

fn read_u64s(f: &mut File, n: usize) -> std::io::Result<Vec<u64>> {
    let mut v = vec![0u64; n];
    // Read straight into the Vec's bytes: aligned by construction, one copy from
    // the kernel. Little-endian layout (x86-64 / aarch64 — the addon's hosts).
    let bytes = unsafe { std::slice::from_raw_parts_mut(v.as_mut_ptr() as *mut u8, n * 8) };
    f.read_exact(bytes)?;
    Ok(v)
}
fn read_u32s(f: &mut File, n: usize) -> std::io::Result<Vec<u32>> {
    let mut v = vec![0u32; n];
    let bytes = unsafe { std::slice::from_raw_parts_mut(v.as_mut_ptr() as *mut u8, n * 4) };
    f.read_exact(bytes)?;
    Ok(v)
}

pub(crate) fn load(path: &str, stamp: &[u8]) -> Result<FrozenBase, String> {
    let mut f = File::open(path).map_err(|e| e.to_string())?;
    let header = read_u64s(&mut f, 7).map_err(|e| e.to_string())?;
    if header[0] != MAGIC || header[1] != VERSION {
        return Err("bad magic/version".into());
    }
    let checksum = header[2];
    let stamp_len = header[3] as usize;
    let padded = stamp_len.div_ceil(8) * 8;
    let mut sbuf = vec![0u8; padded];
    f.read_exact(&mut sbuf).map_err(|e| e.to_string())?;
    if &sbuf[..stamp_len] != stamp {
        return Err("stamp mismatch".into());
    }
    let (node_len, intern_cap, memo_cap) = (header[4] as usize, header[5] as usize, header[6] as usize);
    if node_len >= (1 << 31) {
        return Err("node table exceeds id space".into());
    }
    let nodes = read_u64s(&mut f, node_len).map_err(|e| e.to_string())?;
    let intern = read_u32s(&mut f, intern_cap).map_err(|e| e.to_string())?;
    let memo_f = read_u32s(&mut f, memo_cap).map_err(|e| e.to_string())?;
    let memo_x = read_u32s(&mut f, memo_cap).map_err(|e| e.to_string())?;
    let memo_r = read_u32s(&mut f, memo_cap).map_err(|e| e.to_string())?;
    let memo_c = read_u32s(&mut f, memo_cap).map_err(|e| e.to_string())?;
    let mut h = FxHasher::default();
    sbuf.hash(&mut h);
    bytes_of_u64s(&nodes).hash(&mut h);
    bytes_of_u32s(&intern).hash(&mut h);
    bytes_of_u32s(&memo_f).hash(&mut h);
    bytes_of_u32s(&memo_x).hash(&mut h);
    bytes_of_u32s(&memo_r).hash(&mut h);
    bytes_of_u32s(&memo_c).hash(&mut h);
    if h.finish() != checksum {
        return Err("checksum mismatch (corrupt snapshot)".into());
    }
    Ok(FrozenBase { nodes, intern, memo_f, memo_x, memo_r, memo_c })
}

fn table_cap(n: usize) -> usize {
    (n.max(4) * 2).next_power_of_two()
}

/// Write a snapshot: a (compacted) node table plus intern/memo tables built from
/// the given entry iterators. Atomic via temp + rename. Returns (nodes, memo entries).
pub(crate) fn save(
    path: &str,
    stamp: &[u8],
    nodes: Vec<u64>,
    intern_ids: impl Iterator<Item = u32>,
    memo_entries: impl Iterator<Item = (u32, u32, u32, u32)>,
) -> Result<(u32, u32), String> {
    let word_hash = |w: u64| {
        let mut h = FxHasher::default();
        w.hash(&mut h);
        h.finish()
    };
    // Frozen intern: id+1 slots, probed by the packed word's hash.
    let ids: Vec<u32> = intern_ids.collect();
    let icap = table_cap(ids.len());
    let imask = icap - 1;
    let mut intern = vec![0u32; icap];
    for id in ids {
        let mut i = (word_hash(nodes[id as usize]) as usize) & imask;
        while intern[i] != 0 {
            i = (i + 1) & imask;
        }
        intern[i] = id + 1;
    }
    // Frozen memo: first writer wins (frozen entries precede live ones; `apply`
    // is pure, so a duplicate key always carries the same result).
    let entries: Vec<(u32, u32, u32, u32)> = memo_entries.collect();
    let mcap = table_cap(entries.len());
    let mmask = mcap - 1;
    let (mut mf, mut mx, mut mr, mut mc) =
        (vec![0u32; mcap], vec![0u32; mcap], vec![0u32; mcap], vec![0u32; mcap]);
    let mut memo_count = 0u32;
    for (f, x, r, c) in entries {
        let mut i = (pair_hash(f, x) as usize) & mmask;
        loop {
            if mf[i] == 0 {
                mf[i] = f;
                mx[i] = x;
                mr[i] = r;
                mc[i] = c;
                memo_count += 1;
                break;
            }
            if mf[i] == f && mx[i] == x {
                break;
            }
            i = (i + 1) & mmask;
        }
    }
    let padded = stamp.len().div_ceil(8) * 8;
    let mut sbuf = vec![0u8; padded];
    sbuf[..stamp.len()].copy_from_slice(stamp);
    let mut h = FxHasher::default();
    sbuf.hash(&mut h);
    bytes_of_u64s(&nodes).hash(&mut h);
    bytes_of_u32s(&intern).hash(&mut h);
    bytes_of_u32s(&mf).hash(&mut h);
    bytes_of_u32s(&mx).hash(&mut h);
    bytes_of_u32s(&mr).hash(&mut h);
    bytes_of_u32s(&mc).hash(&mut h);
    let checksum = h.finish();
    let tmp = format!("{path}.tmp");
    {
        let mut w = BufWriter::new(File::create(&tmp).map_err(|e| e.to_string())?);
        let hdr = [MAGIC, VERSION, checksum, stamp.len() as u64, nodes.len() as u64, icap as u64, mcap as u64];
        let wr = |w: &mut BufWriter<File>, b: &[u8]| w.write_all(b).map_err(|e| e.to_string());
        wr(&mut w, bytes_of_u64s(&hdr))?;
        wr(&mut w, &sbuf)?;
        wr(&mut w, bytes_of_u64s(&nodes))?;
        wr(&mut w, bytes_of_u32s(&intern))?;
        wr(&mut w, bytes_of_u32s(&mf))?;
        wr(&mut w, bytes_of_u32s(&mx))?;
        wr(&mut w, bytes_of_u32s(&mr))?;
        wr(&mut w, bytes_of_u32s(&mc))?;
        w.flush().map_err(|e| e.to_string())?;
    }
    fs::rename(&tmp, path).map_err(|e| e.to_string())?;
    Ok((nodes.len() as u32, memo_count))
}
