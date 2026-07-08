//! E1 instrumentation (research/interaction-combinator/SPATIAL_IC.md §10): rung A
//! address-distance histograms + rung C interaction-DAG event log.
//!
//! Rung C logs one record per interaction: `(consumer_tag, producer_tag, in-edges)`, where
//! an in-edge `i → j` means interaction `j` consumed a port that interaction `i` produced.
//! Provenance is tracked at two grains, both real communication in a physical embedding:
//!  - **flow**: who last moved each side of the redex into contact (rule code emitting a
//!    port into `link`, or the parker recorded in `var_writer` when a port waits in a
//!    substitution cell) — the enablement/latency edge;
//!  - **birth**: who allocated each consumed agent's node (`node_birth`) — the data edge
//!    (where the consumed cells physically live). Nullary agents (L/ε) have no node; their
//!    flow edge is the only one.
//! Interaction ids are assigned in firing order by the SEQUENTIAL drain, so every in-edge
//! satisfies `i < j` and the log order is a topological order of the DAG. Tracing is
//! sequential-only (the shadow tables are unsynchronized); the CLI enforces `-threads 1`.
//!
//! Rung A logs `floor(log2(d))` histograms: `pop_hist` = node-base distance between the
//! two agents of each fired redex (allocation order as a witness 1-D embedding — a bump
//! allocator makes this meaningful; free-list reuse pollutes it, so it is a cheap signal
//! only, and per §10.2 short distances confirm embeddability while long ones refute
//! nothing), `var_hist` = var-index distance per var-var chain step in the exchange linker.
//!
//! Event record (little-endian): `u8` packed tags (consumer<<4 | producer), `u8` edge
//! count, then `count × u32` in-edge ids (deduped; id 0 = the initial net / loader).

use std::fs::File;
use std::io::{BufWriter, Write};

// Constructed only from the native CLI path (bench.rs, non-wasm); the wasm oracle build
// compiles this module but never instantiates it.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) const TRACE_MAGIC: &[u8; 8] = b"E1DAG1\0\0";

pub(crate) struct Tracer {
    /// Ambient interaction id: set at each traced interaction, 0 during load/readback.
    pub(crate) cur: u32,
    next_id: u32,
    /// node base -> id of the interaction that allocated it (0 = loader). Overwritten on
    /// free-list reuse; always read at consume time, before the consuming rule frees it.
    pub(crate) node_birth: Box<[u32]>,
    /// var index -> flow provenance of the port currently parked there.
    pub(crate) var_writer: Box<[u32]>,
    /// Lockstep with `Worker::bag`: flow provenance of each queued redex's two sides.
    pub(crate) prov_bag: Vec<(u32, u32)>,
    /// Provenance of the redex currently being fired (popped by `drain`).
    pub(crate) pending: (u32, u32),
    out: BufWriter<File>,
    pub(crate) events: u64,
    limit: u64,
    pub(crate) pop_hist: [u64; 33],
    pub(crate) var_hist: [u64; 33],
}

#[inline]
pub(crate) fn dist_bucket(d: u64) -> usize {
    // 0 -> 0, d>=1 -> 1 + floor(log2(d)) (so bucket b>=1 covers [2^(b-1), 2^b)).
    if d == 0 {
        0
    } else {
        (64 - d.leading_zeros()) as usize
    }
}

#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
impl Tracer {
    pub(crate) fn new(path: &str, node_cap: usize, var_cap: usize, limit: u64) -> std::io::Result<Self> {
        let f = File::create(path)?;
        let mut out = BufWriter::with_capacity(1 << 20, f);
        out.write_all(TRACE_MAGIC)?;
        Ok(Tracer {
            cur: 0,
            next_id: 1,
            node_birth: vec![0u32; node_cap].into_boxed_slice(),
            var_writer: vec![0u32; var_cap].into_boxed_slice(),
            prov_bag: Vec::new(),
            pending: (0, 0),
            out,
            events: 0,
            limit,
            pop_hist: [0; 33],
            var_hist: [0; 33],
        })
    }

    /// Record one interaction; returns its id and sets it as the ambient `cur`.
    /// `edges` must already be deduped (≤ 4 entries).
    pub(crate) fn event(&mut self, ct: u32, pt: u32, edges: &[u32]) -> u32 {
        let id = self.next_id;
        self.next_id = self.next_id.checked_add(1).expect("trace: id overflow");
        self.cur = id;
        self.events += 1;
        if self.events > self.limit {
            // No silent caps (SPATIAL_IC quality patterns): a truncated DAG must never be
            // fit as if complete. Abort loudly; rerun with a smaller workload or a higher
            // -trace-limit.
            eprintln!("trace: event limit {} exceeded — refusing to truncate the DAG", self.limit);
            std::process::exit(3);
        }
        let tb = (((ct & 0xF) as u8) << 4) | ((pt & 0xF) as u8);
        let mut rec = [0u8; 2 + 4 * 4];
        rec[0] = tb;
        rec[1] = edges.len() as u8;
        for (k, &e) in edges.iter().enumerate() {
            rec[2 + 4 * k..2 + 4 * k + 4].copy_from_slice(&e.to_le_bytes());
        }
        self.out
            .write_all(&rec[..2 + 4 * edges.len()])
            .expect("trace: write failed");
        id
    }

    /// Flush and return (events, pop_hist, var_hist).
    pub(crate) fn finish(mut self) -> (u64, [u64; 33], [u64; 33]) {
        self.out.flush().expect("trace: flush failed");
        (self.events, self.pop_hist, self.var_hist)
    }
}
