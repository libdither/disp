//! Wire-RC, minimal form: doubly-dead parked-duplicator cancellation
//! (RUST_IC_NET_DESIGN.md §5's targeted fix; SPATIAL_IC.md §3 "GC" gives it a physical
//! reading as a back-pulse down the parked wire).
//!
//! The leak: a parked δⁿ waits on an auxiliary result wire for a demanded value. ε acts
//! only at principals, so when both of the δⁿ's output wires are discarded (an ε parked in
//! each), nothing can reach the δⁿ; it pins the demand chain behind it. Under the
//! fire-everything drain the chain still completes and the copies are erased after the
//! fact, so the cost is wasted work plus peak memory rather than a permanent strand; under
//! a demand-driven schedule it strands outright. Either way the fix is the same: give ε a
//! back-channel to the parked agent.
//!
//! This implementation detects death directly instead of counting: an output wire is dead
//! iff its var cell holds a parked ε, and `pack(EPS, 0)` is a unique port word, so
//! deadness is one load per output. Three cancellation sites:
//!  - at ε-park (`link`): an ε parking into a watched output cell triggers `try_cancel`
//!    on the watching duplicator; if both outputs are dead, the parked δⁿ is replaced in
//!    its park cell by an ε (the demanded value, when it arrives, is erased on delivery),
//!    and the δⁿ and its output cells are freed now;
//!  - at δⁿ ⊗ P (the park/re-park rule): if both outputs are already dead, the suspension
//!    is forwarded to ε unevaluated instead of demanded, killing the whole future chain;
//!  - at δ ⊗ value (the copy rules, both species): if both outputs are already dead, the
//!    value is forwarded to ε instead of copied.
//!
//! The watch tables are HINTS, never authorities: `try_cancel` re-derives everything from
//! live cells (the park cell must still hold exactly this δⁿ's port; each output cell must
//! hold exactly a parked ε), so a stale watch after node/cell reuse is a harmless no-op.
//! Cancellation only ever erases a subnet none of whose outputs are observable, so normal
//! forms are unchanged; it can only terminate MORE programs (it erases doubly-discarded
//! demands that fire-everything would run), which moves this backend toward the eager
//! backends' K-discard termination domain (EVALUATOR.md "discard-laziness is
//! parity-critical").
//!
//! Sequential-only (the dead-check and park-swap are unsynchronized loads/stores); the
//! parallel drain never sets `Worker.rc`. Off by default: one never-taken branch on the
//! hot paths, the tracer/tiled pattern.

use crate::net::Ctx;
use crate::port::*;
use std::sync::atomic::Ordering;

pub(crate) const RC_NULL: u32 = u32::MAX;

/// Per-run cancellation state + counters. Boxed into `Worker.rc` when `-rc` is on.
pub(crate) struct RcState {
    /// node base -> var cell where that δⁿ is currently parked (RC_NULL when not parked).
    pub(crate) dn_park: Box<[u32]>,
    /// var index -> node base of the duplicator whose OUTPUT this cell is (a hint).
    pub(crate) var_watch: Box<[u32]>,
    pub(crate) cancels_at_eps: u64,  // try_cancel fired from an ε-park
    pub(crate) cancels_at_park: u64, // δⁿ ⊗ P found both outputs dead
    pub(crate) cancels_at_copy: u64, // δ ⊗ value found both outputs dead
}

impl RcState {
    pub(crate) fn new(node_cap: usize, var_cap: usize) -> Self {
        RcState {
            dn_park: vec![RC_NULL; node_cap].into_boxed_slice(),
            var_watch: vec![RC_NULL; var_cap].into_boxed_slice(),
            cancels_at_eps: 0,
            cancels_at_park: 0,
            cancels_at_copy: 0,
        }
    }
}

impl<'a> Ctx<'a> {
    /// Is this port a dead duplicator output: a var wire whose cell holds a parked ε?
    #[inline]
    pub(crate) fn rc_dead(&self, p: u32) -> bool {
        is_var(p) && self.net.vars[val(p)].load(Ordering::Relaxed) == pack(EPS, 0)
    }

    /// Both outputs of the duplicator node at `base` are dead. Reads the aux cells live.
    #[inline]
    pub(crate) fn rc_both_dead(&self, base: u32) -> bool {
        self.rc_dead(self.nd(base)) && self.rc_dead(self.nd(base + 1))
    }

    /// Free a dead output wire's var cell (and its parked ε word) + clear its watch.
    pub(crate) fn rc_free_output(&mut self, p: u32) {
        debug_assert!(self.rc_dead(p));
        let v = val(p);
        let rc = self.w.rc.as_mut().unwrap();
        rc.var_watch[v] = RC_NULL;
        self.net.vars[v].store(self.w.free_var_head, Ordering::Relaxed);
        self.w.free_var_head = v as u32;
    }

    /// A δⁿ port just parked into `cell`: record the park and watch its two outputs.
    pub(crate) fn rc_register_park(&mut self, dn_base: u32, cell: usize) {
        let dl = self.nd(dn_base);
        let dr = self.nd(dn_base + 1);
        let rc = self.w.rc.as_mut().unwrap();
        rc.dn_park[dn_base as usize] = cell as u32;
        if is_var(dl) {
            rc.var_watch[val(dl)] = dn_base;
        }
        if is_var(dr) {
            rc.var_watch[val(dr)] = dn_base;
        }
    }

    /// An ε just parked into a watched cell: cancel the watching δⁿ if it is genuinely
    /// parked and both outputs are genuinely dead. The watch is a hint; every condition is
    /// re-derived from live state, so stale watches (after node or cell reuse) no-op.
    pub(crate) fn rc_try_cancel(&mut self, dn_base: u32) {
        let park = self.w.rc.as_ref().unwrap().dn_park[dn_base as usize];
        if park == RC_NULL {
            return;
        }
        if self.net.vars[park as usize].load(Ordering::Relaxed) != pack(DN, dn_base) {
            return; // in flight or the cell moved on; the normal path owns it
        }
        if !self.rc_both_dead(dn_base) {
            return;
        }
        // Replace the parked δⁿ with a parked ε: the demanded value erases on delivery.
        self.net.vars[park as usize].store(pack(EPS, 0), Ordering::Relaxed);
        let (dl, dr) = (self.nd(dn_base), self.nd(dn_base + 1));
        self.rc_free_output(dl);
        self.rc_free_output(dr);
        let rc = self.w.rc.as_mut().unwrap();
        rc.dn_park[dn_base as usize] = RC_NULL;
        rc.cancels_at_eps += 1;
        self.free_node(dn_base, 2);
    }
}
