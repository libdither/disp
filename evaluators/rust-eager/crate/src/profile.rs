//! Cost centres (`run.ts --stats-defs`): fork-dispatches attributed to labelled
//! definitions, by GHC's rule that costs go to the stack of open labelled calls. A
//! labelled call is an eager memo frame whose operator carries a label; INCLUSIVE counts
//! the steps under a centre's outermost open call (recursion is not double-counted) and
//! SELF the steps while it is the innermost one. Centre 0 collects steps under no label.
//! Off (`Arena::profile = None`) the reducer pays one predictable branch per frame.

use rustc_hash::FxHashMap;

#[derive(Default)]
pub(crate) struct Profile {
    /// handle → (centre, arguments still to come before the call saturates, is a definition)
    labels: FxHashMap<u32, (u32, u32, bool)>,
    /// open labelled calls: (centre, spine depth of its frame, args left, entry step)
    open: Vec<(u32, u32, u32, u64)>,
    /// open labelled calls per centre (inclusive closes when the outermost one does)
    active: Vec<u32>,
    /// per centre: [inclusive, self, memo hits, memo misses]
    counts: Vec<[u64; 4]>,
    /// step at which the innermost centre last started accruing self
    last: u64,
}

impl Profile {
    pub(crate) fn new(now: u64) -> Self {
        Profile { last: now, ..Default::default() }
    }

    fn centre(&mut self, c: u32) -> usize {
        let c = c as usize;
        if c >= self.counts.len() {
            self.counts.resize(c + 1, [0; 4]);
            self.active.resize(c + 1, 0);
        }
        c
    }

    fn innermost(&self) -> usize {
        self.open.last().map_or(0, |o| o.0 as usize)
    }

    /// Label a definition's handle. The first definition on a handle wins, over any label
    /// carried there earlier: a `rec` definition's handle is itself a `wait` partial.
    pub(crate) fn label(&mut self, handle: u32, centre: u32, arity: u32) {
        self.centre(centre);
        let l = self.labels.entry(handle).or_insert((centre, arity.max(1), true));
        if !l.2 {
            *l = (centre, arity.max(1), true);
        }
    }

    /// Partial applications keep the label: `apply(f, x)` with args still to come after
    /// `x` labels its result. Frames close inside out, so the outermost call that delivers
    /// a partial names it last; only a definition's own label is never replaced.
    fn carry(&mut self, result: u32, centre: u32, left: u32) {
        if left > 1 {
            let l = self.labels.entry(result).or_insert((centre, left - 1, false));
            if !l.2 {
                *l = (centre, left - 1, false);
            }
        }
    }

    pub(crate) fn hit(&mut self, f: u32, result: u32) {
        if let Some(&(c, left, _)) = self.labels.get(&f) {
            self.counts[c as usize][2] += 1;
            self.carry(result, c, left);
        }
    }

    /// A memo frame for `apply(f, _)` opened at spine `depth`; its dispatch is step `now`.
    pub(crate) fn enter(&mut self, f: u32, depth: u32, now: u64) {
        if let Some(&(c, left, _)) = self.labels.get(&f) {
            let i = self.innermost();
            self.counts[i][1] += now - self.last;
            self.last = now;
            let ci = c as usize;
            self.counts[ci][3] += 1;
            self.active[ci] += 1;
            self.open.push((c, depth, left, now));
        }
    }

    /// The memo frame at spine `depth` delivered `result` at step `now`.
    pub(crate) fn leave(&mut self, depth: u32, result: u32, now: u64) {
        if let Some(&(c, d, left, _)) = self.open.last() {
            if d == depth {
                self.close(now);
                self.carry(result, c, left);
            }
        }
    }

    fn close(&mut self, now: u64) {
        let (c, _, _, start) = self.open.pop().unwrap();
        let ci = c as usize;
        self.counts[ci][1] += now - self.last;
        self.last = now;
        self.active[ci] -= 1;
        if self.active[ci] == 0 {
            self.counts[ci][0] += now - start;
        }
    }

    /// Close calls a halted `reduce` abandoned, charging them up to `now`.
    pub(crate) fn unwind(&mut self, now: u64) {
        while !self.open.is_empty() {
            self.close(now);
        }
    }

    /// Drop labels on handles a scoped reclamation freed (their ids get reused).
    pub(crate) fn retain(&mut self, live: impl Fn(u32) -> bool) {
        self.labels.retain(|&h, _| live(h));
    }

    /// Flattened `[inclusive, self, hits, misses]` per centre, as of step `now`.
    pub(crate) fn report(&mut self, now: u64) -> Vec<f64> {
        self.unwind(now);
        let i = self.innermost();
        self.centre(0);
        self.counts[i][1] += now - self.last;
        self.last = now;
        self.counts.iter().flat_map(|c| c.iter().map(|&v| v as f64)).collect()
    }
}
