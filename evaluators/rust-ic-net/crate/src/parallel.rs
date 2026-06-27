//! M2c: native parallel drain (Stage 2 — per-worker work-stealing deques).
//!
//! Each worker owns a crossbeam `Worker` deque (LIFO ⇒ depth-first, hot local working set);
//! a rule's products go to the OWNER's deque with NO atomic (the common case). The shared
//! `Injector` only seeds + absorbs nothing else; an idle worker steals a BATCH from the
//! injector or a victim. The per-interaction `in_flight` counter is GONE — termination is an
//! `active` (non-idle) worker count: a worker decrements when its deque empties AND every
//! steal source is empty, re-increments when it finds work; `active == 0` ⇒ quiescent ⇒ done.
//! Coordination is now O(steal events), not O(interactions). Correctness rests on: (a) only a
//! deque's OWNER pushes to it (stealers only remove), so once a worker idles its deque stays
//! empty; (b) a producing worker is never idle (it drains its deque before trying to steal),
//! so `active` can't reach 0 while any redex exists; (c) AcqRel on `active` + the var AcqRel
//! publish the producer's owned-cell writes to the thief. Strong confluence (Theorem 2) ⇒ NF
//! AND interaction count are thread-count-invariant — the race-detector gate is unchanged.
//!
//! crossbeam does NOT build on wasm32, so this module is `#[cfg]`-excluded there; the wasm
//! build is the single-threaded oracle. The rule kernel + atomic cells + linker are shared.

use crate::net::{Net, Worker};
use crate::port::*;
use std::sync::atomic::Ordering;

impl Net {
    /// Reduce `h` to full NF with `threads` workers sharing this net. NF root, or None on
    /// budget exhaustion. Exercised by the cargo race-detector test and the native parallel
    /// binding (M2d); the plain sequential lib build has no caller, hence the scoped allow.
    #[allow(dead_code)]
    pub(crate) fn full_nf_parallel(&self, h: u32, threads: usize, budget: i64) -> Option<u32> {
        use crossbeam_deque::{Injector, Steal, Stealer, Worker as Deque};
        use crossbeam_utils::CachePadded;
        use std::sync::atomic::{AtomicBool, AtomicI64, AtomicUsize};

        // Steal one task from the injector, else a batch-and-pop from any victim. Returns a
        // task (with any batch already moved into `local`), or None when ALL sources are
        // empty (Retry is looped through, never reported as empty ⇒ live work is never missed).
        fn find_task(
            injector: &Injector<(u32, u32)>,
            stealers: &[Stealer<(u32, u32)>],
            local: &Deque<(u32, u32)>,
        ) -> Option<(u32, u32)> {
            loop {
                match injector.steal_batch_and_pop(local) {
                    Steal::Success(r) => return Some(r),
                    Steal::Empty => break,
                    Steal::Retry => {}
                }
            }
            for st in stealers {
                loop {
                    match st.steal_batch_and_pop(local) {
                        Steal::Success(r) => return Some(r),
                        Steal::Empty => break,
                        Steal::Retry => {}
                    }
                }
            }
            None
        }

        let mut setup = Worker::new();
        let v = {
            let mut c = self.ctx(&mut setup);
            let v = c.new_var();
            let n = c.alloc(&[pack(VAR, v)]);
            c.link(pack(N, n), h);
            v
        };
        if setup.bag.is_empty() {
            return Some(self.resolve(pack(VAR, v)));
        }
        let injector = Injector::new();
        for r in setup.bag.drain(..) {
            injector.push(r);
        }

        const BUDGET_LEASE: i64 = 1 << 16;
        let deques: Vec<Deque<(u32, u32)>> = (0..threads).map(|_| Deque::new_lifo()).collect();
        let stealers: Vec<Stealer<(u32, u32)>> = deques.iter().map(|d| d.stealer()).collect();
        let active = CachePadded::new(AtomicUsize::new(threads));
        let pool = CachePadded::new(AtomicI64::new(budget));
        let exhausted = CachePadded::new(AtomicBool::new(false));
        let injector = &injector;
        let stealers = &stealers;
        let active = &active;
        let pool = &pool;
        let exhausted = &exhausted;

        std::thread::scope(|s| {
            for deque in deques {
                s.spawn(move || {
                    let mut w = Worker::new();
                    let backoff = crossbeam_utils::Backoff::new();
                    let mut local_budget: i64 = 0;
                    let mut local_int: u64 = 0;
                    let mut idle = false;
                    loop {
                        // 1. drain the local deque (no shared-state traffic on this path).
                        while let Some((x, y)) = deque.pop() {
                            if local_budget <= 0 {
                                let got = pool.fetch_sub(BUDGET_LEASE, Ordering::Relaxed);
                                if got <= 0 {
                                    exhausted.store(true, Ordering::Relaxed);
                                    break;
                                }
                                local_budget = got.min(BUDGET_LEASE);
                            }
                            local_budget -= 1;
                            local_int += 1;
                            {
                                let mut c = self.ctx(&mut w);
                                c.interact(x, y);
                            }
                            for nr in w.bag.drain(..) {
                                deque.push(nr);
                            }
                        }
                        if exhausted.load(Ordering::Relaxed) {
                            break;
                        }
                        // 2. local empty — look for external work.
                        if let Some(r) = find_task(injector, stealers, &deque) {
                            if idle {
                                active.fetch_add(1, Ordering::AcqRel);
                                idle = false;
                            }
                            backoff.reset();
                            deque.push(r);
                            continue;
                        }
                        // 3. nothing anywhere — announce idle; quiesce when all are idle.
                        if !idle {
                            active.fetch_sub(1, Ordering::AcqRel);
                            idle = true;
                        }
                        if active.load(Ordering::Acquire) == 0 {
                            break;
                        }
                        backoff.snooze();
                    }
                    if !idle {
                        active.fetch_sub(1, Ordering::AcqRel); // leave decremented (exhausted path)
                    }
                    self.interactions.fetch_add(local_int, Ordering::Relaxed);
                });
            }
        });
        if exhausted.load(Ordering::Relaxed) {
            return None;
        }
        Some(self.resolve(pack(VAR, v)))
    }
}
