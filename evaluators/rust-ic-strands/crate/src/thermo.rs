//! The lattice run thermodynamically (`Params::thermo`): every move a random event whose reverse is
//! proposed as often, paid for in heat that sits in a bank at each site, so the run's speed and its heat
//! are those of a physical system with local detailed balance.
//!
//! - **Events.** Each event draws a live site, a kind, and the kind's parameters from fixed ranges: a
//!   hop (slot, face), an exchange (two slots, face), a flip (strand end), a fold or unfold (any end,
//!   face), a reaction (slot), or heat passing (face, or down). Draws that name nothing to do are null events.
//!   A hop's reverse is the hop back, a flip's the flip from the opposite corner, a fold's the unfold
//!   of the same end and face, so every move is proposed as often as its reverse, but for the number of
//!   live sites, which a Hastings factor corrects. Lanes and slots are taken first free, so states are
//!   counted up to which lane or slot holds what.
//! - **Energy.** A move is made, the energy of the sites it touched is counted again, and the change is
//!   paid from the bank of one of them (the lowest numbered, the same for a move and its reverse); a
//!   bank that cannot pay puts the move back. The energy is `energy_of`: every strand costs the mean of
//!   `w_principal` and `w_aux`, plus the crowding of sites, links and switchboards, plus `bond` a site
//!   of the distance between agents a kept record ties together (`tied`), so that what an undo needs
//!   stays near. So it is a function of the state alone and of what is near a move (the plain
//!   lattice's weight, seen from whichever end drags a wire, is neither: a wire's weight would change
//!   along all its length when a rewrite turns an aux end into a principal one). The bonds strengthen
//!   as the drive turns negative, as on the substrate; what that does to the energy is work done on
//!   the lattice, counted with the fuel.
//! - **Walking.** A wanted reader gets to what it reads as a motor protein does: its step one strand
//!   along its principal wire (a hop, or its half of an exchange) brings `walk` of fuel into the bank,
//!   and any other step of it gives that fuel back. A step and its reverse are proposed as often, so the
//!   walk is driven by its fuel, not by how it is proposed; and every step of a reader pays it, or a
//!   step forward and a hop back would turn fuel into heat for nothing.
//! - **Heat.** A heat event moves one quarter unit from a bank to a live neighbour's, or to or from
//!   empty space (the bath, at `temp`), which takes heat freely and gives a quantum with chance
//!   e^(−1/4T); or the bank meets the substrate under its site, a good sink as a chip's is, and takes
//!   the bath's temperature at once.
//! - **Rewrites.** A reaction fires a pair in reach, its fresh agents seated as on the plain lattice.
//!   Seating costs what the layout makes it; that cost comes with the fuel and its record keeps it, so
//!   every rewrite brings `drive` into the consumer site's bank whatever its seating, as every step of
//!   Bennett's Brownian computer is driven by the same ε. An undo is tried when the bank of the site the
//!   rewrite paid holds the drive, and takes back the drive and the seating the record kept, plus what
//!   the undo's own layout adds; whatever the bank is short of after all (the products moved since)
//!   comes from the bath and is counted (`borrowed`). These two are not exactly balanced: a rewrite is
//!   proposed from its consumer's slot, an undo from its products' sites (or, for one that left
//!   nothing, the site it happened at).
//! - **Time.** A live site has one event per unit of time on average: each event is 1/N of a clock.

use super::{substrate::Undo, Lattice, NONE};
use std::collections::HashMap;

/// The kinds of event, for the counts.
pub const KINDS: [&str; 9] = ["hop", "exchange", "flip", "fold", "unfold", "fire", "undo", "heat", "walk"];

#[derive(Clone, Debug, Default)]
pub struct Ledger {
    /// Fuel brought in: the drive and seating a rewrite forward, the walk a reader's step forward, less
    /// what an undo or a step back gives back (quarter units).
    pub fuel: i64,
    /// Heat given to the bath, less heat taken from it.
    pub to_bath: i64,
    /// Heat undos took from the bath because their bank was short (counted in `to_bath` too).
    pub borrowed: i64,
    /// Events tried and moves made, by kind (`KINDS`).
    pub tried: [u64; 9],
    pub made: [u64; 9],
}

#[derive(Clone, Debug, Default)]
pub struct Thermo {
    /// By site: the heat its bank holds, in quarter units.
    pub bank: Vec<u32>,
    pub ledger: Ledger,
    /// By record: the site whose bank its rewrite paid, and the energy the rewrite added.
    paid: HashMap<u64, (u32, i32)>,
    /// Whether the banks have been given their first heat.
    pub(super) warm: bool,
}

impl Thermo {
    pub fn new(sites: usize) -> Thermo { Thermo { bank: vec![0; sites], ..Default::default() } }
}

impl Lattice {
    /// The energy of these sites, in quarter units: each agent pair sharing a site, each pair of idle
    /// agents sharing one, each switchboard's pairings squared, each strand end half a strand's weight,
    /// each link touching them its strands squared (once a link), and `bond` a site of the distance
    /// from each agent here to each it is tied to (`tied`; half of it, for one here too).
    pub fn energy_of(&self, sites: &[u32]) -> i64 { self.energy_in(sites, false) }

    fn energy_in(&self, sites: &[u32], all: bool) -> i64 {
        let half = ((self.e.principal[0] + self.e.aux[0]) / 4) as i64;
        let pairs = |n: i64| n * (n - 1) / 2;
        let mut e = 0i64;
        let mut links: Vec<(u32, u32)> = vec![];
        for &s in sites {
            let occ = self.occ[s as usize] as i64;
            e += self.e.crowd as i64 * pairs(occ) + self.e.idle as i64 * pairs(self.idle_at(s) as i64);
            let p = self.pairs(s) as i64;
            e += self.e.board as i64 * p * p;
            let base = s as usize * self.ends;
            e += half * self.mate[base + self.ar * self.ks..base + self.ends].iter().filter(|&&m| m != NONE).count() as i64;
            if self.e.link != 0 {
                for f in 0..self.faces {
                    let t = self.nb(s, f);
                    if t == u32::MAX { continue; }
                    let key = (s.min(t), s.max(t));
                    if links.contains(&key) { continue; }
                    links.push(key);
                    let n = self.used_lanes(s, f) as i64;
                    e += self.e.link as i64 * n * n;
                }
            }
        }
        if self.e.bond != 0 {
            let d = |x: [i64; 3], y: [i64; 3]| (0..3).map(|i| (x[i] - y[i]).abs()).sum::<i64>();
            let mut twice = 0;
            for &s in sites {
                for k in 0..self.p.k {
                    if self.tag(s, k) == 0 { continue; }
                    for x in self.tied(self.sids[s as usize * self.ks + k]) {
                        let at = self.sub.seat_of.get(x as usize).copied().unwrap_or(u32::MAX);
                        if at == u32::MAX || !self.alive(x) { continue; }
                        twice += d(self.xyz(s), self.xyz(at)) * if all || sites.contains(&at) { 1 } else { 2 };
                    }
                }
            }
            e += self.e.bond as i64 * twice / 2;
        }
        e
    }

    /// The agents kept records tie agent `id` to: the others its maker made, and the near ends of the
    /// other wires joined by any kept join on one of its ports. Each tie is seen from both its ends (the
    /// substrate's `bonded` sees only the latest join), so the energy is a function of the state.
    fn tied(&self, id: u32) -> Vec<u32> {
        let mut out = vec![];
        if let Some(e) = self.maker(id) {
            let r = self.sub.records[e as usize].as_ref().unwrap();
            out.extend((r.made..r.made + r.n as u32).filter(|&x| x != id));
        }
        for q in 0..self.ar as u8 {
            for &e in self.sub.marks.get(&(id, q)).map_or(&[][..], |v| &v[..]) {
                let Some(Some(r)) = self.sub.records.get(e as usize) else { continue };
                if r.joined.iter().any(|j| j.a == (id, q)) { out.extend(r.joined.iter().map(|j| j.a.0).filter(|&x| x != id)); }
            }
        }
        out
    }

    /// The energy of the whole lattice (`energy_of` every live site).
    pub fn energy(&self) -> i64 { let live = self.live.clone(); self.energy_in(&live, true) }

    /// Energy kept: the lattice's, the banks', and what the bath took, less the fuel brought in. Constant.
    pub fn energy_kept(&self) -> i64 {
        self.energy() + self.th.bank.iter().map(|&b| b as i64).sum::<i64>() + self.th.ledger.to_bath - self.th.ledger.fuel
    }

    /// Mean heat a bank holds at the bath's temperature, in quarter units.
    pub fn bank_mean(&self) -> f64 { let p = (-0.25 / self.p.temp).exp(); p / (1.0 - p) }

    /// A bank's heat at the bath's temperature: geometric, one quantum at a time.
    fn bank_draw(&mut self) -> u32 {
        let p = (-0.25 / self.p.temp).exp();
        let mut n = 0;
        while self.unit() < p && n < 4096 { n += 1; }
        n
    }

    /// Empty space is the bath: a site matter moves into has a bank at the bath's temperature, and one
    /// it leaves gives its heat back.
    pub(super) fn bank_from_bath(&mut self, s: u32) {
        let b = self.bank_draw();
        self.th.ledger.to_bath -= b as i64 - std::mem::replace(&mut self.th.bank[s as usize], b) as i64;
    }
    pub(super) fn bank_to_bath(&mut self, s: u32) { self.th.ledger.to_bath += std::mem::take(&mut self.th.bank[s as usize]) as i64; }

    /// Run events until the answer is in, at most `max` of them. Whether it is in.
    pub fn run_thermo(&mut self, max: u64) -> bool {
        let stop = self.stats.proposals + max;
        while self.stats.proposals < stop {
            if self.stats.proposals % 4096 == 0 && self.answered() { return true; }
            self.event();
        }
        self.answered()
    }

    /// Run `n` events, the answer in or not.
    pub fn run_events(&mut self, n: u64) { for _ in 0..n { self.event(); } }

    /// One event.
    pub fn event(&mut self) {
        if !self.th.warm {
            self.th.warm = true;
            for i in 0..self.live.len() {
                let s = self.live[i];
                let b = self.bank_draw();
                self.th.bank[s as usize] += b;
                self.th.ledger.to_bath -= b as i64;
            }
        }
        let n = self.live.len();
        if n == 0 { return; }
        self.stats.proposals += 1;
        self.stats.clocks += 1.0 / n as f64;
        let i = self.below(n);
        let s = self.live[i];
        let x = self.unit();
        let kind = if x < 0.50 { 0 } else if x < 0.55 { 1 } else if x < 0.70 { 2 } else if x < 0.80 { 3 } else if x < 0.88 { 5 } else { 7 };
        match kind {
            0 => self.ev_hop(s),
            1 => self.ev_exchange(s),
            2 => self.ev_flip(s),
            3 => self.ev_fold(s),
            5 => self.ev_react(s),
            _ => self.ev_heat(s),
        }
    }

    /// A move made tentatively at these sites: keep it if the bank of the lowest-numbered one pays its
    /// energy and the Hastings factor for the live sites allows it, else put the sites back.
    fn settle(&mut self, kind: usize, sites: &[u32], saved: super::Saved, e0: i64, live0: usize) { self.settle_fuel(kind, sites, saved, e0, live0, 0); }

    /// `settle`, with `fuel` brought into the paying bank (taken from it, if negative).
    fn settle_fuel(&mut self, kind: usize, sites: &[u32], saved: super::Saved, e0: i64, live0: usize, fuel: i64) {
        let live1 = self.live.len();
        if live1 > live0 && self.unit() >= live0 as f64 / live1 as f64 { self.restore(saved); return; }
        let de = self.energy_of(sites) - e0 - fuel;
        let payer = *sites.iter().min().unwrap() as usize;
        if de > self.th.bank[payer] as i64 { self.restore(saved); return; }
        self.th.bank[payer] = (self.th.bank[payer] as i64 - de) as u32;
        self.th.ledger.fuel += fuel;
        self.th.ledger.made[kind] += 1;
    }

    /// The fuel the agent at slot k of s brings stepping across face f: a wanted reader walks as a motor
    /// protein does, `walk` of fuel for a step along its principal wire and that fuel given back for any
    /// other step; anything else steps for nothing.
    fn walk_fuel(&self, s: u32, k: usize, f: usize) -> Option<i64> {
        let t = self.tag(s, k);
        if t == 0 || !super::tag_of(t).is_consumer() || !self.want[s as usize * self.ks + k] { return None; }
        let m = self.mate_of(s, self.ae(k, 0));
        let fuel = (self.p.walk * 4.0).round() as i64;
        Some(if self.is_strand(m) && self.face(m) == f { fuel } else { -fuel })
    }

    /// A hop, or for a wanted reader a walk (`walk_fuel`).
    fn ev_hop(&mut self, s: u32) {
        let (k, f) = (self.below(self.p.k), self.below(self.faces));
        if self.tag(s, k) == 0 { return; }
        let t = self.nb(s, f);
        if t == u32::MAX || !self.inside(t) { return; }
        let Some(k2) = self.free_slot(t) else { return };
        let Some((_, lanes, loops, _)) = self.step_plan(s, k, f) else { return };
        if !self.room(t, lanes.len() + loops) { return; }
        let walk = self.walk_fuel(s, k, f);
        let kind = if walk.is_some() { 8 } else { 0 };
        self.th.ledger.tried[kind] += 1;
        let (sites, live0) = ([s, t], self.live.len());
        let (saved, e0) = (self.save(&sites), self.energy_of(&sites));
        if self.hop_to(s, k, f, k2, false, false).is_none() { self.restore(saved); return; }
        self.settle_fuel(kind, &sites, saved, e0, live0, walk.unwrap_or(0));
    }

    fn ev_exchange(&mut self, s: u32) {
        let (ka, f, kb) = (self.below(self.p.k), self.below(self.faces), self.below(self.p.k));
        let t = self.nb(s, f);
        if self.tag(s, ka) == 0 || t == u32::MAX || !self.inside(t) || self.tag(t, kb) == 0 { return; }
        self.th.ledger.tried[1] += 1;
        let fuel = self.walk_fuel(s, ka, f).unwrap_or(0) + self.walk_fuel(t, kb, f ^ 1).unwrap_or(0);
        let (sites, live0) = ([s, t], self.live.len());
        let (saved, e0) = (self.save(&sites), self.energy_of(&sites));
        self.force = true;
        let done = self.exchange(s, ka, f, kb, false).is_some();
        self.force = false;
        if !done { return; }
        self.settle_fuel(1, &sites, saved, e0, live0, fuel);
    }

    fn ev_flip(&mut self, s: u32) {
        let e = (self.ar * self.ks + self.below(self.faces * self.p.lanes)) as u8;
        if self.mate_of(s, e) == NONE { return; }
        let Some(c) = self.corner(s, e, Self::inside) else { return };
        let sites = [s, c.x, c.z, c.w];
        self.th.ledger.tried[2] += 1;
        let live0 = self.live.len();
        let (saved, e0) = (self.save(&sites), self.energy_of(&sites));
        self.force = true;
        let done = self.flip(s, e, false);
        self.force = false;
        if !done { self.restore(saved); return; }
        self.settle(2, &sites, saved, e0, live0);
    }

    /// From end a of s along face f: fold the U-turn its wire makes there, or make one.
    fn ev_fold(&mut self, s: u32) {
        let (a, f) = (self.below(self.ends) as u8, self.below(self.faces));
        let m = self.mate_of(s, a);
        if m == NONE { return; }
        let t = self.nb(s, f);
        if t == u32::MAX || !self.inside(t) { return; }
        let sites = [s, t];
        if self.is_strand(m) && self.face(m) == f && self.fold_at(s, m, Self::inside).is_some() {
            self.th.ledger.tried[3] += 1;
            let live0 = self.live.len();
            let (saved, e0) = (self.save(&sites), self.energy_of(&sites));
            self.fold(s, m);
            self.settle(3, &sites, saved, e0, live0);
            return;
        }
        // Unfold: the wire a–m at s takes a detour to t and back, on two free lanes.
        let Some(l) = self.free_lanes(s, f, 2) else { return };
        if !self.room(s, 1) || !self.room(t, 1) { return; }
        self.th.ledger.tried[4] += 1;
        let live0 = self.live.len();
        let (saved, e0) = (self.save(&sites), self.energy_of(&sites));
        let (i, j) = (l[0], l[1]);
        self.set(s, a, NONE);
        self.set(s, m, NONE);
        self.link(s, a, self.se(f, i));
        self.link(s, self.se(f, j), m);
        self.link(t, self.se(f ^ 1, i), self.se(f ^ 1, j));
        self.strand_count(2);
        self.refresh(s);
        self.refresh(t);
        self.settle(4, &sites, saved, e0, live0);
    }

    /// A reaction at slot k of s: fire the pair the consumer there is in, or undo what made what is there.
    fn ev_react(&mut self, s: u32) {
        let k = self.below(self.p.k);
        let drive = (self.p.drive * 4.0).round() as i64;
        if self.tag(s, k) == 0 {
            // A rewrite that left nothing behind is undone from the site it happened at.
            if let Some(e) = self.sub.bare.get(&s).and_then(|v| v.last().copied()) { self.try_undo(s, e, drive); }
            return;
        }
        if let Some((kc, kp, via)) = self.active_pair(s).filter(|&(kc, _, _)| kc == k) {
            if (self.th.bank[s as usize] as i64) < -drive { return; }
            self.th.ledger.tried[5] += 1;
            let (e0, n0) = (self.energy(), self.sub.records.len());
            if !self.fire(s, kc, kp, via) { return; }
            let de = self.energy() - e0;
            let e = self.sub.records.len().max(n0) as u64 - 1;
            self.th.paid.insert(e, (s, de as i32));
            self.charge(s, -drive);
            self.th.ledger.fuel += drive + de;
            self.th.ledger.made[5] += 1;
            return;
        }
        // Else what made the agent here, or the latest rewrite that joined a wire at one of its ports.
        let id = self.sids[s as usize * self.ks + k];
        let mut tries: Vec<u64> = self.maker(id).into_iter().collect();
        tries.extend((0..self.ar as u8).filter_map(|q| self.sub.marks.get(&(id, q)).and_then(|v| v.last().copied())));
        if tries.is_empty() { return; }
        let e = tries[self.below(tries.len())];
        self.try_undo(s, e, drive);
    }

    /// Undo record e from site s, if the bank its rewrite paid holds the drive.
    fn try_undo(&mut self, s: u32, e: u64, drive: i64) {
        if !self.undoable(e) { return; }
        let (payer, de_fire) = self.th.paid.get(&e).copied().unwrap_or((s, 0));
        if (self.th.bank[payer as usize] as i64) < drive { return; }
        self.th.ledger.tried[6] += 1;
        let e0 = self.energy();
        if !matches!(self.undo(s, e), Undo::Done) { return; }
        let de = self.energy() - e0;
        self.th.paid.remove(&e);
        self.charge(payer, de + drive + de_fire as i64);
        self.th.ledger.fuel -= drive + de_fire as i64;
        self.th.ledger.made[6] += 1;
    }

    /// Take `de` from site s's bank (give it, if negative); what the bank is short of comes from the bath.
    fn charge(&mut self, s: u32, de: i64) {
        let b = self.th.bank[s as usize] as i64 - de;
        if b < 0 {
            self.th.ledger.borrowed -= b;
            self.th.ledger.to_bath += b;
            self.th.bank[s as usize] = 0;
        } else {
            self.th.bank[s as usize] = b as u32;
        }
    }

    /// A quantum of heat from s across face f, or down (f = faces) into the substrate under it: into a
    /// live neighbour's bank, or out to the bath (the substrate, and empty space), or in from it with
    /// chance e^(−1/4T).
    fn ev_heat(&mut self, s: u32) {
        let f = self.below(self.faces + 1);
        let t = if f < self.faces { self.nb(s, f) } else { u32::MAX };
        if f < self.faces && t == u32::MAX { return; }
        self.th.ledger.tried[7] += 1;
        if t != u32::MAX && self.live_pos[t as usize] != u32::MAX {
            if self.th.bank[s as usize] > 0 { self.th.bank[s as usize] -= 1; self.th.bank[t as usize] += 1; self.th.ledger.made[7] += 1; }
            return;
        }
        if t == u32::MAX {
            // The substrate is a good sink: the bank takes the bath's temperature at once.
            let (old, new) = (self.th.bank[s as usize], self.bank_draw());
            self.th.bank[s as usize] = new;
            self.th.ledger.to_bath += old as i64 - new as i64;
            self.th.ledger.made[7] += 1;
            return;
        }
        if self.unit() < 0.5 {
            if self.th.bank[s as usize] > 0 { self.th.bank[s as usize] -= 1; self.th.ledger.to_bath += 1; self.th.ledger.made[7] += 1; }
        } else if self.unit() < (-0.25 / self.p.temp).exp() {
            self.th.bank[s as usize] += 1;
            self.th.ledger.to_bath -= 1;
            self.th.ledger.made[7] += 1;
        }
    }
}
