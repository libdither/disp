//! Fields: a few small numbers per site that every site, empty or not, recomputes each clock from
//! what it holds, its own numbers and its face neighbours' numbers of the clock before, the way
//! demand pulses move. A channel has sources (what in a site sets it), a falloff per hop through
//! space and per hop across a face that carries a strand, and a fade per clock. Moves read the
//! channels as energy: an agent pays its weight times the change in a channel between where it is
//! and where it goes, by what it is (idle, a called value, a wanted computation), and wire laid
//! down by an idle agent's step or by a flip pays per pairing it adds where a channel is high.

use super::{tag_of, Lattice, Params, NONE};
use rust_ca_lattice::rules::Tag;

/// What sets a channel at a site, any of: a wanted reader there, a demand pulse there, a rewrite or
/// a rewrite without room there during the clock before, or its switchboard's pairings.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Source(pub u8);
impl Source {
    pub const NONE: Source = Source(0);
    pub const READER: u8 = 1;
    pub const PULSE: u8 = 2;
    pub const FIRE: u8 = 4;
    pub const BLOCKED: u8 = 8;
    pub const LOAD: u8 = 16;
    fn has(self, b: u8) -> bool { self.0 & b != 0 }
}

#[derive(Clone, Copy, Debug)]
pub struct Channel {
    pub source: Source,
    /// Values run from 0 to 2^bits − 1.
    pub bits: u8,
    /// What a source sets its site to (`Load`: the pairings, capped).
    pub level: u8,
    /// Lost per hop to a face neighbour, and across a face that carries a strand (NONE: no
    /// spreading that way).
    pub step: u8,
    pub wire_step: u8,
    /// Lost per clock from a site's own value (NONE: nothing is kept from the clock before).
    pub decay: u8,
    /// Energy per unit of channel, in temperature units, for an idle agent, a called value (a value
    /// a demand pulse has reached, `Params::calls`) and a wanted computation.
    pub idle: f64,
    pub called: f64,
    pub wanted: f64,
    /// Energy, like switchboard crowding (`board`), per unit of channel times the change in a
    /// site's pairings squared, for wire an idle agent drags or a flip moves.
    pub wire: f64,
}

impl Channel {
    pub const OFF: Channel = Channel { source: Source::NONE, bits: 0, level: 0, step: NONE, wire_step: NONE, decay: NONE,
                                       idle: 0.0, called: 0.0, wanted: 0.0, wire: 0.0 };

    /// A channel from `src=reader+pulse,bits=2,level=3,step=1,wstep=-,decay=1,idle=4,called=-2,wanted=0,wire=0.5`
    /// (a missing key keeps its default; `-` for a step or decay is NONE).
    pub fn parse(spec: &str) -> Result<Channel, String> {
        let mut c = Channel { bits: 1, level: 1, step: 1, wire_step: NONE, ..Channel::OFF };
        for kv in spec.split(',').filter(|s| !s.is_empty()) {
            let (k, v) = kv.split_once('=').ok_or(format!("field {spec}: {kv} is not key=value"))?;
            let byte = || if v == "-" { Ok(NONE) } else { v.parse::<u8>().map_err(|_| format!("field {spec}: {kv}")) };
            let num = || v.parse::<f64>().map_err(|_| format!("field {spec}: {kv}"));
            match k {
                "src" => for name in v.split('+') {
                    c.source.0 |= match name {
                        "reader" => Source::READER, "pulse" => Source::PULSE,
                        "fire" => Source::FIRE, "blocked" => Source::BLOCKED, "load" => Source::LOAD,
                        _ => return Err(format!("field {spec}: no source {name}")),
                    };
                },
                "bits" => c.bits = byte()?,
                "level" => c.level = byte()?,
                "step" => c.step = byte()?,
                "wstep" => c.wire_step = byte()?,
                "decay" => c.decay = byte()?,
                "idle" => c.idle = num()?,
                "called" => c.called = num()?,
                "wanted" => c.wanted = num()?,
                "wire" => c.wire = num()?,
                _ => return Err(format!("field {spec}: unknown key {k}")),
            }
        }
        if c.source == Source::NONE { return Err(format!("field {spec}: no src")); }
        if !(1..=8).contains(&c.bits) { return Err(format!("field {spec}: bits must be 1 to 8")); }
        Ok(c)
    }
    pub fn cap(&self) -> u8 { ((1u16 << self.bits) - 1) as u8 }
    /// The weights in quarter units (idle, called, wanted), and wire's halved, as `board` is.
    pub fn weights(&self) -> [i32; 4] {
        let q = |x: f64| (x * 4.0).round() as i32;
        [q(self.idle), q(self.called), q(self.wanted), q(self.wire) / 2]
    }
}

/// The channels' values, and the rewrites of the clock before.
pub struct Fields {
    pub on: bool,
    /// Summed over clocks: sites where some channel is not zero (they must be updated each clock).
    pub support: u64,
    chans: Vec<Chan>,
    fired: Vec<u32>,
    blocked: Vec<u32>,
}

struct Chan {
    spec: Channel,
    value: Vec<u8>,
    /// Sites where the value is not zero.
    near: Vec<u32>,
    /// `Channel::weights`.
    w: [i32; 4],
}

impl Fields {
    pub fn new(p: &Params, sites: usize) -> Fields {
        let chans: Vec<Chan> = p.fields.iter().filter(|c| c.source != Source::NONE).map(|&spec| Chan {
            spec, value: vec![0; sites], near: vec![], w: spec.weights(),
        }).collect();
        Fields { on: !chans.is_empty(), support: 0, chans, fired: vec![], blocked: vec![] }
    }
    /// Bits a site spends on the channels.
    pub fn bits(&self) -> u32 { self.chans.iter().map(|c| c.spec.bits as u32).sum() }
    /// The first channel's value at site s (the one channel a GPU keeps).
    pub fn value(&self, s: u32) -> u8 { self.chans.first().map_or(0, |c| c.value[s as usize]) }
    /// The first channel at every site (empty without a channel).
    pub fn values(&self) -> &[u8] { self.chans.first().map_or(&[], |c| &c.value) }
    /// The sites where the first channel is not zero.
    pub fn near(&self) -> &[u32] { self.chans.first().map_or(&[], |c| &c.near) }
    /// Set the first channel at site s (handing a run over from a GPU).
    pub fn set_value(&mut self, s: u32, v: u8) {
        let Some(c) = self.chans.first_mut() else { return };
        let old = std::mem::replace(&mut c.value[s as usize], v);
        if old == 0 && v != 0 { c.near.push(s); }
        if old != 0 && v == 0 { c.near.retain(|&x| x != s); }
    }
    pub fn fired(&mut self, s: u32) { if self.on { self.fired.push(s); } }
    pub fn blocked(&mut self, s: u32) { if self.on { self.blocked.push(s); } }
}

impl Lattice {
    /// Every channel's new value at every site that has one or could get one: the most of its
    /// source, each neighbour's value less the hop's falloff, and its own value less the decay.
    pub(super) fn update_fields(&mut self) {
        let readers: Vec<u32> = self.live.iter().copied().filter(|&s| (0..self.ks).any(|k| {
            let i = s as usize * self.ks + k;
            self.tags[i] != 0 && tag_of(self.tags[i]).is_consumer() && tag_of(self.tags[i]) != Tag::Eps && self.want[i]
        })).collect();
        let pulses: Vec<u32> = self.pulse_sites.iter().copied().filter(|&s| self.pulse_at[s as usize] != NONE).collect();
        let (fired, blocked) = (std::mem::take(&mut self.fields.fired), std::mem::take(&mut self.fields.blocked));
        for c in 0..self.fields.chans.len() {
            let spec = self.fields.chans[c].spec;
            let cap = spec.cap();
            let lvl = spec.level.min(cap);
            let mut src: Vec<(u32, u8)> = vec![];
            for (b, sites) in [(Source::READER, &readers), (Source::PULSE, &pulses), (Source::FIRE, &fired), (Source::BLOCKED, &blocked)] {
                if spec.source.has(b) { src.extend(sites.iter().map(|&s| (s, lvl))); }
            }
            if spec.source.has(Source::LOAD) { src.extend(self.live.iter().map(|&s| (s, (self.pairs(s) as u8).min(cap))).filter(|&(_, v)| v > 0)); }
            src.sort_unstable();
            src.dedup_by(|a, b| { if a.0 == b.0 { b.1 = b.1.max(a.1); true } else { false } });
            let old = std::mem::take(&mut self.fields.chans[c].near);
            let mut cand: Vec<u32> = src.iter().map(|&(s, _)| s).collect();
            let reach = spec.step.min(spec.wire_step);
            for &x in &old {
                cand.push(x);
                if self.fields.chans[c].value[x as usize] > reach {
                    for f in 0..self.faces { let y = self.nb(x, f); if y != u32::MAX { cand.push(y); } }
                }
            }
            cand.sort_unstable();
            cand.dedup();
            let ch = &self.fields.chans[c];
            let new: Vec<(u32, u8)> = cand.iter().map(|&x| {
                let mut v = src.binary_search_by_key(&x, |&(s, _)| s).map_or(0, |j| src[j].1);
                if spec.decay != NONE { v = v.max(ch.value[x as usize].saturating_sub(spec.decay)); }
                for f in 0..self.faces {
                    let y = self.nb(x, f);
                    if y == u32::MAX { continue; }
                    let fall = if self.used_lanes(x, f) > 0 { spec.wire_step.min(spec.step) } else { spec.step };
                    if fall != NONE { v = v.max(ch.value[y as usize].saturating_sub(fall)); }
                }
                (x, v.min(cap))
            }).collect();
            let ch = &mut self.fields.chans[c];
            for &x in &old { ch.value[x as usize] = 0; }
            for &(x, v) in &new {
                ch.value[x as usize] = v;
                if v > 0 { ch.near.push(x); }
            }
        }
        if let Some(ch) = self.fields.chans.iter().max_by_key(|c| c.near.len()) { self.fields.support += ch.near.len() as u64; }
    }

    /// The fields' energy for agent k stepping from s to t: its own term by what it is, and for an
    /// idle agent the wire it drags (pairings ps → ps + ds at s, pt → pt + dt at t).
    pub(super) fn field_step_de(&self, s: u32, k: usize, t: u32, ps: i32, ds: i32, pt: i32, dt: i32) -> i32 {
        let i = s as usize * self.ks + k;
        let class = if !self.want[i] { 0 } else if tag_of(self.tags[i]).is_producer() { 1 } else { 2 };
        let sq = |x: i32| x * x;
        let mut de = 0;
        for ch in &self.fields.chans {
            let (vs, vt) = (ch.value[s as usize] as i32, ch.value[t as usize] as i32);
            de += ch.w[class] * (vt - vs);
            if class == 0 && ch.w[3] != 0 { de += ch.w[3] * (vs * (sq(ps + ds) - sq(ps)) + vt * (sq(pt + dt) - sq(pt))); }
        }
        de
    }

    /// The fields' energy for a wire's corner moving from s (ps pairings) to w (pw).
    pub(super) fn field_flip_de(&self, s: u32, ps: i32, w: u32, pw: i32) -> i32 {
        self.fields.chans.iter().map(|ch| ch.w[3] * (ch.value[w as usize] as i32 * (2 * pw + 1) - ch.value[s as usize] as i32 * (2 * ps - 1))).sum()
    }
}
