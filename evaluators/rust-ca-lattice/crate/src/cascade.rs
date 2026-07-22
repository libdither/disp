//! Cascade substrate: the event-driven successor to the Cell64 packed lattice
//! (CASCADE_CELL_DESIGN.md). One `u64` per site; the atomic dynamic primitive is an edge
//! transaction rewriting the two words across one face. There are no protocol phases, no
//! request trees, and no activation sweeps: activity is a wake front, rewrites grow from a
//! docked seed via a single walking builder cursor, and all wire-ish matter is one uniform
//! route format (up to three single-lane routes per cell).
//!
//! Word layout (LSB up):
//!
//! | bits | field |
//! |---|---|
//! | 0..2 | kind: 0 Empty, 1 Wire, 2 Agent, 3 Seed |
//! | 2..38 | per-kind payload (36) |
//! | 38..59 | cursor overlay: present 1, rule 5, axis 3, roll 2, pc 9, reverse 1 |
//! | 59..63 | chi (obstruction pressure) |
//! | 63 | claim (parallel drivers only) |
//!
//! Payloads: Empty = reserve mark (present 1 + face 3). Wire = route count 2 + three 8-bit
//! routes + per-route hot 3 + cooldown 2. Agent = tag 4, principal 3, plane 1, has_tail 1,
//! tail 3, lane_order 1, passthrough count 2 + two 8-bit routes, nursery 1, hot 1,
//! cooldown 2. Seed = rule 5, half 1, partner 3, roll 2, stub present 1 + face 3 +
//! lane_order 1, plane 1.

use crate::lattice::Dir;
use crate::lattice::{Pos, Topo};
use crate::rules::{Tag, ALL_TAGS, RULES};

#[cfg(test)]
use crate::lattice::DIRS;

// ---------------------------------------------------------------- decoded state

/// One endpoint of a route: a face of the cell plus the lane index on that face's edge.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct EndPt {
    pub face: Dir,
    pub lane: u8, // 0 or 1
}

impl EndPt {
    pub fn key(self) -> u8 { self.face.code() * 2 + self.lane }
    fn code(self) -> u64 { ((self.face.code() as u64) << 1) | self.lane as u64 }
    fn from_code(c: u64) -> Option<Self> {
        Some(Self { face: Dir::from_code((c >> 1) as u8 & 0b111)?, lane: (c & 1) as u8 })
    }
}

/// One single-lane route through a cell between two distinct endpoints.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Route {
    pub a: EndPt,
    pub b: EndPt,
}

impl Route {
    pub fn new(a: EndPt, b: EndPt) -> Self {
        if a.key() <= b.key() { Self { a, b } } else { Self { a: b, b: a } }
    }
    pub fn ends(&self) -> [EndPt; 2] { [self.a, self.b] }
    /// The other endpoint, entering at `at`.
    pub fn through(&self, at: EndPt) -> Option<EndPt> {
        if self.a == at { Some(self.b) } else if self.b == at { Some(self.a) } else { None }
    }
    fn code(self) -> u64 { self.a.code() | (self.b.code() << 4) }
    fn from_code(c: u64) -> Option<Self> {
        let a = EndPt::from_code(c & 0xf)?;
        let b = EndPt::from_code((c >> 4) & 0xf)?;
        if a == b { return None; }
        Some(Self::new(a, b))
    }
}

/// Which side of the docked pair a seed cell replaced.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Half { Consumer, Producer }

/// Per-kind matter payload.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Cell {
    Empty {
        /// A growth reservation: the face toward the cell that owns the claim.
        reserved: Option<Dir>,
    },
    Wire {
        routes: Vec<Route>, // 1..=3
        hot: u8,            // per-route demand bits
        cooldown: u8,
        /// A growth reservation, as on Empty: the builder cursor will merge its planned
        /// matter with these routes. Entry (walking, swapping) is refused while set.
        reserved: Option<Dir>,
    },
    Agent {
        tag: Tag,
        /// Where the principal leaves the cell.
        principal: EndPt,
        /// Where each auxiliary leaves the cell: aux k is `aux[k-1]`. Only the first
        /// `arity - 1` entries are meaningful; unused entries mirror `aux[0]`. The two
        /// endpoints are independent, so an auxiliary cable can enter through any face:
        /// that freedom is what lets a walker detour one trail around a busy edge.
        aux: [EndPt; 2],
        /// Foreign routes passing through this cell (the guest configuration).
        pass: Vec<Route>, // 0..=2
        /// Placed by a growing seed and not yet live.
        nursery: bool,
        cooldown: u8, // 0..=1
    },
    Seed {
        rule: u8,
        half: Half,
        /// Face toward the other seed cell.
        partner: Dir,
        roll: u8,
        /// The dying agent's aux endpoints (meaningful up to its arity minus one; unused
        /// entries mirror the first).
        stub: [EndPt; 2],
        /// Lane of the consumed principal edge (to restore the pair on abort).
        plane: u8,
        /// A foreign route inherited from the docked agent's cell; folded into the final
        /// patch panel at resolve.
        pass: Option<Route>,
    },
}

/// The builder token that grows a seed's blocklet by walking its script.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Cursor {
    pub rule: u8,
    pub axis: Dir,
    pub roll: u8,
    pub pc: u16, // script step, 9 bits
    pub reverse: bool,
}

/// One decoded site.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Site {
    pub cell: Cell,
    pub cursor: Option<Cursor>,
    pub chi: u8,
    pub claim: bool,
}

impl Site {
    pub fn empty() -> Self {
        Self { cell: Cell::Empty { reserved: None }, cursor: None, chi: 0, claim: false }
    }
    pub fn of(cell: Cell) -> Self { Self { cell, cursor: None, chi: 0, claim: false } }
}

// ---------------------------------------------------------------- pack / unpack

const KIND_EMPTY: u64 = 0;
const KIND_WIRE: u64 = 1;
const KIND_AGENT: u64 = 2;
const KIND_SEED: u64 = 3;

const PAYLOAD_SHIFT: u32 = 2;
const CURSOR_SHIFT: u32 = 38;
const CHI_SHIFT: u32 = 59;
const CLAIM_SHIFT: u32 = 63;
pub const CHI_MAX: u8 = 15;

fn tag_code(tag: Tag) -> u64 {
    ALL_TAGS.iter().position(|t| *t == tag).expect("tag in alphabet") as u64
}
fn tag_from_code(c: u64) -> Option<Tag> { ALL_TAGS.get(c as usize).copied() }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct PackError(pub &'static str);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Word2(pub u64);

impl Word2 {
    pub const EMPTY: Word2 = Word2(0);

    pub fn chi(self) -> u8 { ((self.0 >> CHI_SHIFT) & 0xf) as u8 }
    pub fn with_chi(self, chi: u8) -> Self {
        debug_assert!(chi <= CHI_MAX);
        Word2((self.0 & !(0xfu64 << CHI_SHIFT)) | ((chi as u64 & 0xf) << CHI_SHIFT))
    }
    pub fn claimed(self) -> bool { (self.0 >> CLAIM_SHIFT) & 1 == 1 }
    pub fn with_claim(self, on: bool) -> Self {
        Word2((self.0 & !(1u64 << CLAIM_SHIFT)) | ((on as u64) << CLAIM_SHIFT))
    }

    pub fn pack(site: &Site) -> Result<Self, PackError> {
        validate(site)?;
        let payload: u64 = match &site.cell {
            Cell::Empty { reserved } => {
                let mut p = 0u64;
                if let Some(d) = reserved { p = 1 | ((d.code() as u64) << 1); }
                KIND_EMPTY | (p << PAYLOAD_SHIFT)
            }
            Cell::Wire { routes, hot, cooldown, reserved } => {
                let mut p = (routes.len() as u64 - 1) & 0b11;
                for (i, r) in routes.iter().enumerate() {
                    p |= r.code() << (2 + 8 * i);
                }
                p |= ((*hot as u64) & 0b111) << 26;
                p |= ((*cooldown as u64) & 0b11) << 29;
                if let Some(d) = reserved {
                    p |= 1 << 31;
                    p |= (d.code() as u64) << 32;
                }
                KIND_WIRE | (p << PAYLOAD_SHIFT)
            }
            Cell::Agent { tag, principal, aux, pass, nursery, cooldown } => {
                let mut p = tag_code(*tag);
                p |= principal.code() << 4;
                p |= aux[0].code() << 8;
                p |= aux[1].code() << 12;
                p |= (pass.len() as u64) << 16;
                for (i, r) in pass.iter().enumerate() {
                    p |= r.code() << (18 + 8 * i);
                }
                p |= (*nursery as u64) << 34;
                p |= ((*cooldown as u64) & 1) << 35;
                KIND_AGENT | (p << PAYLOAD_SHIFT)
            }
            Cell::Seed { rule, half, partner, roll, stub, plane, pass } => {
                let mut p = *rule as u64;
                p |= (matches!(half, Half::Producer) as u64) << 5;
                p |= (partner.code() as u64) << 6;
                p |= ((*roll as u64) & 0b11) << 9;
                p |= stub[0].code() << 11;
                p |= stub[1].code() << 15;
                p |= (*plane as u64) << 19;
                if let Some(r) = pass {
                    p |= 1 << 20;
                    p |= r.code() << 21;
                }
                KIND_SEED | (p << PAYLOAD_SHIFT)
            }
        };
        let mut w = payload;
        if let Some(c) = &site.cursor {
            let mut cw = 1u64;
            cw |= (c.rule as u64) << 1;
            cw |= (c.axis.code() as u64) << 6;
            cw |= ((c.roll as u64) & 0b11) << 9;
            cw |= ((c.pc as u64) & 0x1ff) << 11;
            cw |= (c.reverse as u64) << 20;
            w |= cw << CURSOR_SHIFT;
        }
        w |= ((site.chi as u64) & 0xf) << CHI_SHIFT;
        w |= (site.claim as u64) << CLAIM_SHIFT;
        Ok(Word2(w))
    }

    pub fn unpack(self) -> Result<Site, PackError> {
        let kind = self.0 & 0b11;
        let p = (self.0 >> PAYLOAD_SHIFT) & ((1u64 << 36) - 1);
        let cell = match kind {
            KIND_EMPTY => {
                let reserved = if p & 1 == 1 {
                    Some(Dir::from_code(((p >> 1) & 0b111) as u8).ok_or(PackError("reserve face"))?)
                } else if p == 0 {
                    None
                } else {
                    return Err(PackError("empty payload bits"));
                };
                Cell::Empty { reserved }
            }
            KIND_WIRE => {
                let count = (p & 0b11) as usize + 1;
                if count > 3 { return Err(PackError("wire route count")); }
                let mut routes = Vec::with_capacity(count);
                for i in 0..count {
                    routes.push(Route::from_code((p >> (2 + 8 * i)) & 0xff).ok_or(PackError("route"))?);
                }
                let hot = ((p >> 26) & 0b111) as u8;
                let cooldown = ((p >> 29) & 0b11) as u8;
                let reserved = if (p >> 31) & 1 == 1 {
                    Some(Dir::from_code(((p >> 32) & 0b111) as u8).ok_or(PackError("wire reserve"))?)
                } else {
                    None
                };
                Cell::Wire { routes, hot, cooldown, reserved }
            }
            KIND_AGENT => {
                let tag = tag_from_code(p & 0xf).ok_or(PackError("tag"))?;
                let principal = EndPt::from_code((p >> 4) & 0xf).ok_or(PackError("principal"))?;
                let aux = [
                    EndPt::from_code((p >> 8) & 0xf).ok_or(PackError("aux1"))?,
                    EndPt::from_code((p >> 12) & 0xf).ok_or(PackError("aux2"))?,
                ];
                let pt_count = ((p >> 16) & 0b11) as usize;
                if pt_count > 2 { return Err(PackError("passthrough count")); }
                let mut pass = Vec::with_capacity(pt_count);
                for i in 0..pt_count {
                    pass.push(Route::from_code((p >> (18 + 8 * i)) & 0xff).ok_or(PackError("pass route"))?);
                }
                let nursery = (p >> 34) & 1 == 1;
                let cooldown = ((p >> 35) & 1) as u8;
                Cell::Agent { tag, principal, aux, pass, nursery, cooldown }
            }
            KIND_SEED => {
                let rule = (p & 0b11111) as u8;
                if rule as usize >= RULES.len() { return Err(PackError("rule index")); }
                let half = if (p >> 5) & 1 == 1 { Half::Producer } else { Half::Consumer };
                let partner = Dir::from_code(((p >> 6) & 0b111) as u8).ok_or(PackError("partner"))?;
                let roll = ((p >> 9) & 0b11) as u8;
                let stub = [
                    EndPt::from_code((p >> 11) & 0xf).ok_or(PackError("stub1"))?,
                    EndPt::from_code((p >> 15) & 0xf).ok_or(PackError("stub2"))?,
                ];
                let plane = ((p >> 19) & 1) as u8;
                let pass = if (p >> 20) & 1 == 1 {
                    Some(Route::from_code((p >> 21) & 0xff).ok_or(PackError("seed pass"))?)
                } else {
                    None
                };
                Cell::Seed { rule, half, partner, roll, stub, plane, pass }
            }
            _ => unreachable!(),
        };
        let cw = (self.0 >> CURSOR_SHIFT) & ((1u64 << 21) - 1);
        let cursor = if cw & 1 == 1 {
            Some(Cursor {
                rule: ((cw >> 1) & 0b11111) as u8,
                axis: Dir::from_code(((cw >> 6) & 0b111) as u8).ok_or(PackError("cursor axis"))?,
                roll: ((cw >> 9) & 0b11) as u8,
                pc: ((cw >> 11) & 0x1ff) as u16,
                reverse: (cw >> 20) & 1 == 1,
            })
        } else {
            if cw != 0 { return Err(PackError("cursor bits without presence")); }
            None
        };
        let site = Site {
            cell,
            cursor,
            chi: self.chi(),
            claim: self.claimed(),
        };
        validate(&site)?;
        Ok(site)
    }
}

/// Every (face, lane) a cell's matter exposes to its edges. Distinctness is the geometric
/// well-formedness condition: an edge lane belongs to at most one attachment.
pub fn exposures(cell: &Cell) -> Vec<EndPt> {
    let mut out = vec![];
    match cell {
        Cell::Empty { .. } => {}
        Cell::Wire { routes, .. } => {
            for r in routes {
                out.push(r.a);
                out.push(r.b);
            }
        }
        Cell::Agent { tag, principal, aux, pass, .. } => {
            out.push(*principal);
            for k in 0..tag.arity().saturating_sub(1) {
                out.push(aux[k]);
            }
            for r in pass {
                out.push(r.a);
                out.push(r.b);
            }
        }
        Cell::Seed { rule, half, partner, plane, stub, pass, .. } => {
            out.push(EndPt { face: *partner, lane: *plane });
            let tag = match half {
                Half::Consumer => RULES[*rule as usize].consumer,
                Half::Producer => RULES[*rule as usize].producer,
            };
            for k in 0..tag.arity().saturating_sub(1) {
                out.push(stub[k]);
            }
            if let Some(r) = pass {
                out.push(r.a);
                out.push(r.b);
            }
        }
    }
    out
}

fn validate(site: &Site) -> Result<(), PackError> {
    let ex = exposures(&site.cell);
    for (i, a) in ex.iter().enumerate() {
        for b in &ex[i + 1..] {
            if a == b { return Err(PackError("duplicate exposure")); }
        }
    }
    match &site.cell {
        Cell::Wire { routes, hot, cooldown, .. } => {
            if routes.is_empty() || routes.len() > 3 { return Err(PackError("wire route count")); }
            if *hot >= 8 || *cooldown >= 4 { return Err(PackError("wire signal range")); }
            for r in routes {
                if r.a == r.b { return Err(PackError("degenerate route")); }
            }
        }
        Cell::Agent { pass, cooldown, .. } => {
            if pass.len() > 2 || *cooldown > 1 {
                return Err(PackError("agent field range"));
            }
        }
        Cell::Seed { .. } => {}
        Cell::Empty { .. } => {}
    }
    if let Some(c) = &site.cursor {
        if c.rule as usize >= RULES.len() || c.roll > 3 || c.pc > 0x1ff {
            return Err(PackError("cursor range"));
        }
        // A cursor on an empty cell is legal: an Eps-consumer seed resolves to empty
        // matter while the finalize pass is still running.
    }
    if site.chi > CHI_MAX { return Err(PackError("chi range")); }
    Ok(())
}

// ---------------------------------------------------------------- rotation

/// The four perpendicular directions of an axis, in a fixed rotation cycle.
const fn perp_cycle(axis: Dir) -> [Dir; 4] {
    match axis {
        Dir::E => [Dir::N, Dir::U, Dir::S, Dir::D],
        Dir::W => [Dir::N, Dir::D, Dir::S, Dir::U],
        Dir::N => [Dir::E, Dir::U, Dir::W, Dir::D],
        Dir::S => [Dir::E, Dir::D, Dir::W, Dir::U],
        Dir::U => [Dir::N, Dir::E, Dir::S, Dir::W],
        Dir::D => [Dir::N, Dir::W, Dir::S, Dir::E],
    }
}

/// Rotate a canonical-frame direction (blocklets are compiled with axis = E) into the
/// world frame given the dock axis and roll.
pub fn rot_dir(d: Dir, axis: Dir, roll: u8) -> Dir {
    let cycle = perp_cycle(axis);
    let canon = perp_cycle(Dir::E);
    match d {
        Dir::E => axis,
        Dir::W => axis.opp(),
        other => {
            let i = canon.iter().position(|c| *c == other).expect("perp of E");
            cycle[(i + roll as usize) % 4]
        }
    }
}

/// Rotate a canonical offset (x along E, y along S, z along U) into the world frame.
pub fn rot_pos(p: Pos, axis: Dir, roll: u8) -> Pos {
    let e = rot_dir(Dir::E, axis, roll).delta();
    let s = rot_dir(Dir::S, axis, roll).delta();
    let u = rot_dir(Dir::U, axis, roll).delta();
    (
        p.0 * e.0 + p.1 * s.0 + p.2 * u.0,
        p.0 * e.1 + p.1 * s.1 + p.2 * u.1,
        p.0 * e.2 + p.1 * s.2 + p.2 * u.2,
    )
}

pub fn rot_endpt(e: EndPt, axis: Dir, roll: u8) -> EndPt {
    EndPt { face: rot_dir(e.face, axis, roll), lane: e.lane }
}

pub fn rot_route(r: Route, axis: Dir, roll: u8) -> Route {
    Route::new(rot_endpt(r.a, axis, roll), rot_endpt(r.b, axis, roll))
}

pub fn rot_cell(cell: &Cell, axis: Dir, roll: u8) -> Cell {
    match cell {
        Cell::Empty { reserved } => Cell::Empty {
            reserved: reserved.map(|d| rot_dir(d, axis, roll)),
        },
        Cell::Wire { routes, hot, cooldown, reserved } => Cell::Wire {
            routes: routes.iter().map(|r| rot_route(*r, axis, roll)).collect(),
            hot: *hot,
            cooldown: *cooldown,
            reserved: reserved.map(|d| rot_dir(d, axis, roll)),
        },
        Cell::Agent { tag, principal, aux, pass, nursery, cooldown } => Cell::Agent {
            tag: *tag,
            principal: rot_endpt(*principal, axis, roll),
            aux: [rot_endpt(aux[0], axis, roll), rot_endpt(aux[1], axis, roll)],
            pass: pass.iter().map(|r| rot_route(*r, axis, roll)).collect(),
            nursery: *nursery,
            cooldown: *cooldown,
        },
        Cell::Seed { rule, half, partner, roll: sroll, stub, plane, pass } => Cell::Seed {
            rule: *rule,
            half: *half,
            partner: rot_dir(*partner, axis, roll),
            roll: *sroll,
            stub: [rot_endpt(stub[0], axis, roll), rot_endpt(stub[1], axis, roll)],
            plane: *plane,
            pass: pass.map(|r| rot_route(r, axis, roll)),
        },
    }
}

// ---------------------------------------------------------------- grid

/// Sparse cascade grid. Absent entries are the all-zero empty word. Observer sidecars
/// (stable ids, counters) never enter transitions.
#[derive(Clone, Debug)]
pub struct Grid2 {
    pub cells: std::collections::BTreeMap<Pos, Word2>,
    pub topo: Topo,
    pub sid: std::collections::BTreeMap<Pos, u32>,
    /// Seeds remember the docked pair's observer ids until resolve.
    pub seed_sids: std::collections::BTreeMap<Pos, (u32, u32)>,
    pub transport: u64,
    pub rewrites: u64,
}

impl Grid2 {
    pub fn new(topo: Topo) -> Self {
        Self {
            cells: Default::default(),
            topo,
            sid: Default::default(),
            seed_sids: Default::default(),
            transport: 0,
            rewrites: 0,
        }
    }

    pub fn word(&self, p: Pos) -> Word2 {
        if self.topo.in_bounds(p) {
            self.cells.get(&p).copied().unwrap_or(Word2::EMPTY)
        } else {
            // Out-of-topology reads as a permanently reserved boundary: nothing enters.
            Word2::pack(&Site {
                cell: Cell::Empty { reserved: Some(Dir::U) },
                cursor: None,
                chi: 0,
                claim: true,
            })
            .expect("boundary word")
        }
    }

    pub fn site(&self, p: Pos) -> Site {
        self.word(p).unpack().expect("stored word must be canonical")
    }

    pub fn set(&mut self, p: Pos, site: &Site) {
        assert!(self.topo.in_bounds(p), "write outside topology");
        let w = Word2::pack(site).expect("engine produced an invalid site");
        if w == Word2::EMPTY {
            self.cells.remove(&p);
        } else {
            self.cells.insert(p, w);
        }
    }

    pub fn agents(&self) -> impl Iterator<Item = (Pos, Site)> + '_ {
        self.cells.iter().filter_map(|(p, w)| {
            let s = w.unpack().expect("canonical");
            matches!(s.cell, Cell::Agent { .. }).then_some((*p, s))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn roundtrip(site: Site) {
        let w = Word2::pack(&site).expect("packs");
        let back = w.unpack().expect("unpacks");
        assert_eq!(site, back, "word 0x{:016x}", w.0);
    }

    #[test]
    fn empty_roundtrip() {
        roundtrip(Site::empty());
        for d in DIRS {
            roundtrip(Site::of(Cell::Empty { reserved: Some(d) }));
        }
        assert_eq!(Word2::pack(&Site::empty()).unwrap(), Word2::EMPTY);
    }

    #[test]
    fn wire_roundtrip_exhaustive_singles() {
        // Every distinct endpoint pair as a single route.
        let mut count = 0;
        for fa in DIRS {
            for la in 0..2u8 {
                for fb in DIRS {
                    for lb in 0..2u8 {
                        let a = EndPt { face: fa, lane: la };
                        let b = EndPt { face: fb, lane: lb };
                        if a.key() >= b.key() { continue; }
                        roundtrip(Site::of(Cell::Wire { reserved: None,
                            routes: vec![Route::new(a, b)],
                            hot: 0,
                            cooldown: 0,
                        }));
                        count += 1;
                    }
                }
            }
        }
        assert_eq!(count, 12 * 11 / 2);
    }

    #[test]
    fn wire_triple_route() {
        // Three parallel-ish routes: a full cell (two trails plus one foreign crossing).
        let r0 = Route::new(EndPt { face: Dir::W, lane: 0 }, EndPt { face: Dir::E, lane: 0 });
        let r1 = Route::new(EndPt { face: Dir::W, lane: 1 }, EndPt { face: Dir::E, lane: 1 });
        let r2 = Route::new(EndPt { face: Dir::N, lane: 0 }, EndPt { face: Dir::S, lane: 0 });
        roundtrip(Site::of(Cell::Wire { routes: vec![r0, r1, r2], hot: 0b101, cooldown: 3, reserved: None }));
    }

    #[test]
    fn wire_rejects_duplicate_exposure() {
        let r0 = Route::new(EndPt { face: Dir::W, lane: 0 }, EndPt { face: Dir::E, lane: 0 });
        let r1 = Route::new(EndPt { face: Dir::W, lane: 0 }, EndPt { face: Dir::N, lane: 0 });
        let site = Site::of(Cell::Wire { routes: vec![r0, r1], hot: 0, cooldown: 0, reserved: None });
        assert!(Word2::pack(&site).is_err());
    }

    #[test]
    fn agent_roundtrip_all_tags_orientations() {
        for tag in ALL_TAGS {
            for pface in DIRS {
                for plane in 0..2u8 {
                    let principal = EndPt { face: pface, lane: plane };
                    for aface in DIRS {
                        if tag.arity() >= 2 && aface == pface && (plane == 0 || tag.arity() == 3) {
                            continue; // an aux would collide with the principal exposure
                        }
                        let aux1 = EndPt { face: aface, lane: 0 };
                        let aux2 = if tag.arity() == 3 {
                            EndPt { face: aface, lane: 1 }
                        } else {
                            aux1
                        };
                        roundtrip(Site::of(Cell::Agent {
                            tag,
                            principal,
                            aux: [aux1, aux2],
                            pass: vec![],
                            nursery: false,
                            cooldown: 0,
                        }));
                    }
                }
            }
        }
    }

    #[test]
    fn agent_split_aux_faces() {
        // The two auxiliaries may leave through entirely different faces.
        roundtrip(Site::of(Cell::Agent {
            tag: Tag::F,
            principal: EndPt { face: Dir::E, lane: 0 },
            aux: [EndPt { face: Dir::W, lane: 1 }, EndPt { face: Dir::U, lane: 0 }],
            pass: vec![],
            nursery: false,
            cooldown: 1,
        }));
    }

    #[test]
    fn agent_with_passthrough_guest() {
        // An arity-two producer crossing a foreign route: the uniform guest configuration.
        let foreign = Route::new(EndPt { face: Dir::N, lane: 0 }, EndPt { face: Dir::S, lane: 0 });
        roundtrip(Site::of(Cell::Agent {
            tag: Tag::S,
            principal: EndPt { face: Dir::E, lane: 1 },
            aux: [EndPt { face: Dir::W, lane: 0 }, EndPt { face: Dir::W, lane: 0 }],
            pass: vec![foreign],
            nursery: true,
            cooldown: 1,
        }));
    }

    #[test]
    fn agent_rejects_passthrough_collision() {
        // Passthrough may not use the principal's (face, lane).
        let foreign = Route::new(EndPt { face: Dir::E, lane: 0 }, EndPt { face: Dir::S, lane: 0 });
        let site = Site::of(Cell::Agent {
            tag: Tag::L,
            principal: EndPt { face: Dir::E, lane: 0 },
            aux: [EndPt { face: Dir::E, lane: 0 }; 2],
            pass: vec![foreign],
            nursery: false,
            cooldown: 0,
        });
        assert!(Word2::pack(&site).is_err());
    }

    #[test]
    fn seed_roundtrip_all_rules() {
        for (i, rule) in RULES.iter().enumerate() {
            for (half, _tag) in [(Half::Consumer, rule.consumer), (Half::Producer, rule.producer)] {
                let stub1 = EndPt { face: Dir::N, lane: 0 };
                let stub2 = EndPt { face: Dir::N, lane: 1 };
                roundtrip(Site::of(Cell::Seed {
                    rule: i as u8,
                    half,
                    partner: Dir::E,
                    roll: (i % 4) as u8,
                    stub: [stub1, stub2],
                    plane: (i % 2) as u8,
                    pass: None,
                }));
            }
        }
    }

    #[test]
    fn cursor_roundtrip() {
        let mut site = Site::of(Cell::Seed {
            rule: 2,
            half: Half::Consumer,
            partner: Dir::E,
            roll: 1,
            stub: [EndPt { face: Dir::N, lane: 0 }, EndPt { face: Dir::N, lane: 1 }],
            plane: 0,
            pass: Some(Route::new(
                EndPt { face: Dir::S, lane: 0 },
                EndPt { face: Dir::W, lane: 1 },
            )),
        });
        site.cursor = Some(Cursor { rule: 2, axis: Dir::E, roll: 1, pc: 317, reverse: true });
        site.chi = 13;
        roundtrip(site);
    }

    #[test]
    fn rotation_is_a_bijection_preserving_adjacency() {
        for axis in DIRS {
            for roll in 0..4u8 {
                // Distinct dirs stay distinct and opposites stay opposite.
                for d in DIRS {
                    assert_eq!(rot_dir(d.opp(), axis, roll), rot_dir(d, axis, roll).opp());
                }
                let mut seen: Vec<Dir> = DIRS.iter().map(|d| rot_dir(*d, axis, roll)).collect();
                seen.sort();
                seen.dedup();
                assert_eq!(seen.len(), 6);
                // Position rotation agrees with direction rotation.
                for d in DIRS {
                    assert_eq!(rot_pos(d.delta(), axis, roll), rot_dir(d, axis, roll).delta());
                }
                // E maps to the axis.
                assert_eq!(rot_dir(Dir::E, axis, roll), axis);
            }
        }
    }

    #[test]
    fn chi_and_claim_do_not_disturb_matter() {
        let site = Site::of(Cell::Wire { reserved: None,
            routes: vec![Route::new(
                EndPt { face: Dir::W, lane: 0 },
                EndPt { face: Dir::E, lane: 0 },
            )],
            hot: 1,
            cooldown: 0,
        });
        let w = Word2::pack(&site).unwrap();
        let w2 = w.with_chi(11).with_claim(true);
        let s2 = w2.unpack().unwrap();
        assert_eq!(s2.cell, site.cell);
        assert_eq!(s2.chi, 11);
        assert!(s2.claim);
        assert_eq!(w2.with_claim(false).with_chi(0), w);
    }
}
