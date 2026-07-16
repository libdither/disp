//! The complete dynamic state of one lattice site, packed into one machine word.
//!
//! The automaton stores no coordinates, endpoint ids, transaction ids, clocks, or heap
//! pointers.  A transition decodes one center word and its six face-neighbor words, then
//! replaces only the center word.  Observer metadata (stable display ids and events) lives
//! outside this representation.

use crate::rules::Tag;

pub const MATTER_BITS: u32 = 20;
pub const CONTROL_BITS: u32 = 28;
pub const CHI_BITS: u32 = 8;
pub const SIGMA_BITS: u32 = 8;
pub const TOTAL_BITS: u32 = MATTER_BITS + CONTROL_BITS + CHI_BITS + SIGMA_BITS;

const MATTER_MASK: u64 = (1u64 << MATTER_BITS) - 1;
const CONTROL_MASK: u64 = (1u64 << CONTROL_BITS) - 1;
const CONTROL_SHIFT: u32 = MATTER_BITS;
const CHI_SHIFT: u32 = CONTROL_SHIFT + CONTROL_BITS;
const SIGMA_SHIFT: u32 = CHI_SHIFT + CHI_BITS;

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
#[repr(u8)]
pub enum Dir { N, E, S, W, U, D }

pub const DIRS: [Dir; 6] = [Dir::N, Dir::E, Dir::S, Dir::W, Dir::U, Dir::D];

impl Dir {
    pub const fn code(self) -> u8 { self as u8 }

    pub const fn from_code(code: u8) -> Option<Self> {
        match code {
            0 => Some(Self::N), 1 => Some(Self::E), 2 => Some(Self::S),
            3 => Some(Self::W), 4 => Some(Self::U), 5 => Some(Self::D),
            _ => None,
        }
    }

    pub const fn delta(self) -> (i32, i32, i32) {
        match self {
            Self::N => (0, -1, 0), Self::E => (1, 0, 0), Self::S => (0, 1, 0),
            Self::W => (-1, 0, 0), Self::U => (0, 0, 1), Self::D => (0, 0, -1),
        }
    }

    pub const fn opp(self) -> Self {
        match self {
            Self::N => Self::S, Self::S => Self::N,
            Self::E => Self::W, Self::W => Self::E,
            Self::U => Self::D, Self::D => Self::U,
        }
    }

    pub const fn perp(self) -> [Self; 4] {
        match self {
            Self::N | Self::S => [Self::E, Self::W, Self::U, Self::D],
            Self::E | Self::W => [Self::N, Self::S, Self::U, Self::D],
            Self::U | Self::D => [Self::N, Self::E, Self::S, Self::W],
        }
    }

    pub const fn ch(self) -> char {
        match self { Self::N => 'N', Self::E => 'E', Self::S => 'S', Self::W => 'W', Self::U => 'U', Self::D => 'D' }
    }
}

macro_rules! bounded {
    ($name:ident, $max:expr) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
        pub struct $name(u8);
        impl $name {
            pub const MAX: u8 = $max;
            pub const fn new(value: u8) -> Option<Self> {
                if value <= Self::MAX { Some(Self(value)) } else { None }
            }
            pub const fn get(self) -> u8 { self.0 }
        }
    };
}

bounded!(U3, 0b111);
bounded!(U4, 0b1111);
bounded!(U6, 0b11_1111);
bounded!(LaneMask, 0b11);
bounded!(RuleId, 25);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum LaneCount { One, Two }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum Pull { None, Request, Ack, Hold }

impl Pull {
    const fn from_code(code: u8) -> Option<Self> {
        match code {
            0 => Some(Self::None), 1 => Some(Self::Request),
            2 => Some(Self::Ack), 3 => Some(Self::Hold), _ => None,
        }
    }
}

/// An unordered pair of distinct faces.  Its packed code is one of the 15 edges of K6.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct FacePair { pub a: Dir, pub b: Dir }

impl FacePair {
    pub fn new(a: Dir, b: Dir) -> Option<Self> {
        (a != b).then(|| if a < b { Self { a, b } } else { Self { a: b, b: a } })
    }

    pub fn contains(self, d: Dir) -> bool { self.a == d || self.b == d }

    pub fn other(self, d: Dir) -> Option<Dir> {
        if self.a == d { Some(self.b) } else if self.b == d { Some(self.a) } else { None }
    }

    fn code(self) -> u8 {
        let (a, b) = (self.a.code(), self.b.code());
        debug_assert!(a < b);
        // Number of pairs beginning with a smaller first endpoint, then the offset at a.
        let before = a * 5 - (a * a.saturating_sub(1)) / 2;
        before + (b - a - 1)
    }

    fn from_code(mut code: u8) -> Option<Self> {
        if code >= 15 { return None; }
        for a in 0..5 {
            let row = 5 - a;
            if code < row {
                return Some(Self {
                    a: Dir::from_code(a)?,
                    b: Dir::from_code(a + 1 + code)?,
                });
            }
            code -= row;
        }
        None
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Matter {
    Empty,
    /// `tail` is absent for arity-one tags, a single strand for arity two, and a two-lane
    /// cable for arity three. `aux_flip` maps physical cable lanes to semantic ports 1/2.
    Agent { tag: Tag, principal: Dir, tail: Option<Dir>, aux_flip: bool },
    /// One exclusive geometric route. A two-lane route shares its pull and cooldown state.
    Link {
        ends: FacePair,
        lanes: LaneCount,
        twist: bool,
        hot: LaneMask,
        cooldown: U3,
        pull: [Pull; 2],
    },
    /// A projection-transparent boundary from one two-lane trunk to two ordered branches.
    Zip {
        trunk: Dir,
        branches: [Dir; 2],
        twist: bool,
        hot: LaneMask,
        cooldown: U3,
        pull: [Pull; 2],
    },
    /// Two independent single-lane routes sharing a fixed crossing cell. Unlike the old
    /// switchbox this is not an arbitrary three-route container: neither route may join,
    /// repoint, or displace the other in place.
    Cross {
        routes: [FacePair; 2],
        hot: LaneMask,
        cooldown: U3,
        pull: [Pull; 2],
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum Phase { Offer, Ack, Commit, Done, Abort }

impl Phase {
    const fn from_code(code: u8) -> Option<Self> {
        match code {
            0 => Some(Self::Offer), 1 => Some(Self::Ack), 2 => Some(Self::Commit),
            3 => Some(Self::Done), 4 => Some(Self::Abort), _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum LineRole { Source, Target, Tail }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum CableRole { Driver, Trunk, Branch }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum ZipMode { Zip, Unzip }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum ReliefRole { Driver, Spine, Corner, Endpoint }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum RewriteRole { Driver, Boundary, Probe, Seat, Wire, Cleanup, AbortRelay }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Control {
    Idle,
    Translate {
        role: LineRole,
        phase: Phase,
        toward: Dir,
        exit: Dir,
        old_tail: Dir,
        epoch: bool,
    },
    CableShift {
        role: CableRole,
        phase: Phase,
        mode: ZipMode,
        axis: Dir,
        side: Dir,
        epoch: bool,
    },
    PressureRelief {
        role: ReliefRole,
        phase: Phase,
        axis: Dir,
        side: Dir,
        hot: LaneMask,
        epoch: bool,
    },
    Rewrite {
        role: RewriteRole,
        phase: Phase,
        rule: RuleId,
        axis: Dir,
        side: u8, // 0..3: index into axis.perp()
        lift: bool,
        slot: U6,
        epoch: bool,
    },
    Contest { source: Dir, stable: U4, epoch: bool },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct DecodedCell {
    pub matter: Matter,
    pub control: Control,
    pub chi: u8,
    pub sigma: u8,
}

impl Default for DecodedCell {
    fn default() -> Self {
        Self { matter: Matter::Empty, control: Control::Idle, chi: 0, sigma: 0 }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PackError {
    InvalidAgentGeometry,
    InvalidLinkSignals,
    InvalidZipperGeometry,
    InvalidControlGeometry,
    InvalidMatterWord,
    InvalidControlWord,
}

#[repr(transparent)]
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CellWord(pub u64);

impl core::fmt::Debug for CellWord {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "CellWord({:#018x})", self.0)
    }
}

impl CellWord {
    pub fn pack(cell: DecodedCell) -> Result<Self, PackError> {
        let matter = pack_matter(cell.matter)?;
        let control = pack_control(cell.control)?;
        debug_assert!(matter <= MATTER_MASK && control <= CONTROL_MASK);
        Ok(Self(
            matter
                | (control << CONTROL_SHIFT)
                | ((cell.chi as u64) << CHI_SHIFT)
                | ((cell.sigma as u64) << SIGMA_SHIFT),
        ))
    }

    pub fn unpack(self) -> Result<DecodedCell, PackError> {
        Ok(DecodedCell {
            matter: unpack_matter(self.0 & MATTER_MASK)?,
            control: unpack_control((self.0 >> CONTROL_SHIFT) & CONTROL_MASK)?,
            chi: (self.0 >> CHI_SHIFT) as u8,
            sigma: (self.0 >> SIGMA_SHIFT) as u8,
        })
    }

    pub const fn chi(self) -> u8 { (self.0 >> CHI_SHIFT) as u8 }
    pub const fn sigma(self) -> u8 { (self.0 >> SIGMA_SHIFT) as u8 }
}

fn tag_code(tag: Tag) -> u8 { tag as u8 }

fn tag_from_code(code: u8) -> Option<Tag> {
    crate::rules::ALL_TAGS.get(code as usize).copied()
}

fn encode_ordered_pair(a: Dir, b: Dir) -> Option<u8> {
    if a == b { return None; }
    let ai = a.code();
    let bi = b.code();
    Some(ai * 5 + if bi < ai { bi } else { bi - 1 })
}

fn decode_ordered_pair(code: u8) -> Option<(Dir, Dir)> {
    if code >= 30 { return None; }
    let ai = code / 5;
    let mut bi = code % 5;
    if bi >= ai { bi += 1; }
    Some((Dir::from_code(ai)?, Dir::from_code(bi)?))
}

fn encode_zip_geometry(trunk: Dir, branches: [Dir; 2]) -> Option<u8> {
    if trunk == branches[0] || trunk == branches[1] || branches[0] == branches[1] { return None; }
    let t = trunk.code();
    let mut b0 = branches[0].code();
    if b0 > t { b0 -= 1; }
    let mut b1 = branches[1].code();
    let lo = trunk.code().min(branches[0].code());
    let hi = trunk.code().max(branches[0].code());
    if b1 > hi { b1 -= 2; } else if b1 > lo { b1 -= 1; }
    Some(t * 20 + b0 * 4 + b1)
}

fn decode_zip_geometry(code: u8) -> Option<(Dir, [Dir; 2])> {
    if code >= 120 { return None; }
    let t = code / 20;
    let rest = code % 20;
    let b0_rank = rest / 4;
    let b1_rank = rest % 4;
    let mut remaining: Vec<u8> = (0..6).filter(|x| *x != t).collect();
    let b0 = *remaining.get(b0_rank as usize)?;
    remaining.retain(|x| *x != b0);
    let b1 = *remaining.get(b1_rank as usize)?;
    Some((Dir::from_code(t)?, [Dir::from_code(b0)?, Dir::from_code(b1)?]))
}

fn encode_cross_geometry(mut routes: [FacePair; 2]) -> Option<u8> {
    if routes[0].a == routes[1].a || routes[0].a == routes[1].b
        || routes[0].b == routes[1].a || routes[0].b == routes[1].b
    {
        return None;
    }
    if routes[1].code() < routes[0].code() { routes.swap(0, 1); }
    let mut code = 0;
    for a in 0..15 {
        let first = FacePair::from_code(a)?;
        for b in a + 1..15 {
            let second = FacePair::from_code(b)?;
            if first.a == second.a || first.a == second.b
                || first.b == second.a || first.b == second.b
            {
                continue;
            }
            if [first, second] == routes { return Some(code); }
            code += 1;
        }
    }
    None
}

fn decode_cross_geometry(want: u8) -> Option<[FacePair; 2]> {
    if want >= 45 { return None; }
    let mut code = 0;
    for a in 0..15 {
        let first = FacePair::from_code(a)?;
        for b in a + 1..15 {
            let second = FacePair::from_code(b)?;
            if first.a == second.a || first.a == second.b
                || first.b == second.a || first.b == second.b
            {
                continue;
            }
            if code == want { return Some([first, second]); }
            code += 1;
        }
    }
    None
}

fn pack_matter(matter: Matter) -> Result<u64, PackError> {
    let (kind, data): (u64, u64) = match matter {
        Matter::Empty => (0, 0),
        Matter::Agent { tag, principal, tail, aux_flip } => {
            let geometry = match (tag.arity(), tail) {
                (1, None) => principal.code(),
                (2 | 3, Some(tail)) => encode_ordered_pair(principal, tail)
                    .ok_or(PackError::InvalidAgentGeometry)?,
                _ => return Err(PackError::InvalidAgentGeometry),
            };
            if tag.arity() != 3 && aux_flip { return Err(PackError::InvalidAgentGeometry); }
            let data = tag_code(tag) as u64
                | ((geometry as u64) << 4)
                | ((aux_flip as u64) << 9);
            (1, data)
        }
        Matter::Link { ends, lanes, twist, hot, cooldown, pull } => {
            if lanes == LaneCount::One && (twist || hot.get() & 0b10 != 0) {
                return Err(PackError::InvalidLinkSignals);
            }
            let data = ends.code() as u64
                | ((lanes as u64) << 4)
                | ((twist as u64) << 5)
                | ((hot.get() as u64) << 6)
                | ((cooldown.get() as u64) << 8)
                | ((pull[0] as u64) << 11)
                | ((pull[1] as u64) << 13);
            (2, data)
        }
        Matter::Zip { trunk, branches, twist, hot, cooldown, pull } => {
            let geometry = encode_zip_geometry(trunk, branches)
                .ok_or(PackError::InvalidZipperGeometry)?;
            let data = geometry as u64
                | ((twist as u64) << 7)
                | ((hot.get() as u64) << 8)
                | ((cooldown.get() as u64) << 10)
                | ((pull[0] as u64) << 13)
                | ((pull[1] as u64) << 15);
            (3, data)
        }
        Matter::Cross { routes, hot, cooldown, pull } => {
            let geometry = encode_cross_geometry(routes)
                .ok_or(PackError::InvalidLinkSignals)?;
            let data = geometry as u64
                | ((hot.get() as u64) << 6)
                | ((cooldown.get() as u64) << 8)
                | ((pull[0] as u64) << 11)
                | ((pull[1] as u64) << 13);
            (4, data)
        }
    };
    Ok(kind | (data << 3))
}

fn unpack_matter(word: u64) -> Result<Matter, PackError> {
    let kind = word & 0b111;
    let data = word >> 3;
    match kind {
        0 if data == 0 => Ok(Matter::Empty),
        0 => Err(PackError::InvalidMatterWord),
        1 => {
            if data >> 10 != 0 { return Err(PackError::InvalidMatterWord); }
            let tag = tag_from_code((data & 0xf) as u8).ok_or(PackError::InvalidMatterWord)?;
            let geometry = ((data >> 4) & 0x1f) as u8;
            let aux_flip = (data >> 9) & 1 != 0;
            let (principal, tail) = match tag.arity() {
                1 => (Dir::from_code(geometry).ok_or(PackError::InvalidMatterWord)?, None),
                2 | 3 => {
                    let (p, t) = decode_ordered_pair(geometry).ok_or(PackError::InvalidMatterWord)?;
                    (p, Some(t))
                }
                _ => return Err(PackError::InvalidMatterWord),
            };
            if tag.arity() != 3 && aux_flip { return Err(PackError::InvalidMatterWord); }
            Ok(Matter::Agent { tag, principal, tail, aux_flip })
        }
        2 => {
            if data >> 15 != 0 { return Err(PackError::InvalidMatterWord); }
            let ends = FacePair::from_code((data & 0xf) as u8).ok_or(PackError::InvalidMatterWord)?;
            let lanes = if (data >> 4) & 1 == 0 { LaneCount::One } else { LaneCount::Two };
            let twist = (data >> 5) & 1 != 0;
            let hot = LaneMask::new(((data >> 6) & 0b11) as u8).unwrap();
            let cooldown = U3::new(((data >> 8) & 0b111) as u8).unwrap();
            let pull = [
                Pull::from_code(((data >> 11) & 0b11) as u8).unwrap(),
                Pull::from_code(((data >> 13) & 0b11) as u8).unwrap(),
            ];
            if lanes == LaneCount::One && (twist || hot.get() & 0b10 != 0) {
                return Err(PackError::InvalidMatterWord);
            }
            Ok(Matter::Link { ends, lanes, twist, hot, cooldown, pull })
        }
        3 => {
            if data >> 17 != 0 { return Err(PackError::InvalidMatterWord); }
            let (trunk, branches) = decode_zip_geometry((data & 0x7f) as u8)
                .ok_or(PackError::InvalidMatterWord)?;
            Ok(Matter::Zip {
                trunk,
                branches,
                twist: (data >> 7) & 1 != 0,
                hot: LaneMask::new(((data >> 8) & 0b11) as u8).unwrap(),
                cooldown: U3::new(((data >> 10) & 0b111) as u8).unwrap(),
                pull: [
                    Pull::from_code(((data >> 13) & 0b11) as u8).unwrap(),
                    Pull::from_code(((data >> 15) & 0b11) as u8).unwrap(),
                ],
            })
        }
        4 => {
            if data >> 15 != 0 { return Err(PackError::InvalidMatterWord); }
            Ok(Matter::Cross {
                routes: decode_cross_geometry((data & 0x3f) as u8)
                    .ok_or(PackError::InvalidMatterWord)?,
                hot: LaneMask::new(((data >> 6) & 0b11) as u8).unwrap(),
                cooldown: U3::new(((data >> 8) & 0b111) as u8).unwrap(),
                pull: [
                    Pull::from_code(((data >> 11) & 0b11) as u8).unwrap(),
                    Pull::from_code(((data >> 13) & 0b11) as u8).unwrap(),
                ],
            })
        }
        _ => Err(PackError::InvalidMatterWord),
    }
}

fn bool_bit(value: bool, shift: u32) -> u64 { (value as u64) << shift }

fn pack_control(control: Control) -> Result<u64, PackError> {
    let (kind, data): (u64, u64) = match control {
        Control::Idle => (0, 0),
        Control::Translate { role, phase, toward, exit, old_tail, epoch } => {
            let data = role as u64
                | ((phase as u64) << 2)
                | ((toward.code() as u64) << 5)
                | ((exit.code() as u64) << 8)
                | ((old_tail.code() as u64) << 11)
                | bool_bit(epoch, 14);
            (1, data)
        }
        Control::CableShift { role, phase, mode, axis, side, epoch } => {
            if axis == side { return Err(PackError::InvalidControlGeometry); }
            let data = role as u64
                | ((phase as u64) << 2)
                | ((mode as u64) << 5)
                | ((axis.code() as u64) << 6)
                | ((side.code() as u64) << 9)
                | bool_bit(epoch, 12);
            (2, data)
        }
        Control::PressureRelief { role, phase, axis, side, hot, epoch } => {
            if axis == side || axis.opp() == side { return Err(PackError::InvalidControlGeometry); }
            let data = role as u64
                | ((phase as u64) << 2)
                | ((axis.code() as u64) << 5)
                | ((side.code() as u64) << 8)
                | ((hot.get() as u64) << 11)
                | bool_bit(epoch, 13);
            (3, data)
        }
        Control::Rewrite { role, phase, rule, axis, side, lift, slot, epoch } => {
            if side >= 4 { return Err(PackError::InvalidControlGeometry); }
            let data = role as u64
                | ((phase as u64) << 3)
                | ((rule.get() as u64) << 6)
                | ((axis.code() as u64) << 11)
                | ((side as u64) << 14)
                | ((slot.get() as u64) << 16)
                | bool_bit(epoch, 22)
                | bool_bit(lift, 23);
            (4, data)
        }
        Control::Contest { source, stable, epoch } => {
            let data = source.code() as u64
                | ((stable.get() as u64) << 3)
                | bool_bit(epoch, 7);
            (5, data)
        }
    };
    Ok(kind | (data << 3))
}

fn decode_enum<T>(code: u8, values: &[T]) -> Option<T> where T: Copy {
    values.get(code as usize).copied()
}

fn unpack_control(word: u64) -> Result<Control, PackError> {
    let kind = word & 0b111;
    let data = word >> 3;
    let dir = |shift| Dir::from_code(((data >> shift) & 0b111u64) as u8)
        .ok_or(PackError::InvalidControlWord);
    let phase = |shift| Phase::from_code(((data >> shift) & 0b111u64) as u8)
        .ok_or(PackError::InvalidControlWord);
    match kind {
        0 if data == 0 => Ok(Control::Idle),
        0 => Err(PackError::InvalidControlWord),
        1 => {
            if data >> 15 != 0 { return Err(PackError::InvalidControlWord); }
            Ok(Control::Translate {
                role: decode_enum((data & 0b11) as u8, &[LineRole::Source, LineRole::Target, LineRole::Tail])
                    .ok_or(PackError::InvalidControlWord)?,
                phase: phase(2)?, toward: dir(5)?, exit: dir(8)?, old_tail: dir(11)?,
                epoch: (data >> 14) & 1 != 0,
            })
        }
        2 => {
            if data >> 13 != 0 { return Err(PackError::InvalidControlWord); }
            let axis = dir(6)?;
            let side = dir(9)?;
            if axis == side { return Err(PackError::InvalidControlWord); }
            Ok(Control::CableShift {
                role: decode_enum((data & 0b11) as u8, &[CableRole::Driver, CableRole::Trunk, CableRole::Branch])
                    .ok_or(PackError::InvalidControlWord)?,
                phase: phase(2)?,
                mode: if (data >> 5) & 1 == 0 { ZipMode::Zip } else { ZipMode::Unzip },
                axis, side, epoch: (data >> 12) & 1 != 0,
            })
        }
        3 => {
            if data >> 14 != 0 { return Err(PackError::InvalidControlWord); }
            let axis = dir(5)?;
            let side = dir(8)?;
            if axis == side || axis.opp() == side { return Err(PackError::InvalidControlWord); }
            Ok(Control::PressureRelief {
                role: decode_enum((data & 0b11) as u8, &[
                    ReliefRole::Driver, ReliefRole::Spine, ReliefRole::Corner, ReliefRole::Endpoint,
                ]).ok_or(PackError::InvalidControlWord)?,
                phase: phase(2)?, axis, side,
                hot: LaneMask::new(((data >> 11) & 0b11) as u8).unwrap(),
                epoch: (data >> 13) & 1 != 0,
            })
        }
        4 => {
            if data >> 24 != 0 { return Err(PackError::InvalidControlWord); }
            let side = ((data >> 14) & 0b11) as u8;
            Ok(Control::Rewrite {
                role: decode_enum((data & 0b111) as u8, &[
                    RewriteRole::Driver, RewriteRole::Boundary, RewriteRole::Probe,
                    RewriteRole::Seat, RewriteRole::Wire, RewriteRole::Cleanup,
                    RewriteRole::AbortRelay,
                ]).ok_or(PackError::InvalidControlWord)?,
                phase: phase(3)?,
                rule: RuleId::new(((data >> 6) & 0x1f) as u8).ok_or(PackError::InvalidControlWord)?,
                axis: dir(11)?, side,
                lift: (data >> 23) & 1 != 0,
                slot: U6::new(((data >> 16) & 0x3f) as u8).unwrap(),
                epoch: (data >> 22) & 1 != 0,
            })
        }
        5 => {
            if data >> 8 != 0 { return Err(PackError::InvalidControlWord); }
            Ok(Control::Contest {
                source: dir(0)?,
                stable: U4::new(((data >> 3) & 0xf) as u8).unwrap(),
                epoch: (data >> 7) & 1 != 0,
            })
        }
        _ => Err(PackError::InvalidControlWord),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn roundtrip(matter: Matter, control: Control) {
        let decoded = DecodedCell { matter, control, chi: 0xa5, sigma: 0x5a };
        let word = CellWord::pack(decoded).unwrap();
        assert_eq!(word.unpack().unwrap(), decoded);
    }

    #[test]
    fn cell_word_is_exactly_64_bits() {
        assert_eq!(TOTAL_BITS, 64);
        assert_eq!(core::mem::size_of::<CellWord>(), 8);
    }

    #[test]
    fn every_agent_geometry_roundtrips() {
        for tag in crate::rules::ALL_TAGS {
            for principal in DIRS {
                if tag.arity() == 1 {
                    roundtrip(Matter::Agent { tag, principal, tail: None, aux_flip: false }, Control::Idle);
                } else {
                    for tail in DIRS.into_iter().filter(|d| *d != principal) {
                        for aux_flip in [false, true] {
                            if tag.arity() != 3 && aux_flip { continue; }
                            roundtrip(Matter::Agent { tag, principal, tail: Some(tail), aux_flip }, Control::Idle);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn every_link_and_zip_geometry_roundtrips() {
        for a in DIRS {
            for b in DIRS.into_iter().filter(|b| *b != a) {
                if a < b {
                    for lanes in [LaneCount::One, LaneCount::Two] {
                        for twist in [false, true] {
                            if lanes == LaneCount::One && twist { continue; }
                            let hot = LaneMask::new(if lanes == LaneCount::One { 1 } else { 3 }).unwrap();
                            roundtrip(Matter::Link {
                                ends: FacePair::new(a, b).unwrap(), lanes, twist, hot,
                                cooldown: U3::new(7).unwrap(), pull: [Pull::Request, Pull::Ack],
                            }, Control::Idle);
                        }
                    }
                }
                for c in DIRS.into_iter().filter(|c| *c != a && *c != b) {
                    roundtrip(Matter::Zip {
                        trunk: a, branches: [b, c], twist: true,
                        hot: LaneMask::new(3).unwrap(), cooldown: U3::new(7).unwrap(),
                        pull: [Pull::Hold, Pull::Ack],
                    }, Control::Idle);
                }
            }
        }
    }

    #[test]
    fn every_cross_geometry_roundtrips() {
        let mut count = 0;
        for a in 0..15 {
            for b in a + 1..15 {
                let Some(first) = FacePair::from_code(a) else { continue };
                let Some(second) = FacePair::from_code(b) else { continue };
                if encode_cross_geometry([first, second]).is_none() { continue; }
                roundtrip(Matter::Cross {
                    routes: [first, second], hot: LaneMask::new(3).unwrap(),
                    cooldown: U3::new(7).unwrap(), pull: [Pull::Request, Pull::Hold],
                }, Control::Idle);
                count += 1;
            }
        }
        assert_eq!(count, 45);
    }

    #[test]
    fn every_rewrite_address_roundtrips() {
        for rule in 0..26 {
            for axis in DIRS {
                for side in 0..4 {
                    for slot in 0..64 {
                        roundtrip(Matter::Empty, Control::Rewrite {
                            role: RewriteRole::Seat,
                            phase: Phase::Ack,
                            rule: RuleId::new(rule).unwrap(),
                            axis,
                            side,
                            lift: slot & 2 != 0,
                            slot: U6::new(slot).unwrap(),
                            epoch: slot & 1 != 0,
                        });
                    }
                }
            }
        }
    }

    #[test]
    fn representative_protocols_roundtrip() {
        roundtrip(Matter::Empty, Control::Translate {
            role: LineRole::Target, phase: Phase::Commit,
            toward: Dir::E, exit: Dir::N, old_tail: Dir::W, epoch: true,
        });
        roundtrip(Matter::Empty, Control::CableShift {
            role: CableRole::Branch, phase: Phase::Done, mode: ZipMode::Unzip,
            axis: Dir::N, side: Dir::U, epoch: false,
        });
        roundtrip(Matter::Empty, Control::PressureRelief {
            role: ReliefRole::Corner, phase: Phase::Offer,
            axis: Dir::E, side: Dir::U, hot: LaneMask::new(2).unwrap(), epoch: true,
        });
        roundtrip(Matter::Empty, Control::Contest {
            source: Dir::D, stable: U4::new(15).unwrap(), epoch: true,
        });
    }

    #[test]
    fn invalid_states_are_rejected() {
        assert_eq!(CellWord::pack(DecodedCell {
            matter: Matter::Agent { tag: Tag::L, principal: Dir::E, tail: Some(Dir::W), aux_flip: false },
            ..DecodedCell::default()
        }), Err(PackError::InvalidAgentGeometry));
        assert_eq!(CellWord::pack(DecodedCell {
            matter: Matter::Link {
                ends: FacePair::new(Dir::E, Dir::W).unwrap(), lanes: LaneCount::One,
                twist: true, hot: LaneMask::new(1).unwrap(), cooldown: U3::default(),
                pull: [Pull::None; 2],
            },
            ..DecodedCell::default()
        }), Err(PackError::InvalidLinkSignals));
        assert_eq!(CellWord::pack(DecodedCell {
            matter: Matter::Zip {
                trunk: Dir::E, branches: [Dir::W, Dir::E], twist: false,
                hot: LaneMask::default(), cooldown: U3::default(), pull: [Pull::None; 2],
            },
            ..DecodedCell::default()
        }), Err(PackError::InvalidZipperGeometry));
    }
}
