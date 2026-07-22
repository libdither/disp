//! Lattice geometry shared by the cascade substrate: positions, the six face directions,
//! and the topology (Bilayer restricts z to {0, 1}; Full3D is unbounded, and crossings
//! route around in depth).

pub type Pos = (i32, i32, i32);

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

pub fn step(p: Pos, d: Dir) -> Pos { let (dx, dy, dz) = d.delta(); (p.0 + dx, p.1 + dy, p.2 + dz) }
pub fn dir_to(a: Pos, b: Pos) -> Option<Dir> { DIRS.into_iter().find(|d| step(a, *d) == b) }
pub fn manhattan(a: Pos, b: Pos) -> i32 { (a.0 - b.0).abs() + (a.1 - b.1).abs() + (a.2 - b.2).abs() }

/// The lattice topology. Dynamics are identical across topologies; only which cells exist
/// differs.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Topo { Bilayer, Full3D }
pub const TOPOS: [Topo; 2] = [Topo::Bilayer, Topo::Full3D];

impl Topo {
    pub fn in_bounds(self, p: Pos) -> bool {
        match self { Topo::Bilayer => p.2 == 0 || p.2 == 1, Topo::Full3D => true }
    }
    pub fn name(self) -> &'static str {
        match self { Topo::Bilayer => "bilayer", Topo::Full3D => "full3d" }
    }
}
