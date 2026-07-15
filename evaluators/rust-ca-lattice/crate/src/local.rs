//! Strict radius-one geometry protocols.
//!
//! The cellular geometry rule has the literal shape
//!
//! `next_site(center, [N, E, S, W, U, D]) -> next(center)`.
//!
//! It receives no `Grid`, coordinate, clock, route, id, or mutable neighbor.  The sparse
//! runner at the bottom merely gathers seven-site snapshots, evaluates every site from
//! the same frozen state, and materializes the returned center edits simultaneously.
//! Loader, projection, event collection, and sparse enumeration remain host observers.

use crate::lattice::{step, AgentCell, Cell, Dir, Grid, Pos, Strand, WireCell, DIRS, WIRE_CAP};
use crate::rules::Tag;
use std::collections::BTreeSet;

const PROTOCOL_TTL: u8 = 24;
const SHARED_CONTEST_TICKS: u8 = 64;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LocalStrand {
    pub a: Dir,
    pub b: Dir,
    pub hot: bool,
    pub cooldown: u8,
}

impl LocalStrand {
    fn contains(self, d: Dir) -> bool { self.a == d || self.b == d }
    fn other(self, d: Dir) -> Dir { if self.a == d { self.b } else { self.a } }
    fn is_pair(self, a: Dir, b: Dir) -> bool {
        (self.a == a && self.b == b) || (self.a == b && self.b == a)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LocalWire {
    pub strands: [Option<LocalStrand>; WIRE_CAP],
}

impl LocalWire {
    fn count(self) -> usize { self.strands.iter().flatten().count() }
    fn only(self) -> Option<LocalStrand> {
        if self.count() == 1 { self.strands.into_iter().flatten().next() } else { None }
    }
    fn with_face(self, d: Dir) -> Option<LocalStrand> {
        self.strands.into_iter().flatten().find(|s| s.contains(d))
    }
    fn face_used(self, d: Dir) -> bool { self.with_face(d).is_some() }
    fn has_pair(self, a: Dir, b: Dir) -> bool {
        self.strands.into_iter().flatten().any(|s| s.is_pair(a, b))
    }
}

/// The finite geometry projection visible to the local rule.  In particular, observer
/// ids and the current host-only `Rc<GrowPlan>` never cross this API boundary.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PayloadView {
    Empty,
    Agent { tag: Tag, faces: [Option<Dir>; 3], nascent: bool },
    Wire(LocalWire),
    /// A growing seed is opaque and immobile to the local rule.
    Blocked,
}

impl PayloadView {
    fn agent(self) -> Option<(Tag, [Option<Dir>; 3], bool)> {
        match self { PayloadView::Agent { tag, faces, nascent } => Some((tag, faces, nascent)), _ => None }
    }
    fn wire(self) -> Option<LocalWire> {
        match self { PayloadView::Wire(w) => Some(w), _ => None }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct StarSpec {
    /// Relative geometry latched when the source publishes its Claim.
    /// Source-to-target direction.
    pub toward: Dir,
    /// Principal face of the agent after it occupies the target wire cell.
    pub exit: Dir,
    /// The auxiliary routed through the vacated source cell, if any.
    pub back_port: Option<u8>,
    pub back_face: Option<Dir>,
    /// A second auxiliary routed through the swept square, if any.
    pub square_port: Option<u8>,
    pub square_face: Option<Dir>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SquareMode { Extend, Truncate }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct BulgeSpec {
    /// One direction along the original straight strand; the other is `axis.opp()`.
    pub axis: Dir,
    /// Source-to-spine direction, perpendicular to the axis.
    pub side: Dir,
    pub hot: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SourcePhase { Claim, Armed, Countdown(u8) }
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TargetPhase { Claim, Ready, Countdown(u8) }
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SquarePhase { Claim, Ready, Countdown(u8) }
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum EndpointPhase { Ready, Countdown(u8) }
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BackPhase { Ready, Countdown(u8) }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct DebtState {
    pub back: Option<Dir>,
    pub square: Option<Dir>,
}

/// The portion of a site that can change the geometry of a cold contention. Fields and
/// locking roles are checked separately by `cold_shared_contest`; nonlocking role age is
/// deliberately excluded so the persistence counter does not fingerprint itself.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ContestSite {
    pub in_bounds: bool,
    pub payload: PayloadView,
    pub reserved: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ContestSignature {
    pub sites: [ContestSite; 7],
}

/// Per-site protocol state.  Every direction is relative and every counter saturates;
/// there are no transaction ids or absolute owner coordinates.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MotionMark {
    Source { spec: StarSpec, phase: SourcePhase, age: u8 },
    Target { spec: StarSpec, phase: TargetPhase, age: u8 },
    Square {
        spec: StarSpec,
        mode: SquareMode,
        phase: SquarePhase,
        age: u8,
        resume: Option<DebtState>,
    },
    Endpoint {
        spec: StarSpec,
        mode: SquareMode,
        phase: EndpointPhase,
        age: u8,
        resume: Option<DebtState>,
    },
    /// The unchanged far endpoint of the back bend participates so it cannot start an
    /// overlapping move while the source prepares to leave a strand pointing at it.
    Back {
        spec: StarSpec,
        phase: BackPhase,
        age: u8,
        resume: Option<DebtState>,
    },
    BulgeSource { spec: BulgeSpec, phase: SourcePhase, age: u8, retried: bool },
    BulgeSpine { spec: BulgeSpec, phase: TargetPhase, age: u8 },
    BulgeCorner { spec: BulgeSpec, plus: bool, phase: SquarePhase, age: u8 },
    BulgeEndpoint {
        spec: BulgeSpec,
        plus: bool,
        phase: EndpointPhase,
        age: u8,
        resume: Option<DebtState>,
    },
    /// A neutral degree-three step may not immediately run again while either freshly
    /// inserted aux bend is still adjacent.  This is a local convoy/ratchet condition,
    /// not a reservation, so host-planned demanded reel may still pass through it.
    Debt { back: Option<Dir>, square: Option<Dir> },
    /// A calm shared principal target must retain one exact local geometry before it emits χ.
    /// This nonlocking persistence mark prevents a transient crossing from launching a
    /// pressure response during active transport.
    SharedContest { source: Dir, signature: ContestSignature, age: u8 },
}

impl MotionMark {
    pub fn is_locking(self) -> bool {
        !matches!(self, MotionMark::Debt { .. } | MotionMark::SharedContest { .. })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SiteView {
    pub in_bounds: bool,
    pub payload: PayloadView,
    pub motion: Option<MotionMark>,
    pub reserved: bool,
    pub chi: u8,
    pub sigma: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Hood {
    pub center: SiteView,
    pub neighbors: [SiteView; 6],
}

impl Hood {
    pub fn neighbor(&self, d: Dir) -> SiteView { self.neighbors[d as usize] }
}

/// A center-owned geometry edit. The rule emits only these finite commands. Materialization
/// copies an agent's observer sid; a changed strand keeps heat/cooldown but invalidates its
/// direction-dependent tension survey so the next survey sweep can rebuild it.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PayloadEdit {
    Keep,
    Clear,
    WriteSingleStrand { a: Dir, b: Dir, hot: bool },
    MoveAgentHere { source: Dir, faces: [Option<Dir>; 3] },
    Repoint { from: Dir, to: Dir },
    RemoveStrand { a: Dir, b: Dir },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SiteNext {
    pub motion: Option<MotionMark>,
    pub edit: PayloadEdit,
    /// Center-emitted frustration source derived from this same frozen neighborhood.
    pub chi_source: u8,
    /// Observer flag: exactly the target role sets it on a successful star commit.
    pub target_commit: bool,
    /// Observer flag set by the source cell when a pressure bulge commits.
    pub bulge_commit: bool,
}

impl SiteNext {
    fn keep(motion: Option<MotionMark>) -> SiteNext {
        SiteNext {
            motion,
            edit: PayloadEdit::Keep,
            chi_source: 0,
            target_commit: false,
            bulge_commit: false,
        }
    }
    fn edit(motion: Option<MotionMark>, edit: PayloadEdit) -> SiteNext {
        SiteNext { motion, edit, chi_source: 0, target_commit: false, bulge_commit: false }
    }
}

fn local_strand(s: Strand) -> LocalStrand {
    LocalStrand { a: s.a, b: s.b, hot: s.hot, cooldown: s.cooldown }
}

fn payload_view(cell: Option<&Cell>) -> PayloadView {
    match cell {
        None => PayloadView::Empty,
        Some(Cell::Agent(a)) => PayloadView::Agent { tag: a.tag, faces: a.faces, nascent: a.nascent },
        Some(Cell::Wire(w)) => {
            let mut strands = [None; WIRE_CAP];
            for (dst, src) in strands.iter_mut().zip(w.strands.iter()) {
                *dst = src.map(local_strand);
            }
            PayloadView::Wire(LocalWire { strands })
        }
        Some(Cell::Seed(_)) => PayloadView::Blocked,
    }
}

pub fn site_view_at(grid: &Grid, p: Pos) -> SiteView {
    let in_bounds = grid.topo.in_bounds(p);
    SiteView {
        in_bounds,
        payload: if in_bounds { payload_view(grid.cells.get(&p)) } else { PayloadView::Blocked },
        motion: grid.motion.get(&p).copied(),
        reserved: grid.reserved.contains_key(&p),
        chi: grid.chi_at(p),
        sigma: grid.sigma_at(p),
    }
}

pub fn hood_at(grid: &Grid, p: Pos) -> Hood {
    Hood { center: site_view_at(grid, p), neighbors: DIRS.map(|d| site_view_at(grid, step(p, d))) }
}

fn attachment_can_repoint(site: SiteView, from: Dir, to: Dir) -> bool {
    if !site.in_bounds || site.reserved || site.motion.is_some() { return false; }
    match site.payload {
        PayloadView::Agent { tag, faces, nascent } => {
            !nascent && (0..tag.arity()).any(|i| faces[i] == Some(from))
                && !(0..tag.arity()).any(|i| faces[i] == Some(to))
        }
        PayloadView::Wire(w) => w.with_face(from).is_some() && !w.face_used(to),
        _ => false,
    }
}

fn attachment_hot(site: SiteView, face: Dir) -> bool {
    match site.payload {
        PayloadView::Wire(w) => w.with_face(face).is_some_and(|s| s.hot),
        PayloadView::Agent { tag, faces, nascent: false } => tag.is_consumer() && faces[0] == Some(face),
        _ => false,
    }
}

fn exact_turn(site: SiteView, a: Dir, b: Dir) -> bool {
    site.payload.wire().is_some_and(|w| w.has_pair(a, b))
}

/// Choose the canonical local star geometry.  A degree-two producer always uses the
/// vacated cell.  A degree-three producer uses that back route for one aux and a swept
/// square for the other.  A square aux whose face would collide with the new principal,
/// or whose corner would be the source itself, must be the back aux.
fn source_spec(h: &Hood) -> Option<StarSpec> {
    let (tag, faces, nascent) = h.center.payload.agent()?;
    if nascent || !tag.is_producer() { return None; }
    let d = faces[0]?;
    let target = h.neighbor(d);
    if !target.in_bounds || target.reserved { return None; }
    let principal = target.payload.wire()?.only()?;
    if !principal.contains(d.opp()) { return None; }
    let exit = principal.other(d.opp());

    let arity = tag.arity();
    if arity == 1 {
        return Some(StarSpec { toward: d, exit, back_port: None, back_face: None, square_port: None, square_face: None });
    }
    if arity == 2 {
        return Some(StarSpec { toward: d, exit, back_port: Some(1), back_face: faces[1], square_port: None, square_face: None });
    }

    let f1 = faces[1].expect("live aux face");
    let f2 = faces[2].expect("live aux face");
    // The source cannot see either swept-square cell, but in calm space it can reject
    // an auxiliary whose adjacent endpoint could not repoint toward that square even
    // if the diagonal were empty. Otherwise a settled net can choose an impossible
    // square, time out, and immediately choose it again while a feasible alternate aux
    // sits beside it. Ignore nonlocking marks while assessing physical capability; the
    // claim protocol performs the actual arbitration and lends Debt where necessary.
    let square_capable = |f: Dir| {
        if f == exit || f == d.opp() { return false; }
        let endpoint = SiteView { motion: None, ..h.neighbor(f) };
        exact_turn(endpoint, f.opp(), d)
            || attachment_can_repoint(endpoint, f.opp(), d)
    };
    let calm_cold = h.center.chi == 0 && h.center.sigma == 0
        && target.chi == 0 && target.sigma == 0 && !principal.hot;
    let square = if calm_cold {
        let c1 = square_capable(f1);
        let c2 = square_capable(f2);
        match (c1, c2) {
            (false, false) => return None,
            (true, false) => 1,
            (false, true) => 2,
            // Use a deterministic face/port preference when either aux can be swept;
            // no coordinate, id, or remote state breaks the tie.
            (true, true) => 2,
        }
    } else {
        // Under demand or pressure use the canonical port preference. A failed
        // diagonal square remains a pressure/wait signal rather than letting urgent
        // transport repeatedly pay +1 extension into an already crowded region.
        let r1 = f1 == exit || f1 == d.opp();
        let r2 = f2 == exit || f2 == d.opp();
        if r1 && r2 { return None; }
        if r2 { 1 } else { 2 }
    };
    let back = if square == 1 { 2 } else { 1 };
    let f = faces[square]?;
    Some(StarSpec {
        toward: d,
        exit,
        back_port: Some(back as u8),
        back_face: faces[back],
        square_port: Some(square as u8),
        square_face: Some(f),
    })
}

/// Validate the physical star that an earlier Claim selected. Licensing fields and the
/// auxiliary tie-break are deliberately absent: they may change while the participants
/// are locked, but cannot alter the relative faces already named by `spec`.
fn source_geometry_matches(h: &Hood, spec: StarSpec) -> bool {
    let Some((tag, faces, false)) = h.center.payload.agent() else { return false };
    if !tag.is_producer() || faces[0] != Some(spec.toward)
        || !target_wire_matches(h.neighbor(spec.toward), spec) {
        return false;
    }
    match tag.arity() {
        1 => spec.back_port.is_none() && spec.back_face.is_none()
            && spec.square_port.is_none() && spec.square_face.is_none(),
        2 => spec.back_port == Some(1) && spec.back_face.is_some()
            && spec.back_face == faces[1]
            && spec.square_port.is_none() && spec.square_face.is_none(),
        3 => {
            let (Some(back), Some(back_face), Some(square), Some(square_face)) =
                (spec.back_port, spec.back_face, spec.square_port, spec.square_face)
            else { return false };
            back != square
                && matches!(back, 1 | 2)
                && matches!(square, 1 | 2)
                && faces[back as usize] == Some(back_face)
                && faces[square as usize] == Some(square_face)
                && square_face != spec.exit
                && square_face != spec.toward.opp()
        }
        _ => false,
    }
}

fn source_offer_spec(h: &Hood) -> Option<StarSpec> {
    let spec = source_spec(h)?;
    let target = h.neighbor(spec.toward);
    if target.motion.is_some() { return None; }
    let principal = target.payload.wire()?.only()?;
    let calm_cold = h.center.chi == 0 && h.center.sigma == 0
        && target.chi == 0 && target.sigma == 0 && !principal.hot;
    // ψ is a one-hop-local license: demanded transport may use the same safe star
    // handshake inside field halos.
    let demanded = principal.hot;
    // Pressure does not move a foreign occupant.  It authorizes this agent to consume
    // one element of its own exclusive principal wire when that step descends χ.
    let pressure_licensed = h.center.chi >= 4
        && target.chi.saturating_add(2) <= h.center.chi;
    (calm_cold || demanded || pressure_licensed).then_some(spec)
}

fn target_wire_matches(site: SiteView, spec: StarSpec) -> bool {
    site.payload.wire().and_then(LocalWire::only).is_some_and(|s| {
        s.contains(spec.toward.opp()) && s.other(spec.toward.opp()) == spec.exit
    })
}

fn square_geometry_mode(h: &Hood, spec: StarSpec) -> Option<SquareMode> {
    let f = spec.square_face?;
    let endpoint = h.neighbor(spec.toward.opp());
    if !h.center.in_bounds || h.center.reserved
        || !endpoint.in_bounds || endpoint.reserved { return None; }

    // p--u--c already contains the needed corner: consume u and point c at q.
    let center_unmarked = SiteView { motion: None, ..h.center };
    let endpoint_unmarked = SiteView { motion: None, ..endpoint };
    if exact_turn(endpoint, f.opp(), spec.toward)
        && attachment_can_repoint(center_unmarked, spec.toward.opp(), f.opp()) {
        return Some(SquareMode::Truncate);
    }
    // Otherwise add the one swept-square strand and repoint u from p to c.
    if h.center.payload == PayloadView::Empty
        && attachment_can_repoint(endpoint_unmarked, f.opp(), spec.toward) {
        return Some(SquareMode::Extend);
    }
    None
}

fn square_mode(h: &Hood, spec: StarSpec) -> Option<SquareMode> {
    let endpoint = h.neighbor(spec.toward.opp());
    if h.center.motion.is_some_and(MotionMark::is_locking)
        || endpoint.motion.is_some_and(MotionMark::is_locking) { return None; }
    square_geometry_mode(h, spec)
}

fn endpoint_valid(site: SiteView, spec: StarSpec, mode: SquareMode) -> bool {
    let Some(f) = spec.square_face else { return false };
    if site.reserved { return false; }
    match mode {
        SquareMode::Extend => attachment_can_repoint(
            SiteView { motion: None, ..site }, f.opp(), spec.toward),
        SquareMode::Truncate => exact_turn(site, f.opp(), spec.toward),
    }
}

fn reciprocal_source(site: SiteView, spec: StarSpec) -> Option<SourcePhase> {
    match site.motion {
        Some(MotionMark::Source { spec: s, phase, .. }) if s == spec => Some(phase),
        _ => None,
    }
}
fn reciprocal_target(site: SiteView, spec: StarSpec) -> Option<TargetPhase> {
    match site.motion {
        Some(MotionMark::Target { spec: s, phase, .. }) if s == spec => Some(phase),
        _ => None,
    }
}
fn reciprocal_square(site: SiteView, spec: StarSpec, mode: SquareMode) -> Option<SquarePhase> {
    match site.motion {
        Some(MotionMark::Square { spec: s, mode: m, phase, .. }) if s == spec && m == mode => Some(phase),
        _ => None,
    }
}
fn reciprocal_endpoint(site: SiteView, spec: StarSpec, mode: SquareMode) -> Option<EndpointPhase> {
    match site.motion {
        Some(MotionMark::Endpoint { spec: s, mode: m, phase, .. }) if s == spec && m == mode => Some(phase),
        _ => None,
    }
}
fn reciprocal_back(site: SiteView, spec: StarSpec) -> Option<BackPhase> {
    match site.motion {
        Some(MotionMark::Back { spec: s, phase, .. }) if s == spec => Some(phase),
        _ => None,
    }
}

fn back_valid(site: SiteView, spec: StarSpec) -> bool {
    let Some(f) = spec.back_face else { return spec.back_port.is_none() };
    if !site.in_bounds || site.reserved { return false; }
    match site.payload {
        PayloadView::Agent { tag, faces, nascent } => {
            !nascent && (0..tag.arity()).any(|i| faces[i] == Some(f.opp()))
        }
        PayloadView::Wire(w) => w.with_face(f.opp()).is_some(),
        _ => false,
    }
}

fn next_age(age: u8) -> Option<u8> {
    let age = age.saturating_add(1);
    (age <= PROTOCOL_TTL).then_some(age)
}

/// Claims may time out while they are waiting to assemble a footprint.  Once the
/// fixed-length countdown has begun, however, age is no longer an abort condition:
/// roles are recruited on different ticks, so expiring one participant during the
/// commit wave can otherwise leave a more recently recruited remote role writing alone.
fn protocol_age(age: u8, committing: bool) -> Option<u8> {
    if committing { Some(age.saturating_add(1)) } else { next_age(age) }
}

fn bulge_strand(site: SiteView, spec: BulgeSpec) -> Option<LocalStrand> {
    site.payload.wire()?.strands.into_iter().flatten().find(|s| {
        s.is_pair(spec.axis, spec.axis.opp()) && s.hot == spec.hot
    })
}

fn bulge_source_valid(h: &Hood, spec: BulgeSpec) -> bool {
    if bulge_strand(h.center, spec).is_none() { return false; }
    let spine = h.neighbor(spec.side);
    spine.in_bounds && !spine.reserved && spine.payload == PayloadView::Empty
}

/// A straight strand has no corner for χ-flip to move.  Under a strong gradient its own
/// cell may pay two units of elastic length to create the canonical width-one detour.
fn bulge_offer(h: &Hood) -> Option<BulgeSpec> {
    if h.center.chi < 4 { return None; }
    let wire = h.center.payload.wire()?;
    // Shared switchboxes may yield under an ordinary strong gradient.  A count-one
    // strand may yield only at saturated pressure: this is the locally relayed signal
    // that the cell itself is the hidden corner/endpoint obstruction of another bulge.
    // That distinction prevents every straight cell in a broad χ halo from growing by
    // +2 at once.
    if wire.count() < 2 && h.center.chi < 200 { return None; }
    // Prefer clearing cold plumbing before a hot demanded bond in the same switchbox.
    for want_hot in [false, true] {
        for strand in wire.strands.into_iter().flatten() {
            if strand.hot != want_hot || strand.cooldown > 0 || strand.b != strand.a.opp() { continue; }
            let axis = if (strand.a as usize) < (strand.b as usize) { strand.a } else { strand.b };
            let mut best: Option<(u8, Dir)> = None;
            for side in axis.perp() {
                let q = h.neighbor(side);
                if !q.in_bounds || q.reserved || q.motion.is_some() || q.payload != PayloadView::Empty { continue; }
                if q.chi.saturating_add(2) > h.center.chi { continue; }
                if best.is_none_or(|(chi, _)| q.chi < chi) { best = Some((q.chi, side)); }
            }
            if let Some((_, side)) = best { return Some(BulgeSpec { axis, side, hot: strand.hot }); }
        }
    }
    None
}

/// A source cannot see the two diagonal corner cells of its proposed bulge.  If their
/// relayed claims never assemble, try a different locally admissible straight
/// strand/side instead of deterministically choosing the same hidden obstruction
/// forever. A matching spine mark from the failed claim is transiently reusable: after
/// the source changes spec it releases itself before the new claim can arrive.
fn bulge_retry_offer(h: &Hood, failed: BulgeSpec) -> Option<BulgeSpec> {
    if h.center.chi < 4 { return None; }
    let wire = h.center.payload.wire()?;
    if wire.count() < 2 && h.center.chi < 200 { return None; }
    for want_hot in [false, true] {
        for strand in wire.strands.into_iter().flatten() {
            if strand.hot != want_hot || strand.cooldown > 0 || strand.b != strand.a.opp() {
                continue;
            }
            let axis = if (strand.a as usize) < (strand.b as usize) { strand.a } else { strand.b };
            let mut best: Option<(u8, Dir)> = None;
            for side in axis.perp() {
                let spec = BulgeSpec { axis, side, hot: strand.hot };
                if spec == failed { continue; }
                let q = h.neighbor(side);
                let reusable_spine = matches!(q.motion,
                    Some(MotionMark::BulgeSpine { spec, .. }) if spec == failed);
                if !q.in_bounds || q.reserved || (!reusable_spine && q.motion.is_some())
                    || q.payload != PayloadView::Empty { continue; }
                if q.chi.saturating_add(2) > h.center.chi { continue; }
                if best.is_none_or(|(chi, _)| q.chi < chi) { best = Some((q.chi, side)); }
            }
            if let Some((_, side)) = best {
                return Some(BulgeSpec { axis, side, hot: strand.hot });
            }
        }
    }
    None
}

fn bulge_endpoint_from(spec: BulgeSpec, plus: bool) -> Dir {
    if plus { spec.axis.opp() } else { spec.axis }
}

fn bulge_endpoint_valid(site: SiteView, spec: BulgeSpec, plus: bool) -> bool {
    attachment_can_repoint(
        SiteView { motion: None, ..site },
        bulge_endpoint_from(spec, plus),
        spec.side,
    )
}

fn contest_site(site: SiteView) -> ContestSite {
    ContestSite {
        in_bounds: site.in_bounds,
        payload: site.payload,
        reserved: site.reserved,
    }
}

fn contest_signature(h: &Hood) -> ContestSignature {
    let mut sites = [contest_site(h.center); 7];
    for (slot, site) in sites[1..].iter_mut().zip(h.neighbors) {
        *slot = contest_site(site);
    }
    ContestSignature { sites }
}

/// A shared target becomes a clearance request only after this entire seven-site patch is
/// calm: no field, no locking protocol role, and no hot strand. The returned direction
/// points from the switchbox to the adjacent producer.
fn cold_shared_contest(h: &Hood) -> Option<(Dir, ContestSignature)> {
    if h.center.chi != 0 || h.center.sigma != 0 { return None; }
    let wire = h.center.payload.wire()?;
    if wire.count() <= 1 || wire.strands.into_iter().flatten().any(|strand| strand.hot) {
        return None;
    }
    if h.neighbors.iter().any(|site| {
        site.chi != 0 || site.sigma != 0
            || site.motion.is_some_and(MotionMark::is_locking)
            || site.payload.wire().is_some_and(|wire| {
                wire.strands.into_iter().flatten().any(|strand| strand.hot)
            })
    }) {
        return None;
    }
    let signature = contest_signature(h);
    for to_source in DIRS {
        let Some((tag, faces, false)) = h.neighbor(to_source).payload.agent() else {
            continue;
        };
        if tag.is_producer() && faces[0] == Some(to_source.opp())
            && wire.with_face(to_source).is_some()
        {
            return Some((to_source, signature));
        }
    }
    None
}

/// Local χ sources for contested strict-motion cells. A shared principal switchbox can
/// see the producer asking to enter through one of its strands, and a bulge corner can see
/// both its spine claim and endpoint. Each site raises pressure only at itself; it never
/// names or moves a foreign occupant.
fn local_pressure_source(h: &Hood) -> u8 {
    if !h.center.in_bounds { return 0; }

    if let Some(MotionMark::SharedContest { source, signature, age }) = h.center.motion {
        let unmarked = Hood {
            center: SiteView { motion: None, ..h.center },
            ..*h
        };
        if age >= SHARED_CONTEST_TICKS
            && cold_shared_contest(&unmarked) == Some((source, signature))
        {
            return 16;
        }
    }

    for to_spine in DIRS {
        let Some(MotionMark::BulgeSpine {
            spec, phase: TargetPhase::Claim, ..
        }) = h.neighbor(to_spine).motion else { continue };
        let plus = if to_spine == spec.axis.opp() {
            true
        } else if to_spine == spec.axis {
            false
        } else {
            continue;
        };
        let endpoint = h.neighbor(spec.side.opp());
        if h.center.payload != PayloadView::Empty
            || !bulge_endpoint_valid(endpoint, spec, plus)
        {
            return 250;
        }
    }
    0
}

fn reciprocal_bulge_source(site: SiteView, spec: BulgeSpec) -> Option<SourcePhase> {
    match site.motion {
        Some(MotionMark::BulgeSource { spec: s, phase, .. }) if s == spec => Some(phase),
        _ => None,
    }
}
fn reciprocal_bulge_spine(site: SiteView, spec: BulgeSpec) -> Option<TargetPhase> {
    match site.motion {
        Some(MotionMark::BulgeSpine { spec: s, phase, .. }) if s == spec => Some(phase),
        _ => None,
    }
}
fn reciprocal_bulge_corner(site: SiteView, spec: BulgeSpec, plus: bool) -> Option<SquarePhase> {
    match site.motion {
        Some(MotionMark::BulgeCorner { spec: s, plus: p, phase, .. }) if s == spec && p == plus => Some(phase),
        _ => None,
    }
}
fn reciprocal_bulge_endpoint(site: SiteView, spec: BulgeSpec, plus: bool) -> Option<EndpointPhase> {
    match site.motion {
        Some(MotionMark::BulgeEndpoint { spec: s, plus: p, phase, .. }) if s == spec && p == plus => Some(phase),
        _ => None,
    }
}

fn stable_next(h: &Hood) -> SiteNext {
    if !h.center.in_bounds { return SiteNext::keep(None); }

    // Reservations exclude arrivals, not departures. An incumbent producer or wire may
    // still start an owner-written move that can clear its cell; all incoming roles and
    // writes into reserved targets remain rejected by the role-specific checks below.
    if h.center.reserved {
        if let Some(spec) = bulge_offer(h) {
            return SiteNext::keep(Some(MotionMark::BulgeSource {
                spec, phase: SourcePhase::Claim, age: 0, retried: false,
            }));
        }
        if let Some(spec) = source_offer_spec(h) {
            return SiteNext::keep(Some(MotionMark::Source {
                spec, phase: SourcePhase::Claim, age: 0,
            }));
        }
        return SiteNext::keep(None);
    }

    // A claimed principal-wire cell accepts only the source actually attached through
    // that strand.  Face uniqueness makes this arbitration unique.
    for to_source in DIRS {
        let source = h.neighbor(to_source);
        let Some(MotionMark::Source { spec, phase: SourcePhase::Claim, .. }) = source.motion else { continue };
        if spec.toward != to_source.opp() || !target_wire_matches(h.center, spec) { continue; }
        let phase = if spec.square_port.is_none() { TargetPhase::Ready } else { TargetPhase::Claim };
        return SiteNext::keep(Some(MotionMark::Target { spec, phase, age: 0 }));
    }

    // The diagonal swept cell learns about the source only through its adjacent target.
    for to_target in DIRS {
        let target = h.neighbor(to_target);
        let Some(MotionMark::Target { spec, phase: TargetPhase::Claim, .. }) = target.motion else { continue };
        let Some(f) = spec.square_face else { continue };
        if to_target != f.opp() { continue; }
        if let Some(mode) = square_mode(h, spec) {
            return SiteNext::keep(Some(MotionMark::Square {
                spec, mode, phase: SquarePhase::Claim, age: 0, resume: None,
            }));
        }
    }

    // The source-side auxiliary endpoint locks itself after seeing the claimed square.
    // The moving source cannot mutate it; the endpoint's own role writes its payload.
    for to_square in DIRS {
        let square = h.neighbor(to_square);
        let Some(MotionMark::Square { spec, mode, phase: SquarePhase::Claim, .. }) = square.motion else { continue };
        if to_square != spec.toward || !endpoint_valid(h.center, spec, mode) { continue; }
        return SiteNext::keep(Some(MotionMark::Endpoint {
            spec, mode, phase: EndpointPhase::Ready, age: 0, resume: None,
        }));
    }

    for to_source in DIRS {
        let source = h.neighbor(to_source);
        let Some(MotionMark::Source { spec, phase: SourcePhase::Claim, .. }) = source.motion else { continue };
        let Some(f) = spec.back_face else { continue };
        if to_source != f.opp() || !back_valid(h.center, spec) { continue; }
        return SiteNext::keep(Some(MotionMark::Back {
            spec, phase: BackPhase::Ready, age: 0, resume: None,
        }));
    }

    for to_source in DIRS {
        let source = h.neighbor(to_source);
        let Some(MotionMark::BulgeSource { spec, phase: SourcePhase::Claim, .. }) = source.motion else { continue };
        if to_source != spec.side.opp() || h.center.payload != PayloadView::Empty { continue; }
        return SiteNext::keep(Some(MotionMark::BulgeSpine { spec, phase: TargetPhase::Claim, age: 0 }));
    }

    for to_spine in DIRS {
        let spine = h.neighbor(to_spine);
        let Some(MotionMark::BulgeSpine { spec, phase: TargetPhase::Claim, .. }) = spine.motion else { continue };
        let plus = if to_spine == spec.axis.opp() { true }
            else if to_spine == spec.axis { false }
            else { continue };
        let endpoint = h.neighbor(spec.side.opp());
        if h.center.payload != PayloadView::Empty
            || endpoint.motion.is_some_and(MotionMark::is_locking)
            || !bulge_endpoint_valid(endpoint, spec, plus) { continue; }
        return SiteNext::keep(Some(MotionMark::BulgeCorner {
            spec, plus, phase: SquarePhase::Claim, age: 0,
        }));
    }

    for to_corner in DIRS {
        let corner = h.neighbor(to_corner);
        let Some(MotionMark::BulgeCorner { spec, plus, phase: SquarePhase::Claim, .. }) = corner.motion else { continue };
        if to_corner != spec.side || !bulge_endpoint_valid(h.center, spec, plus) { continue; }
        return SiteNext::keep(Some(MotionMark::BulgeEndpoint {
            spec, plus, phase: EndpointPhase::Ready, age: 0, resume: None,
        }));
    }

    // Incoming claims outrank starting a new one.  A producer has only one possible
    // rootward direction, so no coordinate hash or global arbitration is needed.
    if let Some((source, signature)) = cold_shared_contest(h) {
        return SiteNext::keep(Some(MotionMark::SharedContest {
            source, signature, age: 0,
        }));
    }
    if let Some(spec) = bulge_offer(h) {
        return SiteNext::keep(Some(MotionMark::BulgeSource {
            spec, phase: SourcePhase::Claim, age: 0, retried: false,
        }));
    }
    if let Some(spec) = source_offer_spec(h) {
        return SiteNext::keep(Some(MotionMark::Source { spec, phase: SourcePhase::Claim, age: 0 }));
    }
    SiteNext::keep(None)
}

fn source_next(h: &Hood, spec: StarSpec, phase: SourcePhase, age: u8) -> SiteNext {
    if !source_geometry_matches(h, spec) { return SiteNext::keep(None); }
    let Some(age) = protocol_age(age, matches!(phase, SourcePhase::Countdown(_))) else {
        return SiteNext::keep(None);
    };
    let target = h.neighbor(spec.toward);
    match phase {
        SourcePhase::Claim => {
            let back_ready = spec.back_face.is_none_or(|f| {
                reciprocal_back(h.neighbor(f), spec) == Some(BackPhase::Ready)
            });
            let next = if reciprocal_target(target, spec) == Some(TargetPhase::Ready) && back_ready {
                SourcePhase::Armed
            } else { SourcePhase::Claim };
            SiteNext::keep(Some(MotionMark::Source { spec, phase: next, age }))
        }
        SourcePhase::Armed => {
            let next = if reciprocal_target(target, spec) == Some(TargetPhase::Countdown(2)) {
                SourcePhase::Countdown(1)
            } else { SourcePhase::Armed };
            SiteNext::keep(Some(MotionMark::Source { spec, phase: next, age }))
        }
        SourcePhase::Countdown(1) => {
            if reciprocal_target(target, spec) != Some(TargetPhase::Countdown(1)) {
                return SiteNext::keep(None);
            }
            SiteNext::keep(Some(MotionMark::Source { spec, phase: SourcePhase::Countdown(0), age }))
        }
        SourcePhase::Countdown(0) => {
            let next = match reciprocal_target(target, spec) {
                Some(TargetPhase::Countdown(0)) => SourcePhase::Countdown(0),
                Some(TargetPhase::Countdown(5)) => SourcePhase::Countdown(4),
                _ => return SiteNext::keep(None),
            };
            SiteNext::keep(Some(MotionMark::Source { spec, phase: next, age }))
        }
        SourcePhase::Countdown(4) => {
            if reciprocal_target(target, spec) != Some(TargetPhase::Countdown(4)) {
                return SiteNext::keep(None);
            }
            SiteNext::keep(Some(MotionMark::Source { spec, phase: SourcePhase::Countdown(3), age }))
        }
        SourcePhase::Countdown(3) => {
            if reciprocal_target(target, spec) != Some(TargetPhase::Countdown(3)) { return SiteNext::keep(None); }
            if let Some(f) = spec.back_face {
                if reciprocal_back(h.neighbor(f), spec) != Some(BackPhase::Countdown(0)) { return SiteNext::keep(None); }
            }
            let edit = match spec.back_port {
                None => PayloadEdit::Clear,
                Some(port) => {
                    let (_, faces, _) = h.center.payload.agent().expect("validated source agent");
                    let f = faces[port as usize].expect("live back port");
                    let hot = attachment_hot(h.neighbor(f), f.opp());
                    PayloadEdit::WriteSingleStrand { a: spec.toward, b: f, hot }
                }
            };
            SiteNext::edit(None, edit)
        }
        SourcePhase::Countdown(_) => SiteNext::keep(None),
    }
}

fn back_next(h: &Hood, spec: StarSpec, phase: BackPhase, age: u8) -> SiteNext {
    if !back_valid(SiteView { motion: None, ..h.center }, spec) { return SiteNext::keep(None); }
    let f = spec.back_face.expect("back role has a face");
    let source = h.neighbor(f.opp());
    let Some(source_phase) = reciprocal_source(source, spec) else { return SiteNext::keep(None) };
    let Some(age) = protocol_age(age, matches!(phase, BackPhase::Countdown(_))) else {
        return SiteNext::keep(None);
    };
    match phase {
        BackPhase::Ready => {
            let next = if source_phase == SourcePhase::Countdown(1) {
                BackPhase::Countdown(0)
            } else { BackPhase::Ready };
            SiteNext::keep(Some(MotionMark::Back {
                spec, phase: next, age, resume: None,
            }))
        }
        BackPhase::Countdown(0) => {
            if source_phase == SourcePhase::Countdown(3) {
                SiteNext::keep(None)
            } else {
                SiteNext::keep(Some(MotionMark::Back {
                    spec, phase, age, resume: None,
                }))
            }
        }
        BackPhase::Countdown(_) => SiteNext::keep(None),
    }
}

fn bulge_source_next(
    h: &Hood,
    spec: BulgeSpec,
    phase: SourcePhase,
    age: u8,
    retried: bool,
) -> SiteNext {
    if !bulge_source_valid(h, spec) { return SiteNext::keep(None); }
    let spine = h.neighbor(spec.side);
    let age = match protocol_age(age, matches!(phase, SourcePhase::Countdown(_))) {
        Some(age) => age,
        None if phase == SourcePhase::Claim && !retried => {
            return match bulge_retry_offer(h, spec) {
                Some(spec) => SiteNext::keep(Some(MotionMark::BulgeSource {
                    spec, phase: SourcePhase::Claim, age: 0, retried: true,
                })),
                None => SiteNext::keep(None),
            };
        }
        None => return SiteNext::keep(None),
    };
    match phase {
        SourcePhase::Claim => {
            let next = if reciprocal_bulge_spine(spine, spec) == Some(TargetPhase::Ready) {
                SourcePhase::Armed
            } else { SourcePhase::Claim };
            SiteNext::keep(Some(MotionMark::BulgeSource { spec, phase: next, age, retried }))
        }
        SourcePhase::Armed => {
            let next = if reciprocal_bulge_spine(spine, spec) == Some(TargetPhase::Countdown(2)) {
                SourcePhase::Countdown(1)
            } else { SourcePhase::Armed };
            SiteNext::keep(Some(MotionMark::BulgeSource { spec, phase: next, age, retried }))
        }
        SourcePhase::Countdown(1) => {
            if reciprocal_bulge_spine(spine, spec) != Some(TargetPhase::Countdown(1)) {
                return SiteNext::keep(None);
            }
            SiteNext::keep(Some(MotionMark::BulgeSource {
                spec, phase: SourcePhase::Countdown(0), age, retried,
            }))
        }
        SourcePhase::Countdown(0) => {
            let next = match reciprocal_bulge_spine(spine, spec) {
                Some(TargetPhase::Countdown(0)) => SourcePhase::Countdown(0),
                Some(TargetPhase::Countdown(5)) => SourcePhase::Countdown(4),
                _ => return SiteNext::keep(None),
            };
            SiteNext::keep(Some(MotionMark::BulgeSource { spec, phase: next, age, retried }))
        }
        SourcePhase::Countdown(4) => {
            if reciprocal_bulge_spine(spine, spec) != Some(TargetPhase::Countdown(4)) { return SiteNext::keep(None); }
            SiteNext::keep(Some(MotionMark::BulgeSource {
                spec, phase: SourcePhase::Countdown(3), age, retried,
            }))
        }
        SourcePhase::Countdown(3) => {
            if reciprocal_bulge_spine(spine, spec) != Some(TargetPhase::Countdown(3))
                || reciprocal_bulge_endpoint(h.neighbor(spec.axis), spec, true) != Some(EndpointPhase::Countdown(3))
                || reciprocal_bulge_endpoint(h.neighbor(spec.axis.opp()), spec, false) != Some(EndpointPhase::Countdown(3)) { return SiteNext::keep(None); }
            let mut out = SiteNext::edit(None, PayloadEdit::RemoveStrand {
                a: spec.axis, b: spec.axis.opp(),
            });
            out.bulge_commit = true;
            out
        }
        SourcePhase::Countdown(_) => SiteNext::keep(None),
    }
}

fn bulge_spine_next(h: &Hood, spec: BulgeSpec, phase: TargetPhase, age: u8) -> SiteNext {
    if h.center.payload != PayloadView::Empty || h.center.reserved { return SiteNext::keep(None); }
    let source = h.neighbor(spec.side.opp());
    if reciprocal_bulge_source(source, spec).is_none() { return SiteNext::keep(None); }
    let plus = h.neighbor(spec.axis);
    let minus = h.neighbor(spec.axis.opp());
    let Some(age) = protocol_age(age, matches!(phase, TargetPhase::Countdown(_))) else {
        return SiteNext::keep(None);
    };
    match phase {
        TargetPhase::Claim => {
            let ready = reciprocal_bulge_corner(plus, spec, true) == Some(SquarePhase::Ready)
                && reciprocal_bulge_corner(minus, spec, false) == Some(SquarePhase::Ready);
            let next = if ready { TargetPhase::Ready } else { TargetPhase::Claim };
            SiteNext::keep(Some(MotionMark::BulgeSpine { spec, phase: next, age }))
        }
        TargetPhase::Ready => {
            let next = if reciprocal_bulge_source(source, spec) == Some(SourcePhase::Armed) {
                TargetPhase::Countdown(2)
            } else { TargetPhase::Ready };
            SiteNext::keep(Some(MotionMark::BulgeSpine { spec, phase: next, age }))
        }
        TargetPhase::Countdown(2) => SiteNext::keep(Some(MotionMark::BulgeSpine {
            spec, phase: TargetPhase::Countdown(1), age,
        })),
        TargetPhase::Countdown(1) => SiteNext::keep(Some(MotionMark::BulgeSpine {
            spec, phase: TargetPhase::Countdown(0), age,
        })),
        TargetPhase::Countdown(0) => {
            if reciprocal_bulge_source(source, spec) != Some(SourcePhase::Countdown(0))
                || reciprocal_bulge_corner(plus, spec, true) != Some(SquarePhase::Countdown(0))
                || reciprocal_bulge_corner(minus, spec, false) != Some(SquarePhase::Countdown(0)) {
                return SiteNext::keep(None);
            }
            SiteNext::keep(Some(MotionMark::BulgeSpine {
                spec, phase: TargetPhase::Countdown(5), age,
            }))
        }
        TargetPhase::Countdown(5) => SiteNext::keep(Some(MotionMark::BulgeSpine {
            spec, phase: TargetPhase::Countdown(4), age,
        })),
        TargetPhase::Countdown(4) => SiteNext::keep(Some(MotionMark::BulgeSpine {
            spec, phase: TargetPhase::Countdown(3), age,
        })),
        TargetPhase::Countdown(3) => {
            if reciprocal_bulge_source(source, spec) != Some(SourcePhase::Countdown(3))
                || reciprocal_bulge_corner(plus, spec, true) != Some(SquarePhase::Countdown(3))
                || reciprocal_bulge_corner(minus, spec, false) != Some(SquarePhase::Countdown(3)) { return SiteNext::keep(None); }
            SiteNext::edit(None, PayloadEdit::WriteSingleStrand {
                a: spec.axis, b: spec.axis.opp(), hot: spec.hot,
            })
        }
        TargetPhase::Countdown(_) => SiteNext::keep(None),
    }
}

fn bulge_corner_next(h: &Hood, spec: BulgeSpec, plus: bool, phase: SquarePhase, age: u8) -> SiteNext {
    if h.center.payload != PayloadView::Empty || h.center.reserved { return SiteNext::keep(None); }
    let to_spine = if plus { spec.axis.opp() } else { spec.axis };
    let spine = h.neighbor(to_spine);
    if reciprocal_bulge_spine(spine, spec).is_none() { return SiteNext::keep(None); }
    let endpoint = h.neighbor(spec.side.opp());
    if !bulge_endpoint_valid(endpoint, spec, plus) { return SiteNext::keep(None); }
    let Some(age) = protocol_age(age, matches!(phase, SquarePhase::Countdown(_))) else {
        return SiteNext::keep(None);
    };
    match phase {
        SquarePhase::Claim => {
            let ready = reciprocal_bulge_endpoint(endpoint, spec, plus) == Some(EndpointPhase::Ready);
            let next = if ready { SquarePhase::Ready } else { SquarePhase::Claim };
            SiteNext::keep(Some(MotionMark::BulgeCorner { spec, plus, phase: next, age }))
        }
        SquarePhase::Ready => {
            let next = if reciprocal_bulge_spine(spine, spec) == Some(TargetPhase::Countdown(2)) {
                SquarePhase::Countdown(1)
            } else { SquarePhase::Ready };
            SiteNext::keep(Some(MotionMark::BulgeCorner { spec, plus, phase: next, age }))
        }
        SquarePhase::Countdown(1) => {
            if reciprocal_bulge_spine(spine, spec) != Some(TargetPhase::Countdown(1)) {
                return SiteNext::keep(None);
            }
            SiteNext::keep(Some(MotionMark::BulgeCorner {
                spec, plus, phase: SquarePhase::Countdown(0), age,
            }))
        }
        SquarePhase::Countdown(0) => {
            let next = match reciprocal_bulge_spine(spine, spec) {
                Some(TargetPhase::Countdown(0)) => SquarePhase::Countdown(0),
                Some(TargetPhase::Countdown(5)) => SquarePhase::Countdown(4),
                _ => return SiteNext::keep(None),
            };
            SiteNext::keep(Some(MotionMark::BulgeCorner { spec, plus, phase: next, age }))
        }
        SquarePhase::Countdown(4) => {
            if reciprocal_bulge_spine(spine, spec) != Some(TargetPhase::Countdown(4)) { return SiteNext::keep(None); }
            SiteNext::keep(Some(MotionMark::BulgeCorner { spec, plus, phase: SquarePhase::Countdown(3), age }))
        }
        SquarePhase::Countdown(3) => {
            if reciprocal_bulge_spine(spine, spec) != Some(TargetPhase::Countdown(3))
                || reciprocal_bulge_endpoint(endpoint, spec, plus) != Some(EndpointPhase::Countdown(3)) { return SiteNext::keep(None); }
            let (a, b) = if plus {
                (spec.side.opp(), spec.axis.opp())
            } else {
                (spec.axis, spec.side.opp())
            };
            SiteNext::edit(None, PayloadEdit::WriteSingleStrand { a, b, hot: spec.hot })
        }
        SquarePhase::Countdown(_) => SiteNext::keep(None),
    }
}

fn bulge_endpoint_next(h: &Hood, spec: BulgeSpec, plus: bool, phase: EndpointPhase, age: u8) -> SiteNext {
    if !bulge_endpoint_valid(SiteView { motion: None, ..h.center }, spec, plus) {
        return SiteNext::keep(None);
    }
    let corner = h.neighbor(spec.side);
    if reciprocal_bulge_corner(corner, spec, plus).is_none() { return SiteNext::keep(None); }
    let Some(age) = protocol_age(age, matches!(phase, EndpointPhase::Countdown(_))) else {
        return SiteNext::keep(None);
    };
    match phase {
        EndpointPhase::Ready => {
            let next = if reciprocal_bulge_corner(corner, spec, plus) == Some(SquarePhase::Countdown(1)) {
                EndpointPhase::Countdown(0)
            } else { EndpointPhase::Ready };
            SiteNext::keep(Some(MotionMark::BulgeEndpoint {
                spec, plus, phase: next, age, resume: None,
            }))
        }
        EndpointPhase::Countdown(0) => {
            let next = match reciprocal_bulge_corner(corner, spec, plus) {
                Some(SquarePhase::Countdown(0)) => EndpointPhase::Countdown(0),
                Some(SquarePhase::Countdown(4)) => EndpointPhase::Countdown(3),
                _ => return SiteNext::keep(None),
            };
            SiteNext::keep(Some(MotionMark::BulgeEndpoint {
                spec, plus, phase: next, age, resume: None,
            }))
        }
        EndpointPhase::Countdown(3) => {
            if reciprocal_bulge_corner(corner, spec, plus) != Some(SquarePhase::Countdown(3)) { return SiteNext::keep(None); }
            SiteNext::edit(None, PayloadEdit::Repoint {
                from: bulge_endpoint_from(spec, plus), to: spec.side,
            })
        }
        EndpointPhase::Countdown(_) => SiteNext::keep(None),
    }
}

fn target_next(h: &Hood, spec: StarSpec, phase: TargetPhase, age: u8) -> SiteNext {
    if !target_wire_matches(h.center, spec) { return SiteNext::keep(None); }
    let source = h.neighbor(spec.toward.opp());
    if reciprocal_source(source, spec).is_none() { return SiteNext::keep(None); }
    let Some(age) = protocol_age(age, matches!(phase, TargetPhase::Countdown(_))) else {
        return SiteNext::keep(None);
    };
    match phase {
        TargetPhase::Claim => {
            let Some(f) = spec.square_face else { return SiteNext::keep(None) };
            let square = h.neighbor(f);
            let ready = matches!(square.motion,
                Some(MotionMark::Square { spec: s, phase: SquarePhase::Ready, .. }) if s == spec);
            let next = if ready { TargetPhase::Ready } else { TargetPhase::Claim };
            SiteNext::keep(Some(MotionMark::Target { spec, phase: next, age }))
        }
        TargetPhase::Ready => {
            let next = if reciprocal_source(source, spec) == Some(SourcePhase::Armed) {
                TargetPhase::Countdown(2)
            } else { TargetPhase::Ready };
            SiteNext::keep(Some(MotionMark::Target { spec, phase: next, age }))
        }
        TargetPhase::Countdown(2) => SiteNext::keep(Some(MotionMark::Target {
            spec, phase: TargetPhase::Countdown(1), age,
        })),
        TargetPhase::Countdown(1) => SiteNext::keep(Some(MotionMark::Target {
            spec, phase: TargetPhase::Countdown(0), age,
        })),
        TargetPhase::Countdown(0) => {
            if reciprocal_source(source, spec) != Some(SourcePhase::Countdown(0)) {
                return SiteNext::keep(None);
            }
            if let Some(f) = spec.square_face {
                let square_ok = matches!(h.neighbor(f).motion,
                    Some(MotionMark::Square { spec: s, phase: SquarePhase::Countdown(0), .. }) if s == spec);
                if !square_ok { return SiteNext::keep(None); }
            }
            SiteNext::keep(Some(MotionMark::Target {
                spec, phase: TargetPhase::Countdown(5), age,
            }))
        }
        TargetPhase::Countdown(5) => SiteNext::keep(Some(MotionMark::Target {
            spec, phase: TargetPhase::Countdown(4), age,
        })),
        TargetPhase::Countdown(4) => SiteNext::keep(Some(MotionMark::Target {
            spec, phase: TargetPhase::Countdown(3), age,
        })),
        TargetPhase::Countdown(3) => {
            if reciprocal_source(source, spec) != Some(SourcePhase::Countdown(3)) { return SiteNext::keep(None); }
            if let Some(f) = spec.square_face {
                let square_ok = matches!(h.neighbor(f).motion,
                    Some(MotionMark::Square { spec: s, phase: SquarePhase::Countdown(3), .. }) if s == spec);
                if !square_ok { return SiteNext::keep(None); }
            }
            let (tag, _, nascent) = source.payload.agent().expect("validated source payload");
            if nascent || !tag.is_producer() { return SiteNext::keep(None); }
            let mut faces = [None; 3];
            faces[0] = Some(spec.exit);
            if let Some(port) = spec.back_port { faces[port as usize] = Some(spec.toward.opp()); }
            if let (Some(port), Some(f)) = (spec.square_port, spec.square_face) {
                faces[port as usize] = Some(f);
            }
            let debt = if tag.arity() == 3 {
                Some(MotionMark::Debt { back: spec.back_port.map(|_| spec.toward.opp()), square: spec.square_face })
            } else { None };
            let mut out = SiteNext::edit(debt, PayloadEdit::MoveAgentHere {
                source: spec.toward.opp(), faces,
            });
            out.target_commit = true;
            out
        }
        TargetPhase::Countdown(_) => SiteNext::keep(None),
    }
}

fn square_next(h: &Hood, spec: StarSpec, mode: SquareMode, phase: SquarePhase, age: u8) -> SiteNext {
    if square_geometry_mode(h, spec) != Some(mode) { return SiteNext::keep(None); }
    let f = spec.square_face.expect("square role has a face");
    let target = h.neighbor(f.opp());
    if reciprocal_target(target, spec).is_none() { return SiteNext::keep(None); }
    let endpoint = h.neighbor(spec.toward.opp());
    let Some(age) = protocol_age(age, matches!(phase, SquarePhase::Countdown(_))) else {
        return SiteNext::keep(None);
    };
    match phase {
        SquarePhase::Claim => {
            let ready = reciprocal_endpoint(endpoint, spec, mode) == Some(EndpointPhase::Ready);
            let next = if ready { SquarePhase::Ready } else { SquarePhase::Claim };
            SiteNext::keep(Some(MotionMark::Square {
                spec, mode, phase: next, age, resume: None,
            }))
        }
        SquarePhase::Ready => {
            let next = if reciprocal_target(target, spec) == Some(TargetPhase::Countdown(2)) {
                SquarePhase::Countdown(1)
            } else { SquarePhase::Ready };
            SiteNext::keep(Some(MotionMark::Square {
                spec, mode, phase: next, age, resume: None,
            }))
        }
        SquarePhase::Countdown(1) => {
            if reciprocal_target(target, spec) != Some(TargetPhase::Countdown(1)) {
                return SiteNext::keep(None);
            }
            SiteNext::keep(Some(MotionMark::Square {
                spec, mode, phase: SquarePhase::Countdown(0), age, resume: None,
            }))
        }
        SquarePhase::Countdown(0) => {
            let next = match reciprocal_target(target, spec) {
                Some(TargetPhase::Countdown(0)) => SquarePhase::Countdown(0),
                Some(TargetPhase::Countdown(5)) => SquarePhase::Countdown(4),
                _ => return SiteNext::keep(None),
            };
            SiteNext::keep(Some(MotionMark::Square {
                spec, mode, phase: next, age, resume: None,
            }))
        }
        SquarePhase::Countdown(4) => {
            if reciprocal_target(target, spec) != Some(TargetPhase::Countdown(4)) { return SiteNext::keep(None); }
            SiteNext::keep(Some(MotionMark::Square {
                spec, mode, phase: SquarePhase::Countdown(3), age, resume: None,
            }))
        }
        SquarePhase::Countdown(3) => {
            if reciprocal_target(target, spec) != Some(TargetPhase::Countdown(3))
                || reciprocal_endpoint(endpoint, spec, mode) != Some(EndpointPhase::Countdown(3)) { return SiteNext::keep(None); }
            let edit = match mode {
                SquareMode::Extend => PayloadEdit::WriteSingleStrand {
                    a: f.opp(), b: spec.toward.opp(), hot: attachment_hot(endpoint, f.opp()),
                },
                SquareMode::Truncate => PayloadEdit::Repoint {
                    from: spec.toward.opp(), to: f.opp(),
                },
            };
            SiteNext::edit(None, edit)
        }
        SquarePhase::Countdown(_) => SiteNext::keep(None),
    }
}

fn endpoint_next(h: &Hood, spec: StarSpec, mode: SquareMode, phase: EndpointPhase, age: u8) -> SiteNext {
    if !endpoint_valid(SiteView { motion: None, ..h.center }, spec, mode) {
        return SiteNext::keep(None);
    }
    let square = h.neighbor(spec.toward);
    if reciprocal_square(square, spec, mode).is_none() { return SiteNext::keep(None); }
    let Some(age) = protocol_age(age, matches!(phase, EndpointPhase::Countdown(_))) else {
        return SiteNext::keep(None);
    };
    match phase {
        EndpointPhase::Ready => {
            let next = if reciprocal_square(square, spec, mode) == Some(SquarePhase::Countdown(1)) {
                EndpointPhase::Countdown(0)
            } else { EndpointPhase::Ready };
            SiteNext::keep(Some(MotionMark::Endpoint {
                spec, mode, phase: next, age, resume: None,
            }))
        }
        EndpointPhase::Countdown(0) => {
            let next = match reciprocal_square(square, spec, mode) {
                Some(SquarePhase::Countdown(0)) => EndpointPhase::Countdown(0),
                Some(SquarePhase::Countdown(4)) => EndpointPhase::Countdown(3),
                _ => return SiteNext::keep(None),
            };
            SiteNext::keep(Some(MotionMark::Endpoint {
                spec, mode, phase: next, age, resume: None,
            }))
        }
        EndpointPhase::Countdown(3) => {
            if reciprocal_square(square, spec, mode) != Some(SquarePhase::Countdown(3)) { return SiteNext::keep(None); }
            let f = spec.square_face.expect("endpoint role has square face");
            let edit = match mode {
                SquareMode::Extend => PayloadEdit::Repoint { from: f.opp(), to: spec.toward },
                SquareMode::Truncate => PayloadEdit::RemoveStrand { a: f.opp(), b: spec.toward },
            };
            SiteNext::edit(None, edit)
        }
        EndpointPhase::Countdown(_) => SiteNext::keep(None),
    }
}

fn debt_next(h: &Hood, back: Option<Dir>, square: Option<Dir>) -> SiteNext {
    let Some((tag, faces, false)) = h.center.payload.agent() else { return SiteNext::keep(None) };
    let still = |d: Dir| {
        (0..tag.arity()).any(|i| faces[i] == Some(d))
            && h.neighbor(d).payload.wire().is_some_and(|w| w.with_face(d.opp()).is_some())
    };
    if back.is_some_and(still) || square.is_some_and(still) {
        SiteNext::keep(Some(MotionMark::Debt { back, square }))
    } else {
        SiteNext::keep(None)
    }
}

fn shared_contest_next(
    h: &Hood,
    source: Dir,
    signature: ContestSignature,
    age: u8,
) -> SiteNext {
    let unmarked = Hood {
        center: SiteView { motion: None, ..h.center },
        ..*h
    };
    let incoming = stable_next(&unmarked);
    if matches!(incoming.motion,
        Some(MotionMark::Target { .. }
            | MotionMark::Square { .. }
            | MotionMark::Endpoint { .. }
            | MotionMark::Back { .. }
            | MotionMark::BulgeSpine { .. }
            | MotionMark::BulgeCorner { .. }
            | MotionMark::BulgeEndpoint { .. }))
    {
        return incoming;
    }
    let Some((current_source, current_signature)) = cold_shared_contest(&unmarked) else {
        return SiteNext::keep(None);
    };
    if current_source != source || current_signature != signature {
        return SiteNext::keep(Some(MotionMark::SharedContest {
            source: current_source,
            signature: current_signature,
            age: 0,
        }));
    }
    if age >= SHARED_CONTEST_TICKS {
        SiteNext::keep(None)
    } else {
        SiteNext::keep(Some(MotionMark::SharedContest {
            source,
            signature,
            age: age.saturating_add(1),
        }))
    }
}

fn incoming_role(motion: Option<MotionMark>) -> bool {
    matches!(motion,
        Some(MotionMark::Target { .. }
            | MotionMark::Square { .. }
            | MotionMark::Endpoint { .. }
            | MotionMark::Back { .. }
            | MotionMark::BulgeSpine { .. }
            | MotionMark::BulgeCorner { .. }
            | MotionMark::BulgeEndpoint { .. }))
}

/// A debt-marked payload may lend itself to an incoming attachment role. That role is
/// locking while active, then restores the ratchet; the next local update drops it if the
/// original adjacent bends no longer exist.
fn carry_debt(mut next: SiteNext, resume: Option<DebtState>) -> SiteNext {
    let Some(resume) = resume else { return next };
    next.motion = match next.motion {
        Some(MotionMark::Square { spec, mode, phase, age, .. }) => {
            Some(MotionMark::Square { spec, mode, phase, age, resume: Some(resume) })
        }
        Some(MotionMark::Endpoint { spec, mode, phase, age, .. }) => {
            Some(MotionMark::Endpoint { spec, mode, phase, age, resume: Some(resume) })
        }
        Some(MotionMark::Back { spec, phase, age, .. }) => {
            Some(MotionMark::Back { spec, phase, age, resume: Some(resume) })
        }
        Some(MotionMark::BulgeEndpoint { spec, plus, phase, age, .. }) => {
            Some(MotionMark::BulgeEndpoint {
                spec, plus, phase, age, resume: Some(resume),
            })
        }
        None => Some(MotionMark::Debt { back: resume.back, square: resume.square }),
        other => other,
    };
    next
}

/// The strict substrate rule. This function cannot access anything outside its seven
/// values and returns only the center's next control/edit and center-emitted χ source.
pub fn next_site(h: &Hood) -> SiteNext {
    let mut next = match h.center.motion {
        None => stable_next(h),
        Some(MotionMark::Source { spec, phase, age }) => source_next(h, spec, phase, age),
        Some(MotionMark::Target { spec, phase, age }) => target_next(h, spec, phase, age),
        Some(MotionMark::Square { spec, mode, phase, age, resume }) => {
            carry_debt(square_next(h, spec, mode, phase, age), resume)
        }
        Some(MotionMark::Endpoint { spec, mode, phase, age, resume }) => {
            carry_debt(endpoint_next(h, spec, mode, phase, age), resume)
        }
        Some(MotionMark::Back { spec, phase, age, resume }) => {
            carry_debt(back_next(h, spec, phase, age), resume)
        }
        Some(MotionMark::BulgeSource { spec, phase, age, retried }) => {
            bulge_source_next(h, spec, phase, age, retried)
        }
        Some(MotionMark::BulgeSpine { spec, phase, age }) => bulge_spine_next(h, spec, phase, age),
        Some(MotionMark::BulgeCorner { spec, plus, phase, age }) => bulge_corner_next(h, spec, plus, phase, age),
        Some(MotionMark::BulgeEndpoint { spec, plus, phase, age, resume }) => {
            carry_debt(bulge_endpoint_next(h, spec, plus, phase, age), resume)
        }
        Some(MotionMark::Debt { back, square }) => {
            // Debt is a nonlocking ratchet, not ownership of the payload. Incoming
            // protocol roles may therefore preempt it; only a new source offer remains
            // suppressed until the debt geometry has relaxed.
            let unmarked = Hood {
                center: SiteView { motion: None, ..h.center },
                ..*h
            };
            let incoming = stable_next(&unmarked);
            if incoming_role(incoming.motion) {
                carry_debt(incoming, Some(DebtState { back, square }))
            } else {
                debt_next(h, back, square)
            }
        }
        Some(MotionMark::SharedContest { source, signature, age }) => {
            shared_contest_next(h, source, signature, age)
        }
    };
    next.chi_source = local_pressure_source(h);
    next
}

fn repointed(cell: &Cell, from: Dir, to: Dir) -> Option<Cell> {
    match cell {
        Cell::Agent(a) => {
            let mut a = a.clone();
            let port = a.port_at(from)?;
            if a.port_at(to).is_some() { return None; }
            a.faces[port] = Some(to);
            Some(Cell::Agent(a))
        }
        Cell::Wire(w) => {
            if w.with_he(to).is_some() { return None; }
            let mut w = w.clone();
            let slot = w.strands.iter_mut().flatten().find(|s| s.contains(from))?;
            let old = *slot;
            let mut new = if old.a == from {
                Strand::new(to, old.b)
            } else {
                Strand::new(old.a, to)
            };
            new.hot = old.hot;
            new.cooldown = old.cooldown;
            *slot = new;
            Some(Cell::Wire(w))
        }
        Cell::Seed(_) => None,
    }
}

/// Convert a finite center edit into a full host payload from the frozen grid. Copying
/// `sid` here is observer bookkeeping; the local rule never saw it.
fn materialize_edit(grid: &Grid, p: Pos, edit: PayloadEdit) -> Option<Option<Cell>> {
    match edit {
        PayloadEdit::Keep => None,
        PayloadEdit::Clear => Some(None),
        PayloadEdit::WriteSingleStrand { a, b, hot } => {
            let mut strand = Strand::new(a, b);
            strand.hot = hot;
            let mut wire = WireCell::default();
            wire.strands[0] = Some(strand);
            Some(Some(Cell::Wire(wire)))
        }
        PayloadEdit::MoveAgentHere { source, faces } => {
            let source_pos = step(p, source);
            let mut agent: AgentCell = grid.agent(source_pos)?.clone();
            agent.faces = faces;
            agent.nascent = false;
            agent.frustration = 0;
            Some(Some(Cell::Agent(agent)))
        }
        PayloadEdit::Repoint { from, to } => {
            let cell = grid.cells.get(&p)?;
            Some(Some(repointed(cell, from, to)?))
        }
        PayloadEdit::RemoveStrand { a, b } => {
            let Cell::Wire(old) = grid.cells.get(&p)? else { return None };
            let mut wire = old.clone();
            let slot = wire.strands.iter_mut().find(|slot| {
                slot.is_some_and(|s| (s.a == a && s.b == b) || (s.a == b && s.b == a))
            })?;
            *slot = None;
            if wire.count() == 0 { Some(None) } else { Some(Some(Cell::Wire(wire))) }
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct LocalMotionStep {
    /// Sites whose control or payload state changed.
    pub changes: usize,
    /// Committed `(source, target)` translations, observer-only.
    pub approaches: Vec<(Pos, Pos)>,
    pub bulges: Vec<Pos>,
    pub edits: Vec<(Pos, PayloadEdit)>,
    pub pressure_sources: Vec<(Pos, u8)>,
}

/// Advance the strict rule in canonical sparse-coordinate order. This wrapper is useful
/// for focused protocol tests; the scheduler uses [`advance_local_motion_permuted`] to
/// continuously check that frozen-snapshot evaluation does not depend on traversal order.
pub fn advance_local_motion(grid: &mut Grid) -> LocalMotionStep {
    advance_local_motion_ordered(grid, None)
}

/// Advance the same frozen-snapshot rule after a deterministic Fisher-Yates permutation
/// of the active centers. `seed` selects a reproducible schedule and `sweep` selects one
/// permutation within it. The permutation changes evaluation order only: all centers read
/// the same old grid and all writes commit after evaluation, so a sound local rule must
/// produce exactly the canonical result.
pub fn advance_local_motion_permuted(
    grid: &mut Grid,
    seed: u64,
    sweep: u64,
) -> LocalMotionStep {
    advance_local_motion_ordered(grid, Some((seed, sweep)))
}

fn splitmix64(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9e37_79b9_7f4a_7c15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    z ^ (z >> 31)
}

fn permute<T>(items: &mut [T], seed: u64, sweep: u64) {
    let mut state = seed
        ^ sweep.wrapping_mul(0xd1b5_4a32_d192_ed03)
        ^ (items.len() as u64).wrapping_mul(0x94d0_49bb_1331_11eb);
    for i in (1..items.len()).rev() {
        let j = (splitmix64(&mut state) % (i as u64 + 1)) as usize;
        items.swap(i, j);
    }
}

/// Evaluate the strict rule on a sparse active region. Sparse enumeration is only an
/// optimization: every occupied/control site and its one-cell halo is included, and an
/// all-empty site outside that set would return the all-empty state.
fn advance_local_motion_ordered(
    grid: &mut Grid,
    order: Option<(u64, u64)>,
) -> LocalMotionStep {
    let mut active: BTreeSet<Pos> = grid.cells.keys().copied().collect();
    active.extend(grid.motion.keys().copied());
    let halo: Vec<Pos> = active.iter().flat_map(|p| DIRS.map(|d| step(*p, d))).collect();
    active.extend(halo);

    let mut positions: Vec<Pos> = active.into_iter().collect();
    if let Some((seed, sweep)) = order { permute(&mut positions, seed, sweep); }

    let mut computed: Vec<(Pos, SiteNext, Option<Option<Cell>>)> = positions.into_iter().map(|p| {
        let next = next_site(&hood_at(grid, p));
        let payload = materialize_edit(grid, p, next.edit);
        (p, next, payload)
    }).collect();
    // Observer vectors and sparse commits remain byte-for-byte stable across schedules.
    // Sorting here is not needed for the dynamics; it makes order invariance directly
    // testable and keeps trace event ordering deterministic.
    computed.sort_by_key(|(p, _, _)| *p);

    let mut out = LocalMotionStep::default();
    for (p, next, payload) in &computed {
        let old_motion = grid.motion.get(p).copied();
        if old_motion != next.motion || payload.is_some() { out.changes += 1; }
        if next.edit != PayloadEdit::Keep { out.edits.push((*p, next.edit)); }
        if next.chi_source > 0 { out.pressure_sources.push((*p, next.chi_source)); }
        if next.target_commit {
            let source = match next.edit {
                PayloadEdit::MoveAgentHere { source, .. } => step(*p, source),
                _ => unreachable!("target commit without agent move"),
            };
            out.approaches.push((source, *p));
        }
        if next.bulge_commit { out.bulges.push(*p); }
    }

    // Payload and control planes are both committed only after every center has read the
    // frozen snapshot. No application-order information can flow through this microtick.
    for (p, _, payload) in &computed {
        if let Some(replacement) = payload {
            match replacement {
                Some(cell) => { grid.cells.insert(*p, cell.clone()); }
                None => { grid.cells.remove(p); }
            }
        }
    }
    for (p, next, _) in computed {
        match next.motion {
            Some(mark) => { grid.motion.insert(p, mark); }
            None => { grid.motion.remove(&p); }
        }
    }
    grid.transport += out.approaches.len() as u64;
    out
}

impl Grid {
    pub fn motion_footprint_free<'a>(&self, cells: impl IntoIterator<Item = &'a Pos>) -> bool {
        cells.into_iter().all(|p| !self.motion_locked(*p))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lattice::Topo;
    use crate::net::Net;
    use crate::transitions::{apply_slide, plan_slide};

    fn agent(tag: Tag, faces: [Option<Dir>; 3], sid: u32) -> AgentCell {
        AgentCell { tag, faces, sid, nascent: false, frustration: 0 }
    }

    fn one(a: Dir, b: Dir) -> Cell {
        let mut w = WireCell::default();
        w.strands[0] = Some(Strand::new(a, b));
        Cell::Wire(w)
    }

    fn stem_fixture(topo: Topo) -> (Grid, Net) {
        let mut net = Net::new();
        let s = net.mk(Tag::S);
        let out = net.mk(Tag::Out);
        let child = net.mk(Tag::L);
        net.link(s, 0, out, 0);
        net.link(s, 1, child, 0);

        let mut g = Grid::new(topo);
        g.put_agent((0, 0, 0), agent(Tag::S, [Some(Dir::E), Some(Dir::N), None], s));
        g.cells.insert((1, 0, 0), one(Dir::W, Dir::E));
        g.put_agent((2, 0, 0), agent(Tag::Out, [Some(Dir::W), None, None], out));
        g.put_agent((0, -1, 0), agent(Tag::L, [Some(Dir::S), None, None], child));
        g.check_projection(&net);
        (g, net)
    }

    #[test]
    fn stem_leaf_inchworm_is_projection_identity() {
        for topo in [Topo::Bilayer, Topo::Full3D] {
            let (mut g, net) = stem_fixture(topo);
            let mut commits = vec![];
            for _ in 0..40 {
                let tick = advance_local_motion(&mut g);
                commits.extend(tick.approaches);
                g.check_projection(&net);
                if g.total_strands() == 0 { break; }
            }
            assert_eq!(commits, vec![((0, 0, 0), (1, 0, 0)), ((0, -1, 0), (0, 0, 0))]);
            assert_eq!(g.total_strands(), 0, "the child must consume the S drag bend");
            assert_eq!(g.agent((1, 0, 0)).map(|a| a.tag), Some(Tag::S));
            assert_eq!(g.agent((0, 0, 0)).map(|a| a.tag), Some(Tag::L));
        }
    }

    fn fork_fixture(topo: Topo, truncate: bool) -> (Grid, Net) {
        let mut net = Net::new();
        let f = net.mk(Tag::F);
        let root = net.mk(Tag::Out);
        let a = net.mk(Tag::Out);
        let b = net.mk(Tag::Out);
        net.link(f, 0, root, 0);
        net.link(f, 1, a, 0);
        net.link(f, 2, b, 0);

        let mut g = Grid::new(topo);
        g.put_agent((0, 0, 0), agent(Tag::F,
            [Some(Dir::E), Some(Dir::N), Some(Dir::S)], f));
        g.cells.insert((1, 0, 0), one(Dir::W, Dir::E));
        g.put_agent((2, 0, 0), agent(Tag::Out, [Some(Dir::W), None, None], root));
        g.put_agent((0, -1, 0), agent(Tag::Out, [Some(Dir::S), None, None], a));
        if truncate {
            g.cells.insert((0, 1, 0), one(Dir::N, Dir::E));
            g.cells.insert((1, 1, 0), one(Dir::W, Dir::S));
            g.put_agent((1, 2, 0), agent(Tag::Out, [Some(Dir::N), None, None], b));
        } else {
            g.put_agent((0, 1, 0), agent(Tag::Out, [Some(Dir::N), None, None], b));
        }
        g.check_projection(&net);
        (g, net)
    }

    fn run_one_fork_step(g: &mut Grid, net: &Net) {
        for _ in 0..30 {
            let tick = advance_local_motion(g);
            g.check_projection(net);
            if !tick.approaches.is_empty() {
                assert_eq!(tick.approaches, vec![((0, 0, 0), (1, 0, 0))]);
                return;
            }
        }
        panic!("fork star transaction did not commit");
    }

    #[test]
    fn fork_swept_square_extends_locally() {
        for topo in [Topo::Bilayer, Topo::Full3D] {
            let (mut g, net) = fork_fixture(topo, false);
            run_one_fork_step(&mut g, &net);
            assert_eq!(g.total_strands(), 2); // principal -1, back +1, square +1
            let f = g.agent((1, 0, 0)).expect("moved fork");
            assert_eq!(f.faces, [Some(Dir::E), Some(Dir::W), Some(Dir::S)]);
            assert!(g.wire((0, 0, 0)).is_some_and(|w| w.with_he(Dir::E).is_some()));
            assert!(g.wire((1, 1, 0)).is_some_and(|w| w.with_he(Dir::N).is_some()));
            assert_eq!(g.agent((0, 1, 0)).unwrap().faces[0], Some(Dir::E));
        }
    }

    #[test]
    fn fork_swept_square_prefers_truncation() {
        for topo in [Topo::Bilayer, Topo::Full3D] {
            let (mut g, net) = fork_fixture(topo, true);
            run_one_fork_step(&mut g, &net);
            assert_eq!(g.total_strands(), 2); // principal and source aux turn disappear; back appears
            assert!(g.is_empty((0, 1, 0)), "source aux turn must be consumed");
            let c = g.wire((1, 1, 0)).expect("surviving square continuation");
            assert!(c.with_he(Dir::N).is_some());
            assert!(c.with_he(Dir::S).is_some());
        }
    }

    #[test]
    fn nonlocking_debt_yields_to_an_incoming_back_claim() {
        let (mut g, net) = fork_fixture(Topo::Full3D, false);
        advance_local_motion(&mut g);
        assert!(matches!(g.motion.get(&(0, 0, 0)), Some(MotionMark::Source { .. })));
        g.motion.insert((0, -1, 0), MotionMark::Debt {
            back: Some(Dir::S),
            square: None,
        });
        run_one_fork_step(&mut g, &net);
        assert!(matches!(g.motion.get(&(0, -1, 0)), Some(MotionMark::Debt { .. })),
            "the unchanged endpoint must resume its ratchet after lending a Back role: {:?}",
            g.motion.get(&(0, -1, 0)));
    }

    #[test]
    fn nonlocking_debt_yields_to_square_endpoint_and_resumes() {
        let (mut g, net) = fork_fixture(Topo::Full3D, false);
        for _ in 0..8 {
            advance_local_motion(&mut g);
            if matches!(g.motion.get(&(1, 1, 0)), Some(MotionMark::Square {
                phase: SquarePhase::Claim, ..
            })) {
                break;
            }
        }
        assert!(matches!(g.motion.get(&(1, 1, 0)), Some(MotionMark::Square {
            phase: SquarePhase::Claim, ..
        })), "fixture never exposed the endpoint recruitment tick");
        g.motion.insert((0, 1, 0), MotionMark::Debt {
            back: Some(Dir::N),
            square: None,
        });
        run_one_fork_step(&mut g, &net);
        assert!(matches!(g.motion.get(&(0, 1, 0)), Some(MotionMark::Debt { .. })),
            "the repointed endpoint restores Debt before revalidating its geometry: {:?}",
            g.motion.get(&(0, 1, 0)));
    }

    #[test]
    fn nonlocking_debt_yields_to_a_truncating_square_and_resumes() {
        let (mut g, net) = fork_fixture(Topo::Full3D, true);
        let sid = g.agent((1, 2, 0)).expect("fixture endpoint").sid;
        g.cells.remove(&(1, 2, 0));
        g.cells.insert((1, 1, 0), Cell::Agent(agent(
            Tag::Out, [Some(Dir::W), None, None], sid,
        )));
        g.motion.insert((1, 1, 0), MotionMark::Debt {
            back: Some(Dir::W),
            square: None,
        });
        g.check_projection(&net);
        run_one_fork_step(&mut g, &net);
        assert!(matches!(g.motion.get(&(1, 1, 0)), Some(MotionMark::Debt { .. })),
            "a Square role must return the borrowed ratchet after its repoint");
    }

    fn alternate_square_fixture(block_east: bool) -> (Grid, Net) {
        let mut net = Net::new();
        let f = net.mk(Tag::F);
        let root = net.mk(Tag::Out);
        let east = net.mk(if block_east { Tag::S } else { Tag::L });
        let north = net.mk(Tag::S);
        let north_child = net.mk(Tag::Out);
        net.link(f, 0, root, 0);
        net.link(f, 1, east, 0);
        net.link(f, 2, north, 0);
        net.link(north, 1, north_child, 0);
        let east_child = if block_east {
            let child = net.mk(Tag::Out);
            net.link(east, 1, child, 0);
            Some(child)
        } else {
            None
        };

        let mut g = Grid::new(Topo::Full3D);
        g.put_agent(
            (0, 0, 1),
            agent(Tag::F, [Some(Dir::D), Some(Dir::E), Some(Dir::N)], f),
        );
        g.cells.insert((0, 0, 0), one(Dir::U, Dir::D));
        g.put_agent(
            (0, 0, -1),
            agent(Tag::Out, [Some(Dir::U), None, None], root),
        );
        g.put_agent(
            (0, -1, 1),
            agent(Tag::S, [Some(Dir::S), Some(Dir::D), None], north),
        );
        g.put_agent(
            (0, -1, 0),
            agent(Tag::Out, [Some(Dir::U), None, None], north_child),
        );
        if let Some(child) = east_child {
            g.put_agent(
                (1, 0, 1),
                agent(Tag::S, [Some(Dir::W), Some(Dir::D), None], east),
            );
            g.put_agent(
                (1, 0, 0),
                agent(Tag::Out, [Some(Dir::U), None, None], child),
            );
        } else {
            g.put_agent(
                (1, 0, 1),
                agent(Tag::L, [Some(Dir::W), None, None], east),
            );
        }
        g.check_projection(&net);
        (g, net)
    }

    #[test]
    fn fork_chooses_the_locally_feasible_aux_square() {
        let (mut g, net) = alternate_square_fixture(false);
        let first = next_site(&hood_at(&g, (0, 0, 1)));
        let Some(MotionMark::Source { spec, .. }) = first.motion else {
            panic!("fork did not offer a star");
        };
        assert_eq!(spec.back_face, Some(Dir::N));
        assert_eq!(spec.square_face, Some(Dir::E));

        for _ in 0..40 {
            let tick = advance_local_motion(&mut g);
            g.check_projection(&net);
            if !tick.approaches.is_empty() {
                assert_eq!(tick.approaches, vec![((0, 0, 1), (0, 0, 0))]);
                return;
            }
        }
        panic!("fork did not use its feasible alternate swept square");
    }

    #[test]
    fn claimed_star_keeps_its_route_when_pressure_changes() {
        let (mut g, net) = alternate_square_fixture(false);
        let source = (0, 0, 1);
        for _ in 0..40 {
            if matches!(g.motion.get(&source), Some(MotionMark::Source {
                phase: SourcePhase::Countdown(3), ..
            })) {
                break;
            }
            advance_local_motion(&mut g);
            g.check_projection(&net);
        }
        assert!(matches!(g.motion.get(&source), Some(MotionMark::Source {
            phase: SourcePhase::Countdown(3), ..
        })), "fixture never reached the final commit wave");

        // This changes which auxiliary a fresh offer would prefer. The active protocol
        // must validate its already claimed faces, not rerun that volatile selection.
        g.chi.insert((0, 0, 0), 1);
        let tick = advance_local_motion(&mut g);
        assert_eq!(tick.approaches, vec![(source, (0, 0, 0))]);
        assert_eq!(tick.edits.len(), 4, "the source, target, square, and endpoint commit together");
        g.check_projection(&net);
    }

    #[test]
    fn fork_with_no_locally_feasible_aux_square_stays_quiescent() {
        let (mut g, net) = alternate_square_fixture(true);
        let before = format!("{:?}", g.cells);
        for _ in 0..40 {
            let tick = advance_local_motion(&mut g);
            assert_eq!(tick.changes, 0);
            g.check_projection(&net);
        }
        assert_eq!(format!("{:?}", g.cells), before);
        assert!(g.motion.is_empty());
    }

    fn shared_stem_fixture() -> Grid {
        let (mut g, _) = stem_fixture(Topo::Full3D);
        let q = (1, 0, 0);
        let Cell::Wire(w) = g.cells.get_mut(&q).unwrap() else { unreachable!() };
        w.strands[1] = Some(Strand::new(Dir::N, Dir::S));
        g
    }

    #[test]
    fn shared_target_never_starts_or_moves_foreign_wire() {
        let mut g = shared_stem_fixture();
        let q = (1, 0, 0);
        let before = payload_view(g.cells.get(&q));
        let first = next_site(&hood_at(&g, q));
        assert!(matches!(first.motion, Some(MotionMark::SharedContest { age: 0, .. })));
        assert_eq!(first.chi_source, 0, "transient contention must not pump immediately");
        for _ in 0..8 { advance_local_motion(&mut g); }
        assert!(matches!(g.motion.get(&q), Some(MotionMark::SharedContest { .. })));
        assert_eq!(payload_view(g.cells.get(&q)), before);
        assert_eq!(g.agent((0, 0, 0)).map(|a| a.tag), Some(Tag::S));
    }

    #[test]
    fn shared_contest_restarts_when_visible_geometry_changes() {
        let mut g = shared_stem_fixture();
        let q = (1, 0, 0);
        let first = next_site(&hood_at(&g, q));
        let Some(MotionMark::SharedContest { signature: first_signature, .. }) = first.motion
            else { panic!("shared target did not start its persistence mark") };
        g.motion.insert(q, first.motion.unwrap());
        for _ in 0..8 { advance_local_motion(&mut g); }
        let Cell::Wire(w) = g.cells.get_mut(&q).unwrap() else { unreachable!() };
        w.strands[1] = Some(Strand::new(Dir::U, Dir::D));

        let changed = next_site(&hood_at(&g, q));
        let Some(MotionMark::SharedContest { signature, age, .. }) = changed.motion
            else { panic!("changed shared geometry should begin a fresh persistence mark") };
        assert_eq!(age, 0);
        assert_ne!(signature, first_signature);
        assert_eq!(changed.chi_source, 0);
    }

    #[test]
    fn hot_activity_suppresses_the_cold_shared_timer() {
        let mut g = shared_stem_fixture();
        let q = (1, 0, 0);
        let Cell::Wire(w) = g.cells.get_mut(&q).unwrap() else { unreachable!() };
        w.strands[1].as_mut().unwrap().hot = true;
        let next = next_site(&hood_at(&g, q));
        assert!(next.motion.is_none());
        assert_eq!(next.chi_source, 0,
            "active transport must not be classified as a static compression barrier");
    }

    #[test]
    fn cold_shared_target_uses_its_own_pressure_response() {
        let mut net = Net::new();
        let producer = net.mk(Tag::L);
        let root = net.mk(Tag::Out);
        let north = net.mk(Tag::Out);
        let south = net.mk(Tag::Out);
        net.link(producer, 0, root, 0);
        net.link(north, 0, south, 0);

        let q = (0, 0, 0);
        let mut g = Grid::new(Topo::Full3D);
        g.put_agent((-1, 0, 0), agent(Tag::L, [Some(Dir::E), None, None], producer));
        g.put_agent((1, 0, 0), agent(Tag::Out, [Some(Dir::W), None, None], root));
        g.put_agent((0, -1, 0), agent(Tag::Out, [Some(Dir::S), None, None], north));
        g.put_agent((0, 1, 0), agent(Tag::Out, [Some(Dir::N), None, None], south));
        g.cells.insert(q, one(Dir::W, Dir::E));
        let Cell::Wire(shared) = g.cells.get_mut(&q).unwrap() else { unreachable!() };
        shared.strands[1] = Some(Strand::new(Dir::N, Dir::S));
        g.check_projection(&net);

        let before = payload_view(g.cells.get(&q));
        for _ in 0..=SHARED_CONTEST_TICKS {
            let tick = advance_local_motion(&mut g);
            assert!(tick.pressure_sources.is_empty(),
                "the source must wait until age 64 is present in the frozen input");
        }
        let pumps = advance_local_motion(&mut g).pressure_sources;
        assert!(pumps.contains(&(q, 16)));
        assert_eq!(payload_view(g.cells.get(&q)), before,
            "emitting pressure cannot rewrite the contested switchbox");
        g.chi_step(&pumps);

        let mut committed = false;
        for _ in 0..30 {
            let tick = advance_local_motion(&mut g);
            g.check_projection(&net);
            if !tick.bulges.is_empty() {
                committed = true;
                break;
            }
        }
        assert!(committed, "the pressured switchbox did not choose an owned bulge");
        assert_eq!(g.agent((-1, 0, 0)).map(|a| a.tag), Some(Tag::L),
            "the requester must not enter or push the shared target");
        assert_eq!(g.strand_count(q), 1,
            "one strand leaves by its own projection-preserving response");
    }

    #[test]
    fn reserved_wire_can_depart_but_reserved_targets_reject_arrivals() {
        let mut net = Net::new();
        let left = net.mk(Tag::Out);
        let right = net.mk(Tag::Out);
        net.link(left, 0, right, 0);

        let mut g = Grid::new(Topo::Full3D);
        g.put_agent((-1, 0, 0), agent(Tag::Out, [Some(Dir::E), None, None], left));
        g.cells.insert((0, 0, 0), one(Dir::W, Dir::E));
        g.put_agent((1, 0, 0), agent(Tag::Out, [Some(Dir::W), None, None], right));
        g.reserved.insert((0, 0, 0), (9, 9, 9));
        g.chi.insert((0, 0, 0), 250);

        let first = next_site(&hood_at(&g, (0, 0, 0)));
        assert!(matches!(first.motion, Some(MotionMark::BulgeSource { .. })),
            "a reservation must not freeze its incumbent wire");
        let mut committed = false;
        for _ in 0..30 {
            let tick = advance_local_motion(&mut g);
            g.check_projection(&net);
            if !tick.bulges.is_empty() {
                committed = true;
                break;
            }
        }
        assert!(committed);
        assert!(g.is_empty((0, 0, 0)), "the reserved source cell should be vacated");

        let mut blocked = Grid::new(Topo::Full3D);
        blocked.cells.insert((0, 0, 0), one(Dir::W, Dir::E));
        blocked.chi.insert((0, 0, 0), 250);
        for side in [Dir::N, Dir::S, Dir::U, Dir::D] {
            blocked.reserved.insert(step((0, 0, 0), side), (9, 9, 9));
        }
        assert!(bulge_offer(&hood_at(&blocked, (0, 0, 0))).is_none(),
            "a reserved destination must remain unavailable");
    }

    #[test]
    fn reserved_wire_can_take_its_host_pressure_exit() {
        let mut net = Net::new();
        let left = net.mk(Tag::Out);
        let right = net.mk(Tag::Out);
        net.link(left, 0, right, 0);

        let p = (0, 0, 0);
        let mut g = Grid::new(Topo::Full3D);
        g.put_agent((-1, 0, 0), agent(Tag::Out, [Some(Dir::E), None, None], left));
        g.cells.insert(p, one(Dir::W, Dir::E));
        g.put_agent((1, 0, 0), agent(Tag::Out, [Some(Dir::W), None, None], right));
        g.reserved.insert(p, (9, 9, 9));
        g.chi.insert(p, 16);

        let strand = g.wire(p).unwrap().iter().next().unwrap();
        let plan = plan_slide(&g, p, strand, &[])
            .expect("the incumbent strand should own a route out of its reserved source");
        apply_slide(&mut g, &plan);
        assert!(g.is_empty(p));
        g.check_projection(&net);
    }

    #[test]
    fn straight_pressure_bulge_is_staged_and_projection_identity() {
        for topo in [Topo::Bilayer, Topo::Full3D] {
            let (mut g, net) = bulge_fixture(topo);

            let mut committed = false;
            for _ in 0..30 {
                let tick = advance_local_motion(&mut g);
                g.check_projection(&net);
                if !tick.bulges.is_empty() {
                    assert_eq!(tick.bulges, vec![(0, 0, 0)]);
                    committed = true;
                    break;
                }
            }
            assert!(committed, "bulge protocol did not commit");
            assert_eq!(g.strand_count((0, 0, 0)), 1, "the crossing stays in its owned cell");
            assert_eq!(g.total_strands(), 4);
            assert!(g.wire((0, 0, 1)).is_some_and(|w| w.with_he(Dir::E).is_some()));
            assert_eq!(g.agent((1, 0, 0)).unwrap().faces[0], Some(Dir::U));
            assert_eq!(g.agent((-1, 0, 0)).unwrap().faces[0], Some(Dir::U));
        }
    }

    #[test]
    fn bulge_endpoints_borrow_and_restore_nonlocking_debt() {
        let (mut g, net) = bulge_fixture(Topo::Full3D);
        g.motion.insert((1, 0, 0), MotionMark::Debt {
            back: Some(Dir::W), square: None,
        });
        g.motion.insert((-1, 0, 0), MotionMark::Debt {
            back: Some(Dir::E), square: None,
        });
        for _ in 0..30 {
            let tick = advance_local_motion(&mut g);
            g.check_projection(&net);
            if !tick.bulges.is_empty() {
                assert!(matches!(g.motion.get(&(1, 0, 0)), Some(MotionMark::Debt { .. })));
                assert!(matches!(g.motion.get(&(-1, 0, 0)), Some(MotionMark::Debt { .. })));
                return;
            }
        }
        panic!("Debt-marked endpoints blocked their incoming bulge roles");
    }

    fn bulge_fixture(topo: Topo) -> (Grid, Net) {
        let mut net = Net::new();
        let a = net.mk(Tag::Out);
        let b = net.mk(Tag::Out);
        let c = net.mk(Tag::Out);
        let d = net.mk(Tag::Out);
        net.link(a, 0, b, 0);
        net.link(c, 0, d, 0);
        let mut g = Grid::new(topo);
        g.put_agent((1, 0, 0), agent(Tag::Out, [Some(Dir::W), None, None], a));
        g.put_agent((-1, 0, 0), agent(Tag::Out, [Some(Dir::E), None, None], b));
        g.cells.insert((0, 0, 0), one(Dir::E, Dir::W));
        let Cell::Wire(shared) = g.cells.get_mut(&(0, 0, 0)).unwrap() else { unreachable!() };
        let mut crossing = Strand::new(Dir::N, Dir::S);
        crossing.hot = true;
        shared.strands[1] = Some(crossing);
        g.put_agent((0, -1, 0), agent(Tag::Out, [Some(Dir::S), None, None], c));
        g.put_agent((0, 1, 0), agent(Tag::Out, [Some(Dir::N), None, None], d));
        g.chi.insert((0, 0, 0), 20);
        g.check_projection(&net);
        (g, net)
    }

    #[test]
    fn commit_countdown_cannot_expire_one_role_early() {
        let (mut g, net) = bulge_fixture(Topo::Full3D);
        for _ in 0..40 {
            if matches!(g.motion.get(&(0, 0, 0)), Some(MotionMark::BulgeSource {
                phase: SourcePhase::Countdown(3), ..
            })) {
                break;
            }
            advance_local_motion(&mut g);
        }
        assert!(matches!(g.motion.get(&(0, 0, 0)), Some(MotionMark::BulgeSource {
            phase: SourcePhase::Countdown(3), ..
        })), "fixture never reached the final commit wave");
        let spine = g.motion.values_mut().find(|m| matches!(m,
            MotionMark::BulgeSpine { phase: TargetPhase::Countdown(3), ..
        })).expect("armed spine");
        let MotionMark::BulgeSpine { age, .. } = spine else { unreachable!() };
        *age = PROTOCOL_TTL;

        let tick = advance_local_motion(&mut g);
        assert_eq!(tick.bulges, vec![(0, 0, 0)]);
        assert_eq!(tick.edits.len(), 6, "every role must write in the same frozen tick");
        g.check_projection(&net);
    }

    #[test]
    fn locking_motion_freezes_heat_until_commit_or_abort() {
        let mut g = Grid::new(Topo::Full3D);
        g.cells.insert((0, 0, 0), one(Dir::E, Dir::W));
        let mut hot = Strand::new(Dir::W, Dir::E);
        hot.hot = true;
        let mut continuation = WireCell::default();
        continuation.strands[0] = Some(hot);
        g.cells.insert((1, 0, 0), Cell::Wire(continuation));
        let spec = BulgeSpec { axis: Dir::N, side: Dir::U, hot: false };
        g.motion.insert((0, 0, 0), MotionMark::BulgeEndpoint {
            spec, plus: true, phase: EndpointPhase::Ready, age: 0, resume: None,
        });
        g.heat_step();
        assert!(!g.wire((0, 0, 0)).unwrap().iter().next().unwrap().hot);
        g.motion.clear();
        g.heat_step();
        assert!(g.wire((0, 0, 0)).unwrap().iter().next().unwrap().hot);
    }

    #[test]
    fn failed_diagonal_claim_relays_pressure_without_moving_the_blocker() {
        let mut g = Grid::new(Topo::Full3D);
        let corner = (0, 0, 0);
        let spine = (0, 1, 0);
        g.cells.insert(corner, one(Dir::E, Dir::W));
        let spec = BulgeSpec { axis: Dir::N, side: Dir::W, hot: false };
        g.motion.insert(spine, MotionMark::BulgeSpine {
            spec, phase: TargetPhase::Claim, age: 0,
        });
        let before = payload_view(g.cells.get(&corner));
        assert_eq!(local_pressure_source(&hood_at(&g, corner)), 250);
        assert_eq!(payload_view(g.cells.get(&corner)), before);

        g.chi.insert(corner, 199);
        assert!(bulge_offer(&hood_at(&g, corner)).is_none(),
            "ordinary halo pressure must not grow count-one wires");
        g.motion.clear();
        g.chi.insert(corner, 250);
        assert!(bulge_offer(&hood_at(&g, corner)).is_some(),
            "a saturated blocker owns its pressure response");

    }

    #[test]
    fn remote_state_cannot_change_a_center_decision() {
        let (g1, _) = stem_fixture(Topo::Full3D);
        let (mut g2, _) = stem_fixture(Topo::Full3D);
        g2.cells.insert((100, 100, 100), one(Dir::N, Dir::S));
        g2.chi.insert((-100, 20, 70), 255);
        let p = (0, 0, 0);
        assert_eq!(hood_at(&g1, p), hood_at(&g2, p));
        assert_eq!(next_site(&hood_at(&g1, p)), next_site(&hood_at(&g2, p)));
    }
}
