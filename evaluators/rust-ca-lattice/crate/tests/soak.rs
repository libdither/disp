//! Lattice-tier differential soak: random terms run on the grid against the independent
//! oracle. The abstract tier already fuzzes 4000 terms (stage1::differential_4000); this
//! is the same discipline one level down, where embedding, movement, blocklet growth, and
//! relief can wrong an answer the abstract net cannot. The contract is the frontier's:
//! any run may park, none may answer wrongly, and a seed-free quiescent grid must
//! project onto its shadow net.
//!
//! Every run is deterministic (fixed LCG seed), so a failure line names its exact
//! reproduction. Depth (i%4), embedding (i/4 % 2), and discipline (i/8 % 4) rotate on
//! coprime-free phases that form a full 4x2x4 cross every 32 terms — aliasing any two
//! (e.g. depth with discipline) would silently starve a whole stratum of coverage.

use rust_ca_lattice::cascade_run::{
    check_projection, check_reciprocity, load_net, load_net_tree, Discipline, Runner,
};
use rust_ca_lattice::lattice::Topo;
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Lcg};

const TERMS: u32 = 160;
const BUDGET: u64 = 4_000_000;

#[test]
fn soak_random_terms_never_wrong() {
    let mut rng = Lcg(20260730);
    let disciplines = [
        Discipline::Fifo,
        Discipline::Lifo,
        Discipline::Random(0x9e37_79b9_7f4a_7c15),
        Discipline::AddressOrdered,
    ];
    let (mut complete, mut parked, mut skipped) = (0u32, 0u32, 0u32);
    for i in 0..TERMS {
        let term = rng.rand_term(3 + (i % 4));
        // Oracle divergence / fuel exhaustion is not a differential point.
        let want = match oracle::nf(term.clone(), &mut Fuel(5_000)) {
            Ok(w) => oracle::show(&w),
            Err(_) => {
                skipped += 1;
                continue;
            }
        };
        let discipline = disciplines[((i / 8) % 4) as usize];
        let (embed, embed_name): (fn(&Net, Topo) -> Result<_, _>, &str) =
            if (i / 4) % 2 == 0 { (load_net, "row") } else { (load_net_tree, "tree") };
        let ctx = || format!("term {i} [{discipline:?}/{embed_name}] {}", oracle::show(&term));

        let mut shadow = Net::new();
        let root = shadow.build(&term);
        let (_nrm, out) = shadow.drive(root);
        let grid = embed(&shadow, Topo::Full3D).unwrap_or_else(|e| panic!("{}: load: {e}", ctx()));
        let mut r = Runner::new(grid, shadow, discipline);
        assert!(r.run(BUDGET), "{}: did not quiesce", ctx());

        // Geometry only binds at seed-free quiescence (a wedged seed leaves stubs).
        let seed_free = r.grid.seed_sids.is_empty();
        if seed_free {
            check_reciprocity(&r.grid).unwrap_or_else(|e| panic!("{}: reciprocity: {e}", ctx()));
            check_projection(&r.grid, &r.shadow)
                .unwrap_or_else(|e| panic!("{}: projection: {e}", ctx()));
        }

        // readback is Some only for a whole constructor tree, i.e. an answer; any answer
        // must be the oracle's.
        match r.shadow.readback(r.shadow.get(out).ports[0]) {
            Some(got) => {
                assert_eq!(oracle::show(&got), want, "{}: WRONG ANSWER", ctx());
                if seed_free {
                    complete += 1;
                } else {
                    parked += 1;
                }
            }
            None => parked += 1,
        }
    }
    println!("soak: {complete} complete, {parked} parked, {skipped} skipped of {TERMS}");
    // Aggregate floor, pinned conservatively below measured. 2026-08-01, in order:
    // 99 before endpoint swings, 107 with them, 112 once relief could pay its way past
    // the displacement order, 129 once a declined dock's last blocker could pay too and
    // the ring-clearing bound came off, 130 once a walker could exchange places with a
    // stationary guest. Chaotic-margin discipline: robust aggregates only, schedule
    // jitter must not flap the pin. Raise it as relief rungs land; never hand-tune down.
    assert!(complete >= 123, "soak completion floor: {complete}");
}
