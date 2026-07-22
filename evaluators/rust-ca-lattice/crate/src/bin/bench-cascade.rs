//! Timing snapshot for the cascade drivers. Usage: cargo run --release --bin bench-cascade

use rust_ca_lattice::cascade::Grid2;
use rust_ca_lattice::cascade_gather::GatherGrid;
use rust_ca_lattice::cascade_par::{run_movement, AtomicGrid};
use rust_ca_lattice::cascade_run::{lay_wire, load_net, place_agent, Discipline, Runner};
use rust_ca_lattice::lattice::Dir;
use rust_ca_lattice::lattice::{Pos, Topo};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{ap, Term};
use rust_ca_lattice::rules::Tag;
use std::sync::atomic::Ordering;
use std::time::Instant;

fn lanes(count: i32, length: i32) -> Grid2 {
    let mut grid = Grid2::new(Topo::Full3D);
    let mut shadow = Net::new();
    for lane in 0..count {
        let y = lane * 3;
        let l = place_agent(&mut grid, &mut shadow, (0, y, 0), Tag::L, Dir::E, None);
        let e = place_agent(&mut grid, &mut shadow, (length, y, 0), Tag::Eps, Dir::W, None);
        shadow.link(l.id, 0, e.id, 0);
        lay_wire(&mut grid, &(0..=length).map(|x| (x, y, 0)).collect::<Vec<Pos>>());
    }
    grid
}

fn main() {
    // Serial end-to-end reduction.
    {
        let term = ap(Term::L, Term::L);
        let mut shadow = Net::new();
        let root = shadow.build(&term);
        let (_n, _o) = shadow.drive(root);
        let grid = load_net(&shadow, Topo::Full3D).unwrap();
        let t0 = Instant::now();
        let mut r = Runner::new(grid, shadow, Discipline::Fifo);
        let quiet = r.run(10_000_000);
        let dt = t0.elapsed();
        println!(
            "serial identity: quiet {quiet}, {} rewrites, {} transport, {} generations in {:?}",
            r.grid.rewrites, r.grid.transport, r.generation, dt
        );
    }

    // Parallel movement layer: many disjoint erasure cascades.
    let (count, length) = (256, 48);
    for threads in [1, 2, 4, 8] {
        let grid = lanes(count, length);
        let seeds: Vec<Pos> = grid.cells.keys().copied().collect();
        let agrid = AtomicGrid::from_grid(&grid, 8);
        let t0 = Instant::now();
        let stats = run_movement(&agrid, seeds, threads);
        let dt = t0.elapsed();
        let commits = stats.commits.load(Ordering::Relaxed);
        println!(
            "parallel {count}x{length} lanes, {threads} threads: {} commits, {} conflicts, {} fires in {:?} ({:.1}k commits/s)",
            commits,
            stats.conflicts.load(Ordering::Relaxed),
            stats.fires.load(Ordering::Relaxed),
            dt,
            commits as f64 / dt.as_secs_f64() / 1e3,
        );
    }

    // Gather (GPU-form) driver on the same fixture.
    {
        let grid = lanes(count, length);
        let mut g = GatherGrid::from_grid(&grid);
        let t0 = Instant::now();
        let quiet = g.run(100_000);
        let dt = t0.elapsed();
        println!(
            "gather {count}x{length} lanes: quiet {quiet}, {} fires, {} moves in {:?}",
            g.fires, g.moves, dt
        );
    }
}
