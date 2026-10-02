//! One-call driver: lay a term out on a mesh, run it, and say what happened.

use crate::mesh::{Config, Mesh, Outcome};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Term};

/// Build the abstract net for `term` (applications as suspensions, a normalizer driving
/// the root, `Out` holding the answer) and lay it out on a fresh mesh.
pub fn load(term: &Term, cfg: Config, check: bool) -> Result<Mesh, String> {
    let mut net = Net::new();
    let root = net.build(term);
    let (_nrm, out) = net.drive(root);
    Mesh::load(cfg, net, out, check)
}

pub struct Report {
    pub outcome: &'static str,
    pub answer: Option<String>,
    pub mesh: Mesh,
}

pub fn run(term: &Term, cfg: Config, check: bool, max_ticks: u64) -> Result<Report, String> {
    run_with(term, cfg, check, max_ticks, |_| {})
}

/// `run`, with a chance to adjust the machine before it starts.
pub fn run_with(term: &Term, cfg: Config, check: bool, max_ticks: u64, setup: impl FnOnce(&mut Mesh)) -> Result<Report, String> {
    let mut mesh = load(term, cfg, check)?;
    setup(&mut mesh);
    let outcome = mesh.run(max_ticks);
    if check && outcome == Outcome::Done {
        mesh.check_projection()?;
    }
    let outcome = outcome.name();
    let answer = mesh.readback().map(|t| oracle::show(&t));
    Ok(Report { outcome, answer, mesh })
}
