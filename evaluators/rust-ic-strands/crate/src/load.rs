//! The net a run starts from: the loaded term built as agents and wrapped for normalization.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::Term;
use rust_ca_lattice::sup::STerm;

/// The term built and wrapped for normalization (`Net::drive`): the net and its root `Out`.
pub fn net(t: &Term) -> (Net, u32) {
    let mut net = Net::new();
    let root = net.build(t);
    let (_, out) = net.drive(root);
    (net, out)
}

/// The net a run with these settings starts from: `net_sup`, or with `Params::views` its code in
/// movement agents (`Net::build_views`), which takes no superpositions.
pub fn net_for(t: &STerm, p: &crate::lattice::Params) -> Result<(Net, u32), String> {
    if !p.views { return Ok(net_sup(t)); }
    let t = t.plain().ok_or("movement agents run no superpositions")?;
    let mut net = Net::new();
    let root = net.build_views(&t);
    let (_, out) = net.drive(root);
    Ok((net, out))
}

/// `net` for a term that may hold superpositions.
pub fn net_sup(t: &STerm) -> (Net, u32) {
    if let Some(t) = t.plain() { return net(&t); }
    let mut net = Net::new();
    let root = rust_ca_lattice::sup::build(&mut net, t);
    let (_, out) = net.drive(root);
    (net, out)
}
