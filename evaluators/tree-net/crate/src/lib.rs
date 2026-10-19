//! disp's tree calculus as an interaction net, shared by the evaluators that run it: the agents and
//! their rules ([`rules`], checked by `rules::validate`), the abstract net that fires them ([`net`]), an
//! independent recursive reducer to check answers against ([`oracle`]), terms with superpositions
//! ([`sup`]), reading terms and the benchmark workloads ([`term`]), and which ports are outputs
//! ([`polarity`]).

pub mod rules;
pub mod net;
pub mod oracle;
pub mod sup;
pub mod term;
pub mod polarity;
