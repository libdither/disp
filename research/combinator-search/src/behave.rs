//! Stage 2 of research/COMBINATOR_SEARCH_PLAN.md: how a system behaves on random closed nets.

use crate::live::{canon, port, reduce, Live, System};
use crate::space::Sig;
use std::collections::HashSet;

pub struct Rng(pub u64);

impl Rng {
    pub fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }
    pub fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
}

/// `n` agents wired at random: one in ten an eraser, the rest of the other kinds evenly; one more eraser
/// if the ports do not pair up.
pub fn random_net(sig: &Sig, n: usize, rng: &mut Rng) -> Live {
    let mut net = Live::default();
    let mut ports = vec![];
    let others = sig.kinds() as usize - sig.eraser.is_some() as usize;
    for _ in 0..n {
        let k = match sig.eraser {
            Some(e) if rng.below(10) == 0 => e,
            _ => rng.below(others) as u8,
        };
        let s = net.add(k);
        ports.extend((0..sig.ports(k) as u32).map(|p| port(s, p)));
    }
    if ports.len() % 2 == 1 {
        let s = net.add(sig.eraser.unwrap());
        ports.push(port(s, 0));
    }
    for i in (1..ports.len()).rev() {
        ports.swap(i, rng.below(i + 1));
    }
    for w in ports.chunks(2) {
        net.link(w[0], w[1]);
    }
    net
}

#[derive(Default, Debug, Clone)]
pub struct Behaviour {
    pub nets: usize,
    pub halted: usize,
    pub blew_up: usize,
    /// Over the runs that halted: interactions and rounds per loaded agent, peak and final agents per
    /// loaded agent, eraser interactions and marks per interaction.
    pub interactions: f64,
    pub rounds: f64,
    pub peak: f64,
    pub left: f64,
    pub erasures: f64,
    pub marks: f64,
    /// Different normal forms among the runs that halted.
    pub forms: usize,
}

pub fn behave(sys: &System, sig: &Sig, n: usize, nets: usize, seed: u64) -> Behaviour {
    let mut rng = Rng(seed | 1);
    let mut b = Behaviour { nets, ..Default::default() };
    let mut forms = HashSet::new();
    let (mut steps, mut marks, mut erasures) = (0u64, 0u64, 0u64);
    for _ in 0..nets {
        let mut net = random_net(sig, n, &mut rng);
        let loaded = net.agents as f64;
        let st = reduce(sys, &mut net, 200 * n as u64, 50 * n);
        if st.too_big {
            b.blew_up += 1;
        }
        if !st.halted {
            continue;
        }
        b.halted += 1;
        b.interactions += st.interactions as f64 / loaded;
        b.rounds += st.rounds as f64 / loaded;
        b.peak += st.peak as f64 / loaded;
        b.left += net.agents as f64 / loaded;
        steps += st.interactions;
        marks += st.marks;
        erasures += st.erasures;
        if net.agents <= 200 {
            forms.insert(canon(&net, &[]));
        }
    }
    let h = b.halted.max(1) as f64;
    b.interactions /= h;
    b.rounds /= h;
    b.peak /= h;
    b.left /= h;
    b.erasures = erasures as f64 / steps.max(1) as f64;
    b.marks = marks as f64 / steps.max(1) as f64;
    b.forms = forms.len();
    b
}
