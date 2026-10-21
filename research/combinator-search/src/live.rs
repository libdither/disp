//! Running an interaction system: a net of agents in an arena, reduced a round at a time (every active
//! pair at once), counting what a run costs: interactions, rounds, peak agents, eraser interactions
//! (heat) and footprint marks (what the run keeps so that each step can be undone).

/// A small net with free ports, a rule's right side or a gadget: ports 0..free are its free ports (for a
/// rule, the first agent's auxiliary ports, then the second's), then each agent's ports in turn,
/// principal first.
#[derive(Clone, Debug)]
pub struct Piece {
    pub kinds: Vec<u8>,
    pub partner: Vec<u8>,
    pub free: usize,
}

/// An interface port: an agent with one port, in no rule.
pub const FREE_KIND: u8 = u8::MAX;
const DEAD: u8 = u8::MAX - 1;
const STRIDE: u32 = 4;
pub const NONE: u32 = u32::MAX;

pub struct System {
    pub arity: Vec<u8>,
    pub eraser: Option<u8>,
    /// The rule for kinds a ≤ b, at `pair(a, b)`; None where they do not interact.
    pub rules: Vec<Option<Piece>>,
    /// The marks a step by each rule leaves: one per part of its right side.
    pub marks: Vec<u32>,
}

impl System {
    pub fn pair(&self, a: u8, b: u8) -> usize {
        let (a, b) = if a <= b { (a, b) } else { (b, a) };
        a as usize * self.arity.len() + b as usize
    }
    fn ports(&self, k: u8) -> u32 {
        1 + self.arity[k as usize] as u32
    }
}

#[derive(Default, Clone, Debug)]
pub struct Stats {
    pub interactions: u64,
    pub rounds: u64,
    pub peak: usize,
    pub erasures: u64,
    pub marks: u64,
    pub halted: bool,
    pub too_big: bool,
}

/// Agents in slots of four ports; a dead slot waits in `spare` to be reused. `todo` holds the agents
/// whose principal port may have found a partner since the last round.
#[derive(Clone, Default)]
pub struct Live {
    pub kind: Vec<u8>,
    pub partner: Vec<u32>,
    spare: Vec<u32>,
    pub agents: usize,
    todo: Vec<u32>,
}

pub fn port(s: u32, p: u32) -> u32 {
    s * STRIDE + p
}

impl Live {
    pub fn add(&mut self, k: u8) -> u32 {
        let s = match self.spare.pop() {
            Some(s) => {
                self.kind[s as usize] = k;
                self.partner[(s * STRIDE) as usize..((s + 1) * STRIDE) as usize].fill(NONE);
                s
            }
            None => {
                self.kind.push(k);
                self.partner.extend([NONE; STRIDE as usize]);
                self.kind.len() as u32 - 1
            }
        };
        if k != FREE_KIND {
            self.agents += 1;
            self.todo.push(s);
        }
        s
    }
    fn kill(&mut self, s: u32) {
        self.agents -= 1;
        self.kind[s as usize] = DEAD;
        self.spare.push(s);
    }
    pub fn link(&mut self, p: u32, q: u32) {
        self.partner[p as usize] = q;
        self.partner[q as usize] = p;
        self.todo.push(p / STRIDE);
        self.todo.push(q / STRIDE);
    }
    pub fn is_agent(&self, s: usize) -> bool {
        self.kind[s] != DEAD && self.kind[s] != FREE_KIND
    }

    /// Adds a piece's agents; for each of its free ports, the agent port it leads to (a piece used this
    /// way has no wire from free port to free port).
    pub fn place(&mut self, arity: &[u8], piece: &Piece) -> Vec<u32> {
        let mut port_of = vec![NONE; piece.partner.len()];
        let mut at = piece.free;
        for &k in &piece.kinds {
            let s = self.add(k);
            for p in 0..=arity[k as usize] as usize {
                port_of[at + p] = port(s, p as u32);
            }
            at += 1 + arity[k as usize] as usize;
        }
        for p in piece.free..piece.partner.len() {
            let q = piece.partner[p] as usize;
            if q >= piece.free && p < q {
                self.link(port_of[p], port_of[q]);
            }
        }
        (0..piece.free).map(|f| port_of[piece.partner[f] as usize]).collect()
    }

    /// An interface port wired to `p`.
    pub fn tap(&mut self, p: u32) -> u32 {
        let s = self.add(FREE_KIND);
        self.link(port(s, 0), p);
        s
    }
}

/// Replaces the active pair x ⋈ y by its rule's right side.
fn interact(sys: &System, net: &mut Live, x: u32, y: u32, st: &mut Stats) {
    let (x, y) = if net.kind[x as usize] <= net.kind[y as usize] { (x, y) } else { (y, x) };
    let (kx, ky) = (net.kind[x as usize], net.kind[y as usize]);
    let idx = sys.pair(kx, ky);
    let rule = sys.rules[idx].as_ref().unwrap();
    let ax = sys.arity[kx as usize] as usize;
    let f = rule.free;
    let mut dying = [NONE; 6];
    let mut outside = [NONE; 6];
    for r in 0..f {
        dying[r] = if r < ax { port(x, 1 + r as u32) } else { port(y, 1 + (r - ax) as u32) };
        outside[r] = net.partner[dying[r] as usize];
    }
    let dying_at = |p: u32| dying[..f].iter().position(|&d| d == p);
    let mut port_of = [NONE; 32];
    let mut at = f;
    for &k in &rule.kinds {
        let s = net.add(k);
        for p in 0..sys.ports(k) {
            port_of[at + p as usize] = port(s, p);
        }
        at += sys.ports(k) as usize;
    }
    let mut links: Vec<(u32, u32)> = Vec::with_capacity(rule.partner.len());
    for p in f..rule.partner.len() {
        let q = rule.partner[p] as usize;
        if q >= f && p < q {
            links.push((port_of[p], port_of[q]));
        }
    }
    // a wire through the pair's ports runs alternately outside the pair and inside the right side, until
    // it reaches a port outside or an agent of the right side; first those with an end outside
    let mut used = [false; 6];
    for r in 0..f {
        if used[r] || dying_at(outside[r]).is_some() {
            continue;
        }
        let mut cur = r;
        let end = loop {
            used[cur] = true;
            let q = rule.partner[cur] as usize;
            if q >= f {
                break port_of[q];
            }
            used[q] = true;
            match dying_at(outside[q]) {
                None => break outside[q],
                Some(r2) => cur = r2,
            }
        };
        links.push((outside[r], end));
    }
    // then those running from an agent of the right side back to one
    for p in f..rule.partner.len() {
        let q = rule.partner[p] as usize;
        if q >= f || used[q] {
            continue;
        }
        let mut r = q;
        let end = loop {
            used[r] = true;
            let r2 = dying_at(outside[r]).unwrap();
            used[r2] = true;
            let q2 = rule.partner[r2] as usize;
            if q2 >= f {
                break port_of[q2];
            }
            r = q2;
        };
        links.push((port_of[p], end));
    }
    net.kill(x);
    net.kill(y);
    for (a, b) in links {
        net.link(a, b);
    }
    st.interactions += 1;
    if Some(kx) == sys.eraser || Some(ky) == sys.eraser {
        st.erasures += 1;
    }
    st.marks += sys.marks[idx] as u64;
}

/// Reduces a round at a time until no pair can interact, or past the caps.
pub fn reduce(sys: &System, net: &mut Live, max_interactions: u64, max_agents: usize) -> Stats {
    let mut st = Stats { peak: net.agents, ..Default::default() };
    let mut pairs: Vec<(u32, u32)> = vec![];
    loop {
        pairs.clear();
        for s in std::mem::take(&mut net.todo) {
            if !net.is_agent(s as usize) {
                continue;
            }
            let q = net.partner[port(s, 0) as usize];
            if q == NONE || q % STRIDE != 0 {
                continue;
            }
            let t = q / STRIDE;
            if net.is_agent(t as usize) && sys.rules[sys.pair(net.kind[s as usize], net.kind[t as usize])].is_some() {
                pairs.push((s.min(t), s.max(t)));
            }
        }
        pairs.sort_unstable();
        pairs.dedup();
        if pairs.is_empty() {
            st.halted = true;
            return st;
        }
        for &(s, t) in &pairs {
            interact(sys, net, s, t, &mut st);
        }
        st.rounds += 1;
        st.peak = st.peak.max(net.agents);
        if net.agents > max_agents {
            st.too_big = true;
            return st;
        }
        if st.interactions >= max_interactions {
            return st;
        }
    }
}

/// Walks the net from `starts`, numbering agents as they are met: each agent's ports name their
/// partners by kind, port and number.
fn walk(net: &Live, starts: &[u32], num: &mut [u32], order: &mut Vec<u32>, out: &mut Vec<u32>) {
    order.clear();
    for &s in starts {
        num[s as usize] = order.len() as u32;
        order.push(s);
        out.push(net.kind[s as usize] as u32);
    }
    let ports = |k: u8| if k == FREE_KIND { 1 } else { STRIDE };
    let mut i = 0;
    while i < order.len() {
        let t = order[i];
        i += 1;
        for p in 0..ports(net.kind[t as usize]) {
            let q = net.partner[port(t, p) as usize];
            if q == NONE {
                out.push(NONE);
                continue;
            }
            let u = q / STRIDE;
            if num[u as usize] == NONE {
                num[u as usize] = order.len() as u32;
                order.push(u);
            }
            out.extend([net.kind[u as usize] as u32, q % STRIDE, num[u as usize]]);
        }
    }
}

/// The net up to renaming its agents: walked from the interface ports in order, then each part they do
/// not reach, written from whichever of its agents gives the least walk, the parts sorted.
pub fn canon(net: &Live, iface: &[u32]) -> Vec<u32> {
    let n = net.kind.len();
    let mut num = vec![NONE; n];
    let mut order = vec![];
    let mut out = vec![];
    walk(net, iface, &mut num, &mut order, &mut out);
    let mut parts: Vec<Vec<u32>> = vec![];
    for s in 0..n {
        if num[s] != NONE || !net.is_agent(s) {
            continue;
        }
        let mut best = vec![];
        walk(net, &[s as u32], &mut num, &mut order, &mut best);
        let comp = order.clone();
        for &a in &comp[1..] {
            let (mut num2, mut ord2, mut o2) = (vec![NONE; n], vec![], vec![]);
            walk(net, &[a], &mut num2, &mut ord2, &mut o2);
            if o2 < best {
                best = o2;
            }
        }
        parts.push(best);
    }
    parts.sort();
    for p in parts {
        out.push(NONE - 1);
        out.extend(p);
    }
    out
}
