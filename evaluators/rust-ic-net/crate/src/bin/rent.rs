// rent — the E1 analyzer (research/interaction-combinator/SPATIAL_IC.md §10, rung C).
//
// Reads a rung C interaction-DAG event log (trace.rs format), builds the undirected
// dependency graph over interactions, recursively bisects it (multilevel: heavy-edge
// matching + BFS seed + FM refinement), and fits the Rent exponent p: at every recursion
// level, for every block, T = edges of the FULL graph with exactly one endpoint in the
// block (all external connections, the Landman–Russo count), g = block size; p is the OLS
// slope of log2(mean T) against log2(mean g) across levels inside the fit window. Also
// reports work (events), span (longest path — ids arrive in topological order), the
// parallelism profile, and the traffic bisection (top-level cut / span, the bandwidth per
// unit depth a mesh must supply).
//
//   rent <trace.bin> [--min-g N] [--fit-lo G] [--seed S] [--json PATH]
//   rent --synthetic grid2:512|grid3:64|tree:22|rand3:200000|chain:100000 [--seed S]
//
// The synthetic mode is the calibration harness: the partitioner is trusted only because
// grid2 fits ≈0.5, grid3 ≈2/3, tree/chain ≈ low, rand3 (an expander) ≈ 1. Run it after
// any change here. Edges to id 0 (the initial net / loader) are counted separately as
// primary inputs, not partitioned. Dependencies are deduped pairs (a directed dependency
// exists or not); every in-edge satisfies i < j or the loader is refusing the file.

use std::fs::File;
use std::io::{BufReader, Read, Write};

// ── deterministic PRNG (splitmix64) ─────────────────────────────────────────
struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E3779B97F4A7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
        z ^ (z >> 31)
    }
    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
    fn shuffle(&mut self, v: &mut [u32]) {
        for i in (1..v.len()).rev() {
            v.swap(i, self.below(i + 1));
        }
    }
}

// ── global graph (CSR over deduped undirected edges) ────────────────────────
// u32 offsets: 2E must fit in u32 (guarded) — halves the index memory at E1 scale.
struct Csr {
    xadj: Vec<u32>,
    adj: Vec<u32>,
}
impl Csr {
    /// Consumes the edge list — the peak holds either the sorted pairs or the CSR, not both
    /// plus a clone (the 69M-event fib-14 DAG OOM'd a 30 GB box through that clone).
    fn build(nv: usize, mut edges: Vec<(u32, u32)>) -> Csr {
        for e in edges.iter_mut() {
            if e.0 > e.1 {
                *e = (e.1, e.0);
            }
        }
        edges.sort_unstable();
        edges.dedup();
        assert!(edges.len() * 2 < u32::MAX as usize, "rent: graph too large for u32 CSR");
        let mut deg = vec![0u32; nv + 1];
        for &(a, b) in edges.iter() {
            deg[a as usize + 1] += 1;
            deg[b as usize + 1] += 1;
        }
        for i in 0..nv {
            deg[i + 1] += deg[i];
        }
        let xadj = deg;
        let mut fill = xadj.clone();
        let mut adj = vec![0u32; xadj[nv] as usize];
        for &(a, b) in edges.iter() {
            adj[fill[a as usize] as usize] = b;
            fill[a as usize] += 1;
            adj[fill[b as usize] as usize] = a;
            fill[b as usize] += 1;
        }
        Csr { xadj, adj }
    }
    #[inline]
    fn nbrs(&self, v: u32) -> &[u32] {
        &self.adj[self.xadj[v as usize] as usize..self.xadj[v as usize + 1] as usize]
    }
    fn nv(&self) -> usize {
        self.xadj.len() - 1
    }
    fn ne(&self) -> u64 {
        self.xadj[self.nv()] as u64 / 2
    }
}

// ── local multilevel bisection ───────────────────────────────────────────────
// One level of the induced local graph: CSR + edge weights + vertex weights.
struct Lg {
    xadj: Vec<u32>,
    adj: Vec<u32>,
    w: Vec<u32>,  // edge weights (parallel to adj)
    vw: Vec<u32>, // vertex weights
    total_vw: u64,
}
impl Lg {
    fn nv(&self) -> usize {
        self.vw.len()
    }
}

/// Coarsen by random-order heavy-edge matching. Returns (coarse graph, fine->coarse map).
fn coarsen(g: &Lg, rng: &mut Rng) -> (Lg, Vec<u32>) {
    let n = g.nv();
    let mut order: Vec<u32> = (0..n as u32).collect();
    rng.shuffle(&mut order);
    const UNMATCHED: u32 = u32::MAX;
    let mut mate = vec![UNMATCHED; n];
    let mut cmap = vec![0u32; n];
    let mut nc = 0u32;
    for &v in &order {
        if mate[v as usize] != UNMATCHED {
            continue;
        }
        let (mut best, mut best_w) = (v, 0u32);
        let (s, e) = (g.xadj[v as usize] as usize, g.xadj[v as usize + 1] as usize);
        for k in s..e {
            let u = g.adj[k];
            if mate[u as usize] == UNMATCHED && u != v && g.w[k] > best_w {
                best = u;
                best_w = g.w[k];
            }
        }
        mate[v as usize] = best;
        mate[best as usize] = v;
        cmap[v as usize] = nc;
        cmap[best as usize] = nc;
        nc += 1;
    }
    // Build the coarse graph with a dense scratch map over coarse ids.
    let mut cxadj = vec![0u32];
    let mut cadj: Vec<u32> = Vec::new();
    let mut cw: Vec<u32> = Vec::new();
    let mut cvw = vec![0u32; nc as usize];
    let mut slot = vec![u32::MAX; nc as usize]; // coarse nbr -> index in cadj for current cv
    // members of each coarse vertex (at most 2)
    let mut members: Vec<[u32; 2]> = vec![[UNMATCHED; 2]; nc as usize];
    for v in 0..n as u32 {
        let c = cmap[v as usize] as usize;
        if members[c][0] == UNMATCHED {
            members[c][0] = v;
        } else {
            members[c][1] = v;
        }
        cvw[c] += g.vw[v as usize];
    }
    for c in 0..nc {
        let row_start = cadj.len();
        for &m in members[c as usize].iter() {
            if m == UNMATCHED {
                continue;
            }
            let (s, e) = (g.xadj[m as usize] as usize, g.xadj[m as usize + 1] as usize);
            for k in s..e {
                let cu = cmap[g.adj[k] as usize];
                if cu == c {
                    continue; // internal edge collapses
                }
                if (slot[cu as usize] as usize) >= row_start && (slot[cu as usize] as usize) < cadj.len() && cadj[slot[cu as usize] as usize] == cu {
                    cw[slot[cu as usize] as usize] += g.w[k];
                } else {
                    slot[cu as usize] = cadj.len() as u32;
                    cadj.push(cu);
                    cw.push(g.w[k]);
                }
            }
        }
        cxadj.push(cadj.len() as u32);
    }
    let total_vw = g.total_vw;
    (
        Lg { xadj: cxadj, adj: cadj, w: cw, vw: cvw, total_vw },
        cmap,
    )
}

/// Initial side assignment on the coarsest graph: BFS-grow side 0 from a pseudo-peripheral
/// vertex until it holds half the weight (continuing from unvisited vertices when the
/// frontier empties, so disconnected graphs still fill).
fn initial_partition(g: &Lg, rng: &mut Rng) -> Vec<u8> {
    let n = g.nv();
    let mut side = vec![1u8; n];
    if n == 0 {
        return side;
    }
    // pseudo-peripheral: BFS twice from a random start, take the last-reached vertex.
    let mut far = rng.below(n) as u32;
    for _ in 0..2 {
        let mut seen = vec![false; n];
        let mut q = std::collections::VecDeque::new();
        seen[far as usize] = true;
        q.push_back(far);
        while let Some(v) = q.pop_front() {
            far = v;
            let (s, e) = (g.xadj[v as usize] as usize, g.xadj[v as usize + 1] as usize);
            for k in s..e {
                let u = g.adj[k];
                if !seen[u as usize] {
                    seen[u as usize] = true;
                    q.push_back(u);
                }
            }
        }
    }
    let half = g.total_vw / 2;
    let mut grown = 0u64;
    let mut seen = vec![false; n];
    let mut q = std::collections::VecDeque::new();
    seen[far as usize] = true;
    q.push_back(far);
    let mut next_unseen = 0usize;
    while grown < half {
        let v = match q.pop_front() {
            Some(v) => v,
            None => {
                while next_unseen < n && seen[next_unseen] {
                    next_unseen += 1;
                }
                if next_unseen >= n {
                    break;
                }
                seen[next_unseen] = true;
                next_unseen as u32
            }
        };
        side[v as usize] = 0;
        grown += g.vw[v as usize] as u64;
        let (s, e) = (g.xadj[v as usize] as usize, g.xadj[v as usize + 1] as usize);
        for k in s..e {
            let u = g.adj[k];
            if !seen[u as usize] {
                seen[u as usize] = true;
                q.push_back(u);
            }
        }
    }
    side
}

/// One FM pass with rollback to the best-seen prefix. Returns the improved cut.
fn fm_pass(g: &Lg, side: &mut [u8], cut0: i64, rng: &mut Rng) -> i64 {
    let n = g.nv();
    let total = g.total_vw;
    let lo = (total as f64 * 0.45) as u64;
    let mut wt = [0u64; 2];
    for v in 0..n {
        wt[side[v] as usize] += g.vw[v] as u64;
    }
    // gain(v) = external weight − internal weight
    let gain = |g: &Lg, side: &[u8], v: usize| -> i64 {
        let (s, e) = (g.xadj[v] as usize, g.xadj[v + 1] as usize);
        let mut ext = 0i64;
        for k in s..e {
            if side[g.adj[k] as usize] != side[v] {
                ext += g.w[k] as i64;
            } else {
                ext -= g.w[k] as i64;
            }
        }
        ext
    };
    // max-heap with lazy invalidation
    let mut heap: std::collections::BinaryHeap<(i64, u32)> = std::collections::BinaryHeap::new();
    let mut order: Vec<u32> = (0..n as u32).collect();
    rng.shuffle(&mut order);
    for &v in &order {
        heap.push((gain(g, side, v as usize), v));
    }
    let mut locked = vec![false; n];
    let mut cut = cut0;
    let mut best_cut = cut0;
    let mut moves: Vec<u32> = Vec::new();
    let mut best_len = 0usize;
    while let Some((gv, v)) = heap.pop() {
        let vu = v as usize;
        if locked[vu] {
            continue;
        }
        let cur = gain(g, side, vu);
        if cur != gv {
            heap.push((cur, v)); // stale entry; reinsert with fresh gain
            continue;
        }
        let from = side[vu] as usize;
        if wt[from] < lo + g.vw[vu] as u64 {
            continue; // move would break balance; leave locked-out (do not lock others)
        }
        // tentative move
        locked[vu] = true;
        side[vu] = 1 - side[vu];
        wt[from] -= g.vw[vu] as u64;
        wt[1 - from] += g.vw[vu] as u64;
        cut -= cur;
        moves.push(v);
        if cut < best_cut {
            best_cut = cut;
            best_len = moves.len();
        }
        let (s, e) = (g.xadj[vu] as usize, g.xadj[vu + 1] as usize);
        for k in s..e {
            let u = g.adj[k];
            if !locked[u as usize] {
                heap.push((gain(g, side, u as usize), u));
            }
        }
    }
    // roll back the tail beyond the best prefix
    for &v in &moves[best_len..] {
        side[v as usize] = 1 - side[v as usize];
    }
    best_cut
}

fn cut_of(g: &Lg, side: &[u8]) -> i64 {
    let mut cut = 0i64;
    for v in 0..g.nv() {
        let (s, e) = (g.xadj[v] as usize, g.xadj[v + 1] as usize);
        for k in s..e {
            let u = g.adj[k] as usize;
            if u > v && side[u] != side[v] {
                cut += g.w[k] as i64;
            }
        }
    }
    cut
}

/// Multilevel bisection of a local graph. Returns side per local vertex.
fn bisect(g0: Lg, rng: &mut Rng) -> Vec<u8> {
    let mut levels: Vec<Lg> = vec![g0];
    let mut maps: Vec<Vec<u32>> = Vec::new();
    loop {
        let top = levels.last().unwrap();
        if top.nv() <= 64 {
            break;
        }
        let (coarse, cmap) = coarsen(top, rng);
        if coarse.nv() as f64 > top.nv() as f64 * 0.95 {
            break; // matching stalled (star-ish graph); refine from here
        }
        maps.push(cmap);
        levels.push(coarse);
    }
    let mut side = initial_partition(levels.last().unwrap(), rng);
    let mut cut = cut_of(levels.last().unwrap(), &side);
    for _ in 0..3 {
        let c2 = fm_pass(levels.last().unwrap(), &mut side, cut, rng);
        if c2 >= cut {
            break;
        }
        cut = c2;
    }
    // uncoarsen + refine
    for li in (0..maps.len()).rev() {
        let fine = &levels[li];
        let cmap = &maps[li];
        let mut fside = vec![0u8; fine.nv()];
        for v in 0..fine.nv() {
            fside[v] = side[cmap[v] as usize];
        }
        side = fside;
        let mut fcut = cut_of(fine, &side);
        for _ in 0..2 {
            let c2 = fm_pass(fine, &mut side, fcut, rng);
            if c2 >= fcut {
                break;
            }
            fcut = c2;
        }
        let _ = fcut;
    }
    side
}

// ── recursive Rent sampling over the global graph ────────────────────────────
struct Sample {
    level: u32,
    g: u64,
    t: u64,
}

/// Extract the induced local graph of `verts` (unit weights, unit edge weights).
fn induced(csr: &Csr, verts: &[u32], lid: &mut [u32], stamp: &mut [u32], epoch: u32) -> Lg {
    for (i, &v) in verts.iter().enumerate() {
        lid[v as usize] = i as u32;
        stamp[v as usize] = epoch;
    }
    let mut xadj = vec![0u32];
    let mut adj: Vec<u32> = Vec::new();
    for &v in verts {
        for &u in csr.nbrs(v) {
            if stamp[u as usize] == epoch && u != v {
                adj.push(lid[u as usize]);
            }
        }
        xadj.push(adj.len() as u32);
    }
    let w = vec![1u32; adj.len()];
    let n = verts.len();
    Lg { xadj, adj, w, vw: vec![1u32; n], total_vw: n as u64 }
}

/// T of a block = edges with exactly one endpoint inside (block membership via stamp).
fn external_edges(csr: &Csr, verts: &[u32], stamp: &[u32], epoch: u32) -> u64 {
    let mut t = 0u64;
    for &v in verts {
        for &u in csr.nbrs(v) {
            if stamp[u as usize] != epoch {
                t += 1;
            }
        }
    }
    t
}

fn rent_samples(csr: &Csr, min_g: u64, rng: &mut Rng) -> (Vec<Sample>, u64) {
    let nv = csr.nv();
    let mut lid = vec![0u32; nv];
    let mut stamp = vec![0u32; nv];
    let mut epoch = 0u32;
    let mut samples: Vec<Sample> = Vec::new();
    let mut queue: Vec<(u32, Vec<u32>)> = vec![(0, (0..nv as u32).collect())];
    let mut top_cut: u64 = 0;
    while let Some((level, verts)) = queue.pop() {
        if (verts.len() as u64) < min_g * 2 {
            continue;
        }
        epoch += 1;
        let g = induced(csr, &verts, &mut lid, &mut stamp, epoch);
        let side = bisect(g, rng);
        let mut a: Vec<u32> = Vec::new();
        let mut b: Vec<u32> = Vec::new();
        for (i, &v) in verts.iter().enumerate() {
            if side[i] == 0 {
                a.push(v)
            } else {
                b.push(v)
            }
        }
        if a.is_empty() || b.is_empty() {
            continue; // degenerate (should not happen with balance bounds)
        }
        for half in [&a, &b] {
            epoch += 1;
            for &v in half.iter() {
                stamp[v as usize] = epoch;
            }
            let t = external_edges(csr, half, &stamp, epoch);
            if level == 0 && top_cut == 0 {
                // top-level cut = crossing edges = T(half) when the block is half the graph
                top_cut = t;
            }
            samples.push(Sample { level, g: half.len() as u64, t });
        }
        queue.push((level + 1, a));
        queue.push((level + 1, b));
    }
    (samples, top_cut)
}

// ── fit ──────────────────────────────────────────────────────────────────────
struct LevelRow {
    level: u32,
    blocks: u64,
    mean_g: f64,
    mean_t: f64,
}

fn level_table(samples: &[Sample]) -> Vec<LevelRow> {
    let max_level = samples.iter().map(|s| s.level).max().unwrap_or(0);
    let mut rows = Vec::new();
    for l in 0..=max_level {
        let blocks: Vec<&Sample> = samples.iter().filter(|s| s.level == l).collect();
        if blocks.is_empty() {
            continue;
        }
        let n = blocks.len() as f64;
        rows.push(LevelRow {
            level: l,
            blocks: blocks.len() as u64,
            mean_g: blocks.iter().map(|s| s.g as f64).sum::<f64>() / n,
            mean_t: blocks.iter().map(|s| s.t as f64).sum::<f64>() / n,
        });
    }
    rows
}

/// OLS slope of log2(mean_t) vs log2(mean_g) over rows with fit_lo <= mean_g <= fit_hi.
fn fit_p(rows: &[LevelRow], fit_lo: f64, fit_hi: f64) -> Option<(f64, f64, usize)> {
    let pts: Vec<(f64, f64)> = rows
        .iter()
        .filter(|r| r.mean_g >= fit_lo && r.mean_g <= fit_hi && r.mean_t > 0.0)
        .map(|r| (r.mean_g.log2(), r.mean_t.log2()))
        .collect();
    if pts.len() < 3 {
        return None;
    }
    let n = pts.len() as f64;
    let (sx, sy): (f64, f64) = pts.iter().fold((0.0, 0.0), |(a, b), (x, y)| (a + x, b + y));
    let (mx, my) = (sx / n, sy / n);
    let sxx: f64 = pts.iter().map(|(x, _)| (x - mx) * (x - mx)).sum();
    let sxy: f64 = pts.iter().map(|(x, y)| (x - mx) * (y - my)).sum();
    let slope = sxy / sxx;
    let syy: f64 = pts.iter().map(|(_, y)| (y - my) * (y - my)).sum();
    let r2 = if syy == 0.0 { 1.0 } else { (sxy * sxy) / (sxx * syy) };
    Some((slope, r2, pts.len()))
}

// ── trace loading ────────────────────────────────────────────────────────────
struct Loaded {
    nv: usize, // interactions (ids 1..=nv); vertex v = interaction v+1
    edges: Vec<(u32, u32)>,
    init_edges: u64,
    span: u64,
    width_max: u64,
    tag_counts: [u64; 16], // by consumer tag
}

/// Load at most `max_events` events. A topological-order PREFIX of the trace is a
/// downward-closed sub-DAG (every in-edge of a prefix member lands inside the prefix,
/// since edges always point backward), so a capped load is exact for the prefix, not an
/// approximation of the whole; the cap is reported loudly, never silent.
fn load_trace(path: &str, max_events: u32) -> Loaded {
    let f = File::open(path).unwrap_or_else(|e| {
        eprintln!("rent: cannot open {path}: {e}");
        std::process::exit(2);
    });
    let mut r = BufReader::with_capacity(1 << 20, f);
    let mut magic = [0u8; 8];
    r.read_exact(&mut magic).expect("rent: short header");
    assert_eq!(&magic, b"E1DAG1\0\0", "rent: not an E1 trace");
    let mut edges: Vec<(u32, u32)> = Vec::new();
    let mut depth: Vec<u32> = vec![0]; // depth[0] = loader
    let mut width: Vec<u64> = Vec::new();
    let mut init_edges = 0u64;
    let mut tag_counts = [0u64; 16];
    let mut hdr = [0u8; 2];
    let mut id: u32 = 0;
    let mut truncated = false;
    loop {
        match r.read_exact(&mut hdr) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
            Err(e) => panic!("rent: read error: {e}"),
        }
        if id >= max_events {
            truncated = true;
            break;
        }
        id += 1;
        let n = hdr[1] as usize;
        assert!(n <= 4, "rent: bad edge count {n} at event {id}");
        let mut buf = [0u8; 16];
        r.read_exact(&mut buf[..4 * n]).expect("rent: truncated event");
        tag_counts[(hdr[0] >> 4) as usize] += 1;
        let mut d = 0u32;
        for k in 0..n {
            let e = u32::from_le_bytes(buf[4 * k..4 * k + 4].try_into().unwrap());
            assert!(e < id, "rent: edge {e} -> {id} violates topological order");
            if e == 0 {
                init_edges += 1;
            } else {
                edges.push((e - 1, id - 1)); // vertex ids are interaction-1
                d = d.max(depth[e as usize] + 1);
            }
        }
        depth.push(d);
        if d as usize >= width.len() {
            width.resize(d as usize + 1, 0);
        }
        width[d as usize] += 1;
    }
    if truncated {
        println!("NOTE: analyzing the first {id} events only (--max-events); the prefix is a downward-closed sub-DAG");
    }
    let span = width.len() as u64;
    let width_max = width.iter().copied().max().unwrap_or(0);
    Loaded { nv: id as usize, edges, init_edges, span, width_max, tag_counts }
}

// ── synthetic calibration graphs ─────────────────────────────────────────────
fn synth(kind: &str, rng: &mut Rng) -> (usize, Vec<(u32, u32)>) {
    let (name, param) = kind.split_once(':').unwrap_or((kind, ""));
    let p: usize = param.parse().unwrap_or(0);
    let mut edges = Vec::new();
    match name {
        "grid2" => {
            // torus (wrap) so every block is interior — open boundaries depress the
            // measured slope (a Region-II-like finite-size effect), which calibration
            // must not conflate with partitioner error.
            let w = p.max(3);
            let id = |x: usize, y: usize| (y * w + x) as u32;
            for y in 0..w {
                for x in 0..w {
                    edges.push((id(x, y), id((x + 1) % w, y)));
                    edges.push((id(x, y), id(x, (y + 1) % w)));
                }
            }
            (w * w, edges)
        }
        "grid3" => {
            let w = p.max(3);
            let id = |x: usize, y: usize, z: usize| (z * w * w + y * w + x) as u32;
            for z in 0..w {
                for y in 0..w {
                    for x in 0..w {
                        edges.push((id(x, y, z), id((x + 1) % w, y, z)));
                        edges.push((id(x, y, z), id(x, (y + 1) % w, z)));
                        edges.push((id(x, y, z), id(x, y, (z + 1) % w)));
                    }
                }
            }
            (w * w * w, edges)
        }
        "tree" => {
            let n = (1usize << p.max(2)) - 1;
            for v in 1..n {
                edges.push((((v - 1) / 2) as u32, v as u32));
            }
            (n, edges)
        }
        "chain" => {
            let n = p.max(2);
            for v in 1..n {
                edges.push(((v - 1) as u32, v as u32));
            }
            (n, edges)
        }
        "rand3" => {
            // configuration model, 3-regular: shuffle 3n stubs, pair adjacent.
            let n = p.max(4) & !1; // even
            let mut stubs: Vec<u32> = (0..n as u32).flat_map(|v| [v, v, v]).collect();
            rng.shuffle(&mut stubs);
            for c in stubs.chunks(2) {
                if c[0] != c[1] {
                    edges.push((c[0].min(c[1]), c[0].max(c[1])));
                }
            }
            (n, edges)
        }
        _ => {
            eprintln!("rent: unknown synthetic kind {kind}");
            std::process::exit(2);
        }
    }
}

// ── main ─────────────────────────────────────────────────────────────────────
fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut path: Option<String> = None;
    let mut synthetic: Option<String> = None;
    let mut min_g: u64 = 32;
    let mut fit_lo: f64 = 64.0;
    let mut seed: u64 = 1;
    let mut json_path: Option<String> = None;
    let mut max_events: u32 = 40_000_000;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--synthetic" => {
                i += 1;
                synthetic = Some(args[i].clone());
            }
            "--min-g" => {
                i += 1;
                min_g = args[i].parse().unwrap();
            }
            "--max-events" => {
                i += 1;
                max_events = args[i].parse().unwrap();
            }
            "--fit-lo" => {
                i += 1;
                fit_lo = args[i].parse().unwrap();
            }
            "--seed" => {
                i += 1;
                seed = args[i].parse().unwrap();
            }
            "--json" => {
                i += 1;
                json_path = Some(args[i].clone());
            }
            a => path = Some(a.to_string()),
        }
        i += 1;
    }
    let mut rng = Rng(seed);

    // Move the edge list into the CSR build (no clone — the DAG stats stay behind as
    // scalars in `dag`); at E1 scale the duplicate was the difference between fitting in
    // RAM and the OOM killer.
    let (label, nv, edges, dag): (String, usize, Vec<(u32, u32)>, Option<Loaded>) =
        if let Some(kind) = synthetic {
            let (nv, edges) = synth(&kind, &mut rng);
            (format!("synthetic {kind}"), nv, edges, None)
        } else {
            let p = path.unwrap_or_else(|| {
                eprintln!("rent: no trace file given");
                std::process::exit(2);
            });
            let mut loaded = load_trace(&p, max_events);
            let edges = std::mem::take(&mut loaded.edges);
            (p.clone(), loaded.nv, edges, Some(loaded))
        };

    let csr = Csr::build(nv, edges);
    println!("== rent: {label}");
    println!("graph: V={} E={} (deduped undirected)", csr.nv(), csr.ne());
    if let Some(d) = &dag {
        let par = d.nv as f64 / d.span as f64;
        println!(
            "dag: work={} span={} avg-parallelism={:.1} max-width={} init-edges={}",
            d.nv, d.span, par, d.width_max, d.init_edges
        );
        const TAGS: [&str; 16] = [
            "nul", "L", "S", "F", "P", "A", "T1", "T2", "ds", "eps", "N", "var", "dn", "?", "?", "?",
        ];
        let comp: Vec<String> = (0..16)
            .filter(|&t| d.tag_counts[t] > 0)
            .map(|t| format!("{}={}", TAGS[t], d.tag_counts[t]))
            .collect();
        println!("consumers: {}", comp.join(" "));
    }

    let t0 = std::time::Instant::now();
    let (samples, top_cut) = rent_samples(&csr, min_g, &mut rng);
    let rows = level_table(&samples);
    println!("partitioned in {:.1}s ({} blocks sampled)", t0.elapsed().as_secs_f64(), samples.len());
    println!("top bisection cut: {top_cut}");
    if let Some(d) = &dag {
        println!("traffic/depth (top cut / span): {:.3}", top_cut as f64 / d.span as f64);
    }
    println!("  lvl   blocks       mean_g        mean_T");
    for r in &rows {
        println!("  {:>3}  {:>7}  {:>11.1}  {:>12.1}", r.level, r.blocks, r.mean_g, r.mean_t);
    }
    let fit_hi = csr.nv() as f64 / 8.0;
    match fit_p(&rows, fit_lo, fit_hi) {
        Some((p, r2, npts)) => {
            println!("Rent fit over {fit_lo} <= mean_g <= {fit_hi:.0} ({npts} levels): p = {p:.3} (r2={r2:.3})");
        }
        None => println!("Rent fit: not enough levels in the window (graph too small?)"),
    }

    if let Some(jp) = json_path {
        let mut s = String::new();
        s.push_str(&format!(
            "{{\"label\":\"{label}\",\"v\":{},\"e\":{},\"top_cut\":{top_cut},",
            csr.nv(),
            csr.ne()
        ));
        if let Some(d) = &dag {
            s.push_str(&format!(
                "\"work\":{},\"span\":{},\"max_width\":{},\"init_edges\":{},",
                d.nv, d.span, d.width_max, d.init_edges
            ));
        }
        s.push_str("\"levels\":[");
        for (k, r) in rows.iter().enumerate() {
            if k > 0 {
                s.push(',');
            }
            s.push_str(&format!(
                "{{\"level\":{},\"blocks\":{},\"mean_g\":{:.3},\"mean_t\":{:.3}}}",
                r.level, r.blocks, r.mean_g, r.mean_t
            ));
        }
        s.push_str("],");
        let fit = fit_p(&rows, fit_lo, fit_hi);
        match fit {
            Some((p, r2, npts)) => s.push_str(&format!("\"p\":{p:.4},\"r2\":{r2:.4},\"fit_levels\":{npts}}}")),
            None => s.push_str("\"p\":null}"),
        }
        s.push('\n');
        let mut f = File::create(&jp).expect("rent: json create");
        f.write_all(s.as_bytes()).expect("rent: json write");
        println!("json -> {jp}");
    }
}
