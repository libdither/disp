//! Can each rule be run backward from its products alone (`cargo run --example clash`)? A rule's right
//! side must be connected (else undoing it has to find a separate piece), and no two rules may leave
//! the same right side with its outside wires unlabelled (else undoing it is ambiguous).
use tree_net::rules::{End, Rule, RULES, TWO_RULES};
use std::collections::HashMap;

fn find(p: &mut Vec<usize>, x: usize) -> usize { if p[x] != x { let r = find(p, p[x]); p[x] = r; } p[x] }

fn pieces(r: &Rule) -> (usize, usize) {
    let n = r.fresh.len();
    let mut p: Vec<usize> = (0..n).collect();
    let mut bare = 0;
    for &(a, b) in r.wires {
        match (a, b) {
            (End::Fresh(x, _), End::Fresh(y, _)) => { let (i, j) = (find(&mut p, x as usize), find(&mut p, y as usize)); p[i] = j; }
            (End::Fresh(..), _) | (_, End::Fresh(..)) => {}
            _ => bare += 1,
        }
    }
    let roots = (0..n).map(|x| find(&mut p, x)).collect::<std::collections::BTreeSet<_>>().len();
    (roots, bare)
}

fn perms(groups: &[Vec<usize>]) -> Vec<Vec<usize>> {
    let mut out = vec![vec![]];
    for g in groups {
        let mut gp: Vec<Vec<usize>> = vec![];
        let mut idx: Vec<usize> = g.clone();
        fn heap(k: usize, a: &mut Vec<usize>, out: &mut Vec<Vec<usize>>) {
            if k <= 1 { out.push(a.clone()); return; }
            for i in 0..k { heap(k - 1, a, out); if k % 2 == 0 { a.swap(i, k - 1) } else { a.swap(0, k - 1) } }
        }
        heap(idx.len(), &mut idx, &mut gp);
        out = out.into_iter().flat_map(|pre| gp.iter().map(move |q| { let mut v = pre.clone(); v.extend(q); v })).collect();
    }
    out
}

/// The right side with the outside wires unlabelled, in a canonical order.
fn shape(r: &Rule) -> String {
    let n = r.fresh.len();
    let mut order: Vec<usize> = (0..n).collect();
    order.sort_by_key(|&k| r.fresh[k].name());
    let mut groups: Vec<Vec<usize>> = vec![];
    for &k in &order { match groups.last_mut() { Some(g) if r.fresh[g[0]] == r.fresh[k] => g.push(k), _ => groups.push(vec![k]) } }
    let bare = r.wires.iter().filter(|(a, b)| !matches!(a, End::Fresh(..)) && !matches!(b, End::Fresh(..))).count();
    let mut best: Option<String> = None;
    for perm in perms(&groups) {
        let pos: HashMap<usize, usize> = perm.iter().enumerate().map(|(i, &k)| (k, i)).collect();
        let e = |x: End| match x { End::Fresh(k, q) => format!("{}.{q}", pos[&(k as usize)]), _ => "B".into() };
        let mut ws: Vec<String> = r.wires.iter().filter(|(a, b)| matches!(a, End::Fresh(..)) || matches!(b, End::Fresh(..)))
            .map(|&(a, b)| { let (x, y) = (e(a), e(b)); if x <= y { format!("{x}-{y}") } else { format!("{y}-{x}") } }).collect();
        ws.sort();
        let s = format!("{}|{}|bare{bare}", perm.iter().map(|&k| r.fresh[k].name()).collect::<Vec<_>>().join(","), ws.join(" "));
        if best.as_ref().is_none_or(|b| s < *b) { best = Some(s); }
    }
    best.unwrap_or_else(|| format!("|bare{bare}"))
}

fn check(name: &str, rules: &[Rule]) {
    let mut by: HashMap<String, Vec<String>> = HashMap::new();
    let mut apart = vec![];
    for r in rules {
        let lhs = format!("{}·{}{}", r.consumer.name(), r.producer.name(), match r.when { tree_net::rules::When::Any => "", tree_net::rules::When::Same => "(same)", _ => "(differ)" });
        let (roots, bare) = pieces(r);
        if roots + bare > 1 { apart.push(format!("{lhs} [{} pieces: {roots} of agents, {bare} bare wires]", roots + bare)); }
        by.entry(shape(r)).or_default().push(lhs);
    }
    println!("{name}: {} rules", rules.len());
    println!("  right side in more than one piece ({}): {}", apart.len(), apart.join(", "));
    let clash: Vec<String> = by.values().filter(|v| v.len() > 1).map(|v| v.join(" = ")).collect();
    println!("  same right side ({}): {}", clash.len(), clash.join("; "));
}

fn main() {
    check("TWO_RULES", TWO_RULES);
    check("RULES", RULES);
}
