//! Terms in, terms out: the `@(F(L,L),L)` notation the oracle prints, the ternary preorder
//! the lambada benchmark programs ship in, and the named workloads both front ends offer.

use rust_ca_lattice::oracle::{self, ap, f2, s, Term};

/// Parse `L`, `S(t)`, `F(t,t)`, `@(t,t)`; whitespace is ignored. Bare ternary digits
/// (`0`/`1`/`2` preorder) are accepted too, and several space-separated terms left-fold
/// into one application, the way the benchmark runner feeds a program its argument.
pub fn parse(src: &str) -> Result<Term, String> {
    let parts: Vec<&str> = src.split_whitespace().collect();
    let all_ternary = !parts.is_empty() && parts.iter().all(|p| p.bytes().all(|b| matches!(b, b'0'..=b'2')));
    if all_ternary {
        let mut terms = parts.iter().map(|p| ternary(p));
        let first = terms.next().unwrap()?;
        return terms.try_fold(first, |acc, t| Ok(ap(acc, t?)));
    }
    let cleaned: Vec<u8> = src.bytes().filter(|b| !b.is_ascii_whitespace()).collect();
    let mut pos = 0;
    let t = parse_at(&cleaned, &mut pos)?;
    if pos != cleaned.len() {
        return Err(format!("trailing input at byte {pos}"));
    }
    Ok(t)
}

fn parse_at(b: &[u8], pos: &mut usize) -> Result<Term, String> {
    let head = *b.get(*pos).ok_or("unexpected end of term")?;
    *pos += 1;
    let arg = |pos: &mut usize, close: u8| -> Result<Term, String> {
        let t = parse_at(b, pos)?;
        if b.get(*pos) != Some(&close) {
            return Err(format!("expected '{}' at byte {}", close as char, *pos));
        }
        *pos += 1;
        Ok(t)
    };
    let open = |pos: &mut usize| -> Result<(), String> {
        if b.get(*pos) != Some(&b'(') {
            return Err(format!("expected '(' at byte {}", *pos));
        }
        *pos += 1;
        Ok(())
    };
    match head {
        b'L' => Ok(Term::L),
        b'S' => { open(pos)?; Ok(s(arg(pos, b')')?)) }
        b'F' => { open(pos)?; let a = arg(pos, b',')?; Ok(f2(a, arg(pos, b')')?)) }
        b'@' => { open(pos)?; let a = arg(pos, b',')?; Ok(ap(a, arg(pos, b')')?)) }
        c => Err(format!("unexpected '{}' at byte {}", c as char, *pos - 1)),
    }
}

/// Ternary preorder: `0` leaf, `1x` stem, `2xy` fork.
pub fn ternary(src: &str) -> Result<Term, String> {
    fn go(b: &[u8], pos: &mut usize) -> Result<Term, String> {
        let d = *b.get(*pos).ok_or("ternary term ends early")?;
        *pos += 1;
        match d {
            b'0' => Ok(Term::L),
            b'1' => Ok(s(go(b, pos)?)),
            b'2' => { let a = go(b, pos)?; Ok(f2(a, go(b, pos)?)) }
            _ => Err(format!("bad ternary digit '{}'", d as char)),
        }
    }
    let b = src.as_bytes();
    let mut pos = 0;
    let t = go(b, &mut pos)?;
    if pos != b.len() {
        return Err("ternary term has trailing digits".into());
    }
    Ok(t)
}

/// A natural number as the benchmark programs read it: LSB-first bits, `F(bit, rest)`,
/// bit 0 = `L`, bit 1 = `S(L)`, terminated by `L`.
pub fn nat(n: u64) -> Term {
    if n == 0 { Term::L } else { f2(if n & 1 == 1 { s(Term::L) } else { Term::L }, nat(n >> 1)) }
}

pub fn read_nat(t: &Term) -> Option<u64> {
    match t {
        Term::L => Some(0),
        Term::F(bit, rest) => {
            let b = match &**bit { Term::L => 0, Term::S(x) if **x == Term::L => 1, _ => return None };
            Some(b + 2 * read_nat(rest)?)
        }
        _ => None,
    }
}

/// A list of nats, `F(head, tail)` terminated by `L`.
pub fn nat_list(xs: &[u64]) -> Term {
    xs.iter().rev().fold(Term::L, |tail, &x| f2(nat(x), tail))
}

pub fn read_nat_list(t: &Term) -> Option<Vec<u64>> {
    let mut out = vec![];
    let mut t = t;
    loop {
        match t {
            Term::L => return Some(out),
            Term::F(h, rest) => { out.push(read_nat(h)?); t = rest; }
            _ => return None,
        }
    }
}

const PROGRAMS: &str = include_str!("../../../../bench/programs/lambada-benchmarks.json");

/// One of the lambada benchmark programs, by its key in `lambada-benchmarks.json`.
pub fn program(name: &str) -> Term {
    let key = format!("\"{name}\": \"");
    let start = PROGRAMS.find(&key).unwrap_or_else(|| panic!("no benchmark program {name}")) + key.len();
    let len = PROGRAMS[start..].find('"').expect("unterminated program string");
    ternary(&PROGRAMS[start..start + len]).expect("benchmark programs are valid ternary")
}

/// Named workloads: name, the term, and a human description.
pub fn workload(name: &str, n: u64) -> Option<Term> {
    Some(match name {
        "k" => ap(ap(oracle::k(), s(Term::L)), Term::L),
        "fork" => ap(f2(Term::L, Term::L), Term::L),
        "s-rule" => ap(f2(s(Term::L), s(Term::L)), Term::L),
        "disp-t" => oracle::disp_t(),
        "k-chain" => oracle::chain_k(n as u32),
        "discard-tree" => oracle::discard_tree(n as u32),
        "convoy" => oracle::convoy(n as u32),
        "share-tower" => oracle::share_tower(n as u32),
        "full-tree" => oracle::full_tree(n as u32),
        "fib" => ap(program("recursive-fib"), nat(n)),
        "exp" => ap(program("silly-exp"), nat(n)),
        "sort" => ap(program("merge-sort"), nat_list(&(1..=n).rev().collect::<Vec<_>>())),
        "size-self" => ap(program("size"), program("size")),
        "rules" => program("exercise-rules"),
        _ => return None,
    })
}

pub const WORKLOADS: &[(&str, &str, u64)] = &[
    ("k", "K applied: K (S L) L → S(L)", 0),
    ("fork", "fork dispatch: (F L L) L → L", 0),
    ("s-rule", "the S-rule shares its argument through a duplicator", 0),
    ("disp-t", "drives apply → triage → second-level dispatch", 0),
    ("k-chain", "n nested K-redexes", 4),
    ("discard-tree", "K throws away a 2^n-leaf tree (erasure wave)", 4),
    ("convoy", "n stacked K-path triages, forced one at a time", 4),
    ("share-tower", "each level duplicates the sub-tower", 4),
    ("fib", "recursive fib(n), the lambada benchmark program", 4),
    ("exp", "the lambada silly-exp program on n", 3),
    ("sort", "merge-sort of the list n..1", 4),
    ("size-self", "size applied to itself", 0),
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn notation_round_trips() {
        for src in ["L", "S(L)", "F(L,S(L))", "@(@(S(L),S(L)),L)", "@(F(F(L,S(L)),L),F(L,L))"] {
            assert_eq!(oracle::show(&parse(src).unwrap()), src);
        }
        assert_eq!(oracle::show(&parse("2100").unwrap()), "F(S(L),L)");
        assert_eq!(oracle::show(&parse("10 0").unwrap()), "@(S(L),L)");
        for n in [0, 1, 2, 5, 14, 255] {
            assert_eq!(read_nat(&nat(n)), Some(n));
        }
    }

    #[test]
    fn benchmark_programs_compute() {
        use rust_ca_lattice::oracle::{nf, Fuel};
        let fib = |n| read_nat(&nf(workload("fib", n).unwrap(), &mut Fuel(50_000_000)).unwrap());
        // The program counts from fib(0) = fib(1) = 1.
        assert_eq!([fib(0), fib(1), fib(5), fib(8)], [Some(1), Some(1), Some(8), Some(34)]);
        let sorted = nf(workload("sort", 5).unwrap(), &mut Fuel(50_000_000)).unwrap();
        assert_eq!(read_nat_list(&sorted), Some(vec![1, 2, 3, 4, 5]));
    }
}
