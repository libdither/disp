//! Terms in, terms out: the `@(F(L,L),L)` notation the oracle prints, the ternary preorder
//! the lambada benchmark programs ship in, and the named workloads both front ends offer.

use rust_ca_lattice::oracle::{self, ap, f2, s, Term};
use rust_ca_lattice::sup::{self, STerm};
use std::rc::Rc;

/// Parse a plain term (`parse_sup`, without superpositions).
pub fn parse(src: &str) -> Result<Term, String> {
    parse_sup(src)?.plain().ok_or_else(|| "a superposition (&ℓ{…}) here: only the strand lattice runs those".into())
}

/// Parse `L`, `S(t)`, `F(t,t)`, `@(t,t)`, bare ternary digits (`0`/`1`/`2` preorder), and
/// `&ℓ{t,t}`, the superposition of label ℓ ≥ 1 (rules.rs `SUP_RULES`). Terms side by side apply
/// left to right, the way the benchmark runner feeds a program its arguments, at the top and
/// inside any bracket; whitespace only separates. So `<program> &1{20200,2020200} 20200` is a
/// program applied to 2 or 3 (as disp writes numbers), then to 2.
pub fn parse_sup(src: &str) -> Result<STerm, String> {
    let b = src.as_bytes();
    let mut pos = 0;
    let t = sequence(b, &mut pos)?;
    skip(b, &mut pos);
    match b.get(pos) {
        None => Ok(t),
        Some(&c) => Err(format!("unexpected '{}' at byte {pos}", c as char)),
    }
}

fn skip(b: &[u8], pos: &mut usize) { while b.get(*pos).is_some_and(|c| c.is_ascii_whitespace()) { *pos += 1; } }

/// Terms side by side, applied left to right, up to a ',' or a closing bracket.
fn sequence(b: &[u8], pos: &mut usize) -> Result<STerm, String> {
    let mut t = one(b, pos)?;
    loop {
        skip(b, pos);
        match b.get(*pos) {
            None | Some(b',' | b')' | b'}') => return Ok(t),
            _ => t = sup::ap(t, one(b, pos)?),
        }
    }
}

fn one(b: &[u8], pos: &mut usize) -> Result<STerm, String> {
    skip(b, pos);
    let head = *b.get(*pos).ok_or("unexpected end of term")?;
    if matches!(head, b'0'..=b'2') {
        let start = *pos;
        while b.get(*pos).is_some_and(|c| matches!(c, b'0'..=b'2')) { *pos += 1; }
        return Ok((&ternary(std::str::from_utf8(&b[start..*pos]).unwrap())?).into());
    }
    *pos += 1;
    let expect = |pos: &mut usize, c: u8| -> Result<(), String> {
        skip(b, pos);
        if b.get(*pos) != Some(&c) { return Err(format!("expected '{}' at byte {}", c as char, *pos)); }
        *pos += 1;
        Ok(())
    };
    let two = |pos: &mut usize, open: u8, close: u8| -> Result<(Rc<STerm>, Rc<STerm>), String> {
        expect(pos, open)?;
        let a = sequence(b, pos)?;
        expect(pos, b',')?;
        let c = sequence(b, pos)?;
        expect(pos, close)?;
        Ok((Rc::new(a), Rc::new(c)))
    };
    match head {
        b'L' => Ok(STerm::L),
        b'S' => { expect(pos, b'(')?; let a = sequence(b, pos)?; expect(pos, b')')?; Ok(STerm::S(Rc::new(a))) }
        b'F' => { let (x, y) = two(pos, b'(', b')')?; Ok(STerm::F(x, y)) }
        b'@' => { let (x, y) = two(pos, b'(', b')')?; Ok(STerm::Ap(x, y)) }
        b'&' => {
            let start = *pos;
            while b.get(*pos).is_some_and(|c| c.is_ascii_digit()) { *pos += 1; }
            let label: u8 = std::str::from_utf8(&b[start..*pos]).unwrap().parse().map_err(|_| format!("expected a label 1–255 after '&' at byte {start}"))?;
            if label == 0 { return Err("label 0 is a plain copy: superpositions are labelled 1–255".into()); }
            let (x, y) = two(pos, b'{', b'}')?;
            Ok(STerm::Sup(label, x, y))
        }
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

/// A workload that is a program applied to its argument, applied to a superposition of
/// arguments instead: `fib:&1{2,3}` is fib applied to &1{2,3}, `sort:&1{&2{1,2},3}` sorts one
/// of three lists. None if `name` is no such workload.
pub fn workload_sup(name: &str, arg: &str) -> Option<Result<STerm, String>> {
    let Some(Term::Ap(program, _)) = workload(name, 0) else { return None };
    if !matches!(name, "fib" | "exp" | "sort") { return None; }
    fn go(name: &str, b: &[u8], pos: &mut usize) -> Result<STerm, String> {
        let digits = |pos: &mut usize| { let s = *pos; while b.get(*pos).is_some_and(|c| c.is_ascii_digit()) { *pos += 1; } std::str::from_utf8(&b[s..*pos]).unwrap().parse::<u64>() };
        if b.get(*pos) != Some(&b'&') {
            let n = digits(pos).map_err(|_| format!("expected a number or &ℓ{{…}} at byte {}", *pos))?;
            let Some(Term::Ap(_, a)) = workload(name, n) else { unreachable!() };
            return Ok((&*a).into());
        }
        *pos += 1;
        let label = digits(pos).ok().filter(|&l| (1..256).contains(&l)).ok_or("a label 1–255 after '&'")? as u8;
        let part = |pos: &mut usize, c: u8| -> Result<STerm, String> {
            if b.get(*pos) != Some(&c) { return Err(format!("expected '{}' at byte {}", c as char, *pos)); }
            *pos += 1;
            go(name, b, pos)
        };
        let x = part(pos, b'{')?;
        let y = part(pos, b',')?;
        if b.get(*pos) != Some(&b'}') { return Err(format!("expected '}}' at byte {}", *pos)); }
        *pos += 1;
        Ok(sup::sup(label, x, y))
    }
    let b: Vec<u8> = arg.bytes().filter(|c| !c.is_ascii_whitespace()).collect();
    let mut pos = 0;
    Some(go(name, &b, &mut pos).and_then(|a| if pos == b.len() { Ok(sup::ap((&*program).into(), a)) } else { Err(format!("trailing input at byte {pos}")) }))
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
        // Superpositions, and terms side by side inside brackets.
        for (src, want) in [("&1{L,S(L)}", "&1{L,S(L)}"), ("10 &2{0, 10} 0", "@(@(S(L),&2{L,S(L)}),L)"),
                            ("F(10 0, &12{200,&1{L,L}})", "F(@(S(L),L),&12{F(L,L),&1{L,L}})")] {
            assert_eq!(sup::show(&parse_sup(src).unwrap()), want);
        }
        assert!(parse("&1{L,L}").is_err() && parse_sup("&0{L,L}").is_err() && parse_sup("&1{L}").is_err());
        let fib = workload_sup("fib", "&1{2,3}").unwrap().unwrap();
        assert_eq!(fib.universes(&[1]).into_iter().map(|(_, t)| t).collect::<Vec<_>>(), [workload("fib", 2).unwrap(), workload("fib", 3).unwrap()]);
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
