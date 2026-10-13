//! The persistent two-way machine (README section Two-way programs). The program is one tree of raw
//! agents (leaf, stem, fork: `iso::encode`'s form), main and the definitions it reaches, and it never
//! changes: the token walks it in place. What an instruction does is its node's shape, decoded once
//! when the program is planted (`instr`; a machine of agents would read the shape as it went: the
//! op's number, then the patterns against the tree it carries). Everything that changes is something the token
//! carries: the tree being worked on and a stack. Working on a part pushes the rest of the tree onto
//! the stack; a call pushes where to come back to and walks to the definition, and coming out of the
//! definition pops it; a call backward walks its definition backward.
//!
//! Agents are kept, not made: a rearrangement reuses the agents it takes apart for the ones it builds,
//! a copy shares what it copies (a shared tree is copied one layer at a time when taken apart). Agents
//! are made only when a tree grows, when a shared tree is taken apart, for a stack frame, or when data
//! is run as a program (its code laid out for the while); `made` and `gone` count them.
//!
//! There can be many tokens. At a `both` the token becomes a fork: the pair it carried, waiting with a
//! hole for each half while a token of its own works on each; a half done comes back into its hole,
//! and with both back the fork is a token again. Every move (a token's step, a half setting off or
//! coming back, the fork opening or closing) is undone by another, so tokens move in any order, and
//! nothing records which went when. A round moves everything that can move at its start.
use crate::iso::{decode, encode_pat, encode_pats, fork, nat, show, show_path, stem, DNode, Defs, Dir, Iso, Pat, Way, DK};
use rust_ca_lattice::oracle::Term;
use std::collections::HashMap;

const NONE: u32 = u32::MAX;

/// A node of the program's tree, and its parent (NONE for a root).
#[derive(Clone, Copy, Debug)]
pub struct CNode { pub kind: DK, pub kids: [u32; 2], pub up: u32 }

/// What an instruction does, as its node's shape says.
#[derive(Clone, Debug)]
pub enum Op { Seq, Note, Re(Pat, Pat), Dup(bool), On(Vec<Dir>), Case(Vec<(Pat, Pat)>), Loop(Pat, Pat), Call(usize, bool), Eval(bool), Forget, Both }

/// An instruction's node: what it does, its parts (their nodes), the instruction it is a part of.
#[derive(Clone, Debug)]
pub struct Instr { pub op: Op, pub kids: Vec<u32>, pub up: Option<(u32, u32)>, pub label: String }

/// What the stack holds: a tree with a part taken out (a hole in its place), where a call comes back
/// to, that and the program a run is running, or (at the bottom of a half's stack) the fork it is from.
#[derive(Clone, Copy, Debug)]
pub enum Frame { Park(u32), Ret { call: u32, inv: bool }, Eval { call: u32, inv: bool, prog: u32, code: u32 }, Half { fork: u32, side: u8 } }
impl Frame {
    /// How many agents the frame is.
    fn agents(&self) -> usize { if matches!(self, Frame::Ret { .. } | Frame::Eval { .. }) { 2 } else { 1 } }
}

/// A token: where it is, the tree it carries, its stack, whether it is inside an odd number of calls
/// run backward, and the round it was made in.
#[derive(Clone, Debug)]
pub struct Token { pub at: (u32, Way), pub payload: u32, pub stack: Vec<Frame>, pub parity: bool, pub born: u64 }

/// Where a half of a fork's pair is: away (a token has it), or back in its hole, at its part's start
/// (before it ran, as the program goes forward) or end.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Slot { Away, Start, End }

/// A pair whose halves went their own ways at a `both` (`node`): the pair, a hole in it for each half
/// away; the stack and parity of the token it was; the round it was made in.
#[derive(Clone, Debug)]
pub struct Fork { pub node: u32, pub pair: u32, pub halves: [Slot; 2], pub stack: Vec<Frame>, pub parity: bool, pub born: u64 }

/// What can move: a token, or a fork (a half setting off from it, or the halves coming together).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Actor { Token(u32), Fork(u32) }

pub struct Walker {
    pub defs: Defs,
    pub code: Vec<Option<CNode>>,
    cfree: Vec<u32>,
    pub instr: HashMap<u32, Instr>,
    /// Each definition's node, if main reaches it.
    pub def_root: Vec<u32>,
    pub main: u32,
    pub data: Vec<Option<DNode>>,
    dfree: Vec<u32>,
    pub toks: Vec<Option<Token>>,
    pub forks: Vec<Option<Fork>>,
    /// Whether time goes forward: the way every move goes.
    pub forward: bool,
    /// Moves made, less moves undone.
    pub clock: i64,
    /// Rounds begun (`round`).
    pub rounds: u64,
    /// In a round, a node of the program serves one token (the others wait their turn), as an agent
    /// meets one other at a time.
    pub exclusive: bool,
    /// Agents made and taken away, all told (trees, stack frames, forks, code laid out for a run).
    pub made: u64,
    pub gone: u64,
    /// The instructions whose rules the last move (or round) fired (walking fires none).
    pub fired: Vec<u32>,
    /// Tree agents looked at, all told: what a move needs next to it.
    pub reads: std::cell::Cell<u64>,
    // The token moving now, in these: where it is, what it carries, its stack, its parity, its number
    // (NONE between moves), whether it is gone (into a fork, or back into one), and when it was made.
    at: (u32, Way),
    payload: u32,
    stack: Vec<Frame>,
    parity: bool,
    cur: u32,
    cur_gone: bool,
    born: u64,
}

/// The definitions a program calls, and the ones they call.
fn reached(defs: &Defs, i: &Iso, out: &mut Vec<usize>) {
    match i {
        Iso::Call(d, _) => if !out.contains(d) { out.push(*d); reached(defs, defs.body(*d, false), out); },
        Iso::Seq(xs) => xs.iter().for_each(|x| reached(defs, x, out)),
        Iso::On(_, f) | Iso::Note(_, f) => reached(defs, f, out),
        Iso::Both(f, g) => { reached(defs, f, out); reached(defs, g, out) }
        Iso::Case(cs) => cs.iter().for_each(|c| reached(defs, &c.body, out)),
        Iso::Loop { body, again, .. } => { reached(defs, body, out); reached(defs, again, out) }
        _ => {}
    }
}

impl Walker {
    pub fn new(defs: Defs, main: &Iso, input: &Term) -> Walker {
        let mut w = Walker { defs, code: vec![], cfree: vec![], instr: HashMap::new(), def_root: vec![], main: NONE, data: vec![], dfree: vec![],
            toks: vec![], forks: vec![], forward: true, clock: 0, rounds: 0, exclusive: false, made: 0, gone: 0, fired: vec![], reads: Default::default(),
            at: (0, Way::Down), payload: NONE, stack: vec![], parity: false, cur: NONE, cur_gone: false, born: 0 };
        let mut reach = vec![];
        reached(&w.defs, main, &mut reach);
        w.def_root = vec![NONE; w.defs.names.len()];
        // The program's tree: (main, the definitions it reaches, as a list).
        let main_root = w.plant_instr(main);
        let mut lib = w.cmk(DK::L, [NONE; 2]);
        for &d in reach.iter().rev() {
            let body = w.defs.body(d, false).clone();
            let r = w.plant_instr(&body);
            w.def_root[d] = r;
            lib = w.cmk(DK::F, [r, lib]);
        }
        w.cmk(DK::F, [main_root, lib]);
        w.main = main_root;
        let payload = w.plant(input);
        w.toks.push(Some(Token { at: (main_root, Way::Down), payload, stack: vec![], parity: false, born: 0 }));
        (w.made, w.gone) = (0, 0);
        w
    }

    // -- the program's tree
    pub fn cnode(&self, n: u32) -> CNode { self.code[n as usize].expect("live code node") }
    fn cmk(&mut self, kind: DK, kids: [u32; 2]) -> u32 {
        let node = Some(CNode { kind, kids, up: NONE });
        let n = match self.cfree.pop() { Some(k) => { self.code[k as usize] = node; k } None => { self.code.push(node); self.code.len() as u32 - 1 } };
        for k in kids { if k != NONE { self.code[k as usize].as_mut().unwrap().up = n; } }
        n
    }
    fn cterm(&mut self, t: &Term) -> u32 {
        match t {
            Term::L => self.cmk(DK::L, [NONE; 2]),
            Term::S(a) => { let a = self.cterm(a); self.cmk(DK::S, [a, NONE]) }
            Term::F(a, b) => { let a = self.cterm(a); let b = self.cterm(b); self.cmk(DK::F, [a, b]) }
            Term::Ap(..) => unreachable!(),
        }
    }
    fn clist(&mut self, xs: Vec<u32>) -> u32 { xs.into_iter().rev().fold(self.cmk(DK::L, [NONE; 2]), |t, x| self.cmk(DK::F, [x, t])) }
    /// Plant an instruction in `iso::encode`'s form (a note's words left out: they are only its label),
    /// noting what each instruction node does; returns its node.
    fn plant_instr(&mut self, i: &Iso) -> u32 {
        let flag = |w: &mut Self, v: bool| w.cterm(&nat(v as usize));
        let mut kids = vec![];
        let (opn, args, op) = match i {
            Iso::Seq(xs) => { let cs: Vec<u32> = xs.iter().map(|x| self.plant_instr(x)).collect(); kids = cs.clone(); (0, self.clist(cs), Op::Seq) }
            Iso::Re(l, r) => { let [a, b] = encode_pats([l, r]).map(|t| self.cterm(&t)); (1, self.cmk(DK::F, [a, b]), Op::Re(l.clone(), r.clone())) }
            Iso::Dup(v) => (2, flag(self, *v), Op::Dup(*v)),
            Iso::On(p, f) => {
                let ds: Vec<u32> = p.iter().map(|d| self.cterm(&nat(*d as usize))).collect();
                let path = self.clist(ds);
                let c = self.plant_instr(f); kids.push(c);
                (3, self.cmk(DK::F, [path, c]), Op::On(p.clone()))
            }
            Iso::Case(cs) => {
                let mut cells = vec![];
                for c in cs {
                    let (w, g) = (self.cterm(&encode_pat(&c.when)), self.cterm(&encode_pat(&c.gives)));
                    let wg = self.cmk(DK::F, [w, g]);
                    let b = self.plant_instr(&c.body); kids.push(b);
                    cells.push(self.cmk(DK::F, [wg, b]));
                }
                (4, self.clist(cells), Op::Case(cs.iter().map(|c| (c.when.clone(), c.gives.clone())).collect()))
            }
            Iso::Loop { from, body, until, again } => {
                let (f, u) = (self.cterm(&encode_pat(from)), self.cterm(&encode_pat(until)));
                let fu = self.cmk(DK::F, [f, u]);
                let (b, a) = (self.plant_instr(body), self.plant_instr(again)); kids = vec![b, a];
                let ba = self.cmk(DK::F, [b, a]);
                (5, self.cmk(DK::F, [fu, ba]), Op::Loop(from.clone(), until.clone()))
            }
            Iso::Call(d, v) => { let (a, b) = (self.cterm(&nat(*d)), flag(self, *v)); (6, self.cmk(DK::F, [a, b]), Op::Call(*d, *v)) }
            Iso::Eval(v) => (7, flag(self, *v), Op::Eval(*v)),
            Iso::Forget => (8, self.cmk(DK::L, [NONE; 2]), Op::Forget),
            Iso::Note(_, f) => { let words = self.cmk(DK::L, [NONE; 2]); let c = self.plant_instr(f); kids.push(c); (9, self.cmk(DK::F, [words, c]), Op::Note) }
            Iso::Both(f, g) => { let (a, b) = (self.plant_instr(f), self.plant_instr(g)); kids = vec![a, b]; (10, self.cmk(DK::F, [a, b]), Op::Both) }
        };
        let opnode = self.cterm(&nat(opn));
        let root = self.cmk(DK::F, [opnode, args]);
        for (k, &c) in kids.iter().enumerate() { self.instr.get_mut(&c).unwrap().up = Some((root, k as u32)); }
        self.instr.insert(root, Instr { op, kids, up: None, label: i.label(&self.defs) });
        root
    }
    /// The program's tree below n, as a tree.
    pub fn code_tree(&self, n: u32) -> Term {
        let c = self.cnode(n);
        match c.kind { DK::L => Term::L, DK::S => stem(self.code_tree(c.kids[0])), DK::F => fork(self.code_tree(c.kids[0]), self.code_tree(c.kids[1])), DK::Hole => unreachable!() }
    }
    fn unplant(&mut self, n: u32) {
        let c = self.cnode(n);
        self.instr.remove(&n);
        self.code[n as usize] = None;
        self.cfree.push(n);
        self.gone += 1;
        for k in c.kids { if k != NONE { self.unplant(k); } }
    }

    // -- trees the token carries (as iso::Machine's, copies shared)
    pub fn dnode(&self, n: u32) -> DNode { self.reads.set(self.reads.get() + 1); self.data[n as usize].expect("live tree node") }
    fn dmk(&mut self, kind: DK, kids: [u32; 2]) -> u32 {
        self.made += 1;
        let n = Some(DNode { kind, kids, rc: 1 });
        match self.dfree.pop() { Some(k) => { self.data[k as usize] = n; k } None => { self.data.push(n); self.data.len() as u32 - 1 } }
    }
    fn ddrop(&mut self, n: u32) { self.gone += 1; self.data[n as usize] = None; self.dfree.push(n); }
    fn hold(&mut self, n: u32) { if n != NONE { self.data[n as usize].as_mut().unwrap().rc += 1; } }
    fn release(&mut self, n: u32) {
        let d = self.data[n as usize].as_mut().unwrap();
        d.rc -= 1;
        if d.rc > 0 { return; }
        let kids = d.kids;
        self.ddrop(n);
        for k in kids { if k != NONE { self.release(k); } }
    }
    fn own(&mut self, n: u32) -> u32 {
        let d = self.dnode(n);
        if d.rc == 1 { return n; }
        self.data[n as usize].as_mut().unwrap().rc -= 1;
        for k in d.kids { self.hold(k); }
        self.dmk(d.kind, d.kids)
    }
    fn plant(&mut self, t: &Term) -> u32 {
        match t {
            Term::L => self.dmk(DK::L, [NONE; 2]),
            Term::S(a) => { let a = self.plant(a); self.dmk(DK::S, [a, NONE]) }
            Term::F(a, b) => { let a = self.plant(a); let b = self.plant(b); self.dmk(DK::F, [a, b]) }
            Term::Ap(..) => panic!("an application is not a tree"),
        }
    }
    pub fn tree(&self, n: u32) -> Term {
        let d = self.dnode(n);
        match d.kind { DK::L => Term::L, DK::S => stem(self.tree(d.kids[0])), DK::F => fork(self.tree(d.kids[0]), self.tree(d.kids[1])), DK::Hole => panic!("a hole in a tree being read") }
    }
    fn same(&self, a: u32, b: u32) -> bool {
        if a == b { return true; }
        let (x, y) = (self.dnode(a), self.dnode(b));
        x.kind == y.kind && (0..2).all(|q| (x.kids[q] == NONE) == (y.kids[q] == NONE) && (x.kids[q] == NONE || self.same(x.kids[q], y.kids[q])))
    }
    /// What the first token carries (at the start and the end there is one).
    pub fn payload(&self) -> Term { self.tree(self.toks.iter().flatten().next().expect("a token").payload) }
    fn carried(&self) -> Term { self.tree(self.payload) }
    fn fits(&self, p: &Pat, n: u32) -> bool {
        let d = self.dnode(n);
        match p {
            Pat::Any | Pat::Var(_) => true,
            Pat::L => d.kind == DK::L,
            Pat::S(a) => d.kind == DK::S && self.fits(a, d.kids[0]),
            Pat::F(a, b) => d.kind == DK::F && self.fits(a, d.kids[0]) && self.fits(b, d.kids[1]),
        }
    }
    /// Take the payload apart by l and put it together by r, reusing the agents l spells out (of each
    /// kind) for the ones r spells out: only what r has more of is made, and only what l has more of goes.
    fn rearrange(&mut self, l: &Pat, r: &Pat) -> Result<(), String> {
        if !self.fits(l, self.payload) { return Err(format!("{} does not fit {}", show(&self.carried()), l.show())); }
        let mut env = [NONE; 26];
        let mut spare: [Vec<u32>; 3] = Default::default();
        fn take(w: &mut Walker, p: &Pat, n: u32, env: &mut [u32; 26], spare: &mut [Vec<u32>; 3]) {
            if let Pat::Var(v) = p { env[*v as usize] = n; return; }
            let d = w.dnode(n);
            if d.rc > 1 { w.data[n as usize].as_mut().unwrap().rc -= 1; for k in d.kids { w.hold(k); } } else { spare[d.kind as usize].push(n); }
            match p { Pat::S(a) => take(w, a, d.kids[0], env, spare), Pat::F(a, b) => { take(w, a, d.kids[0], env, spare); take(w, b, d.kids[1], env, spare) } _ => {} }
        }
        fn make(w: &mut Walker, p: &Pat, env: &[u32; 26], spare: &mut [Vec<u32>; 3]) -> u32 {
            let (kind, kids) = match p {
                Pat::Var(v) => return env[*v as usize],
                Pat::L => (DK::L, [NONE; 2]),
                Pat::S(a) => (DK::S, [make(w, a, env, spare), NONE]),
                Pat::F(a, b) => { let a = make(w, a, env, spare); (DK::F, [a, make(w, b, env, spare)]) }
                Pat::Any => unreachable!(),
            };
            match spare[kind as usize].pop() { Some(n) => { w.data[n as usize] = Some(DNode { kind, kids, rc: 1 }); n } None => w.dmk(kind, kids) }
        }
        take(self, l, self.payload, &mut env, &mut spare);
        self.payload = make(self, r, &env, &mut spare);
        for n in spare.into_iter().flatten() { self.ddrop(n); }
        Ok(())
    }
    fn dup(&mut self, back: bool) -> Result<(), String> {
        let p = self.payload;
        if !back { self.hold(p); self.payload = self.dmk(DK::F, [p, p]); return Ok(()); }
        let d = self.dnode(p);
        if d.kind != DK::F || !self.same(d.kids[0], d.kids[1]) { return Err(format!("un-copy of unequal halves {}", show(&self.carried()))); }
        if d.rc > 1 { self.data[p as usize].as_mut().unwrap().rc -= 1; for k in d.kids { self.hold(k); } } else { self.ddrop(p); }
        self.release(d.kids[1]);
        self.payload = d.kids[0];
        Ok(())
    }
    /// The part at `path` goes with the token; the rest, a hole in its place, onto the stack.
    fn split(&mut self, path: &[Dir]) -> Result<(), String> {
        let q_of = |d: &Dir| if *d == Dir::Right { 1 } else { 0 };
        let mut at = self.payload;
        for (k, d) in path.iter().enumerate() {
            let x = self.dnode(at);
            if !matches!((d, x.kind), (Dir::Left, DK::F) | (Dir::Under, DK::S) | (Dir::Right, DK::F)) {
                return Err(format!("no {} part in {}", show_path(&path[..=k]), show(&self.carried())));
            }
            at = x.kids[q_of(d)];
        }
        self.payload = self.own(self.payload);
        let mut at = self.payload;
        for d in &path[..path.len() - 1] {
            let c = self.own(self.dnode(at).kids[q_of(d)]);
            self.data[at as usize].as_mut().unwrap().kids[q_of(d)] = c;
            at = c;
        }
        let q = q_of(path.last().expect("a path"));
        let part = self.dnode(at).kids[q];
        let hole = self.dmk(DK::Hole, [NONE; 2]);
        self.data[at as usize].as_mut().unwrap().kids[q] = hole;
        self.push(Frame::Park(self.payload));
        self.payload = part;
        Ok(())
    }
    /// The token's part back into the tree on top of the stack, which the token takes again.
    fn join(&mut self, path: &[Dir]) {
        let Some(Frame::Park(whole)) = self.pop() else { panic!("no tree on the stack to come back to") };
        let mut at = whole;
        for (k, d) in path.iter().enumerate() {
            let q = if *d == Dir::Right { 1 } else { 0 };
            if k + 1 == path.len() {
                let hole = self.dnode(at).kids[q];
                self.ddrop(hole);
                self.data[at as usize].as_mut().unwrap().kids[q] = self.payload;
            } else { at = self.dnode(at).kids[q]; }
        }
        self.payload = whole;
    }
    /// A stack frame is an agent of its own (and a return point another).
    fn push(&mut self, f: Frame) { self.made += f.agents() as u64; self.stack.push(f); }
    fn pop(&mut self) -> Option<Frame> { let f = self.stack.pop()?; self.gone += f.agents() as u64; Some(f) }
    fn forget(&mut self) -> Result<(), String> {
        let p = self.payload;
        let d = self.dnode(p);
        if d.kind != DK::F { return Err(format!("forget needs a pair, not {}", show(&self.carried()))); }
        if d.rc > 1 { self.data[p as usize].as_mut().unwrap().rc -= 1; for k in d.kids { self.hold(k); } } else { self.ddrop(p); }
        self.release(d.kids[1]);
        self.payload = d.kids[0];
        Ok(())
    }
    /// Run the program the payload's left half holds: its code laid out (the one time code is made),
    /// the program itself onto the stack, the token going in with the right half. Returns the code's node.
    fn open_eval(&mut self, n: u32, inv: bool) -> Result<u32, String> {
        let p = self.payload;
        let d = self.dnode(p);
        if d.kind != DK::F { return Err(format!("run needs a program and its input, not {}", show(&self.carried()))); }
        let prog = decode(&self.tree(d.kids[0])).ok_or_else(|| format!("{} is not a program", show(&self.tree(d.kids[0]))))?;
        let before = self.code.iter().flatten().count();
        // Definitions it calls that the program's tree does not have yet join it (and stay).
        let mut reach = vec![];
        reached(&self.defs, &prog, &mut reach);
        for dd in reach { if self.def_root[dd] == NONE { let body = self.defs.body(dd, false).clone(); self.def_root[dd] = self.plant_instr(&body); } }
        if d.rc > 1 { self.data[p as usize].as_mut().unwrap().rc -= 1; for k in d.kids { self.hold(k); } } else { self.ddrop(p); }
        let code = self.plant_instr(&prog);
        self.made += (self.code.iter().flatten().count() - before) as u64;
        self.push(Frame::Eval { call: n, inv, prog: d.kids[0], code });
        self.payload = d.kids[1];
        Ok(code)
    }

    // -- tokens and forks
    fn load(&mut self, i: u32) {
        let t = self.toks[i as usize].take().expect("a token");
        (self.at, self.payload, self.stack, self.parity, self.born) = (t.at, t.payload, t.stack, t.parity, t.born);
        (self.cur, self.cur_gone) = (i, false);
    }
    fn store(&mut self) {
        if !self.cur_gone {
            self.toks[self.cur as usize] = Some(Token { at: self.at, payload: self.payload, stack: std::mem::take(&mut self.stack), parity: self.parity, born: self.born });
        }
        self.cur = NONE;
    }
    fn spawn(&mut self, t: Token) -> u32 {
        match self.toks.iter().position(|t| t.is_none()) { Some(i) => { self.toks[i] = Some(t); i as u32 } None => { self.toks.push(Some(t)); self.toks.len() as u32 - 1 } }
    }
    /// At a `both`: the token becomes a fork, its pair's halves in it, each at its part's start (`slot`
    /// Start) or, going backward through the program, its end.
    fn open(&mut self, n: u32, slot: Slot) -> Result<(), String> {
        if self.dnode(self.payload).kind != DK::F { return Err(format!("both needs a pair, not {}", show(&self.carried()))); }
        let pair = self.own(self.payload);
        let f = Some(Fork { node: n, pair, halves: [slot, slot], stack: std::mem::take(&mut self.stack), parity: self.parity, born: self.rounds });
        self.made += 1;
        match self.forks.iter().position(|f| f.is_none()) { Some(j) => self.forks[j] = f, None => self.forks.push(f) }
        self.cur_gone = true;
        Ok(())
    }
    /// A half at the end of its part (`slot` End; going backward, its start) back into its hole in the fork.
    fn park(&mut self, k: u32, slot: Slot) -> Result<(), String> {
        let Some(&Frame::Half { fork, side }) = self.stack.last() else { return Err("a half came back with no fork to go to".into()) };
        if side as u32 != k || self.stack.len() != 1 { return Err("a half came back to the wrong place".into()); }
        self.pop();
        let f = self.forks[fork as usize].as_mut().expect("its fork");
        f.halves[side as usize] = slot;
        let pair = f.pair;
        let hole = self.dnode(pair).kids[side as usize];
        self.ddrop(hole);
        self.data[pair as usize].as_mut().unwrap().kids[side as usize] = self.payload;
        self.cur_gone = true;
        Ok(())
    }
    /// A fork's move: a half waiting at its part's start (as the program goes) sets off, a token of its
    /// own, a hole in its place; or, both halves back at their ends, the fork is a token again.
    fn fork_move(&mut self, j: u32) -> Result<(), String> {
        let f = self.forks[j as usize].clone().expect("a fork");
        let e = self.forward != f.parity;
        let (begin, fin) = if e { (Slot::Start, Slot::End) } else { (Slot::End, Slot::Start) };
        self.fired.push(f.node);
        if let Some(side) = f.halves.iter().position(|&h| h == begin) {
            let part = self.dnode(f.pair).kids[side];
            let hole = self.dmk(DK::Hole, [NONE; 2]);
            self.data[f.pair as usize].as_mut().unwrap().kids[side] = hole;
            self.forks[j as usize].as_mut().unwrap().halves[side] = Slot::Away;
            self.made += 1;
            let at = (self.instr[&f.node].kids[side], if begin == Slot::Start { Way::Down } else { Way::Up });
            self.spawn(Token { at, payload: part, stack: vec![Frame::Half { fork: j, side: side as u8 }], parity: f.parity, born: self.rounds });
            return Ok(());
        }
        if f.halves != [fin, fin] { return Err("a fork with nothing to do".into()); }
        self.forks[j as usize] = None;
        self.gone += 1;
        self.spawn(Token { at: (f.node, if e { Way::Up } else { Way::Down }), payload: f.pair, stack: f.stack, parity: f.parity, born: self.rounds });
        Ok(())
    }

    /// Whether an actor can move, the way time goes.
    pub fn can(&self, a: Actor) -> bool {
        match a {
            Actor::Token(i) => self.toks.get(i as usize).and_then(|t| t.as_ref())
                .is_some_and(|t| !(t.stack.is_empty() && t.at == (self.main, if self.forward { Way::Up } else { Way::Down }))),
            Actor::Fork(j) => self.forks.get(j as usize).and_then(|f| f.as_ref()).is_some_and(|f| {
                let (begin, fin) = if self.forward != f.parity { (Slot::Start, Slot::End) } else { (Slot::End, Slot::Start) };
                f.halves.contains(&begin) || f.halves == [fin, fin]
            }),
        }
    }
    /// Everything that can move now, tokens first, each by number.
    pub fn actors(&self) -> Vec<Actor> {
        let ts = (0..self.toks.len() as u32).map(Actor::Token);
        ts.chain((0..self.forks.len() as u32).map(Actor::Fork)).filter(|&a| self.can(a)).collect()
    }
    /// One move by `a`.
    fn act(&mut self, a: Actor) -> Result<(), String> {
        match a {
            Actor::Token(i) => {
                self.load(i);
                let r = if self.forward != self.parity { self.fwd() } else { self.bwd() };
                self.store();
                r?
            }
            Actor::Fork(j) => self.fork_move(j)?,
        }
        self.clock += if self.forward { 1 } else { -1 };
        Ok(())
    }
    /// Whether nothing can move: the run is at the end it is heading for.
    pub fn done(&self) -> bool { self.actors().is_empty() }
    pub fn flip(&mut self) { self.forward = !self.forward; }
    /// One move: the first that can move (tokens by number, then forks) moves.
    pub fn step(&mut self) -> Result<bool, String> {
        self.fired.clear();
        let Some(&a) = self.actors().first() else { return Ok(false) };
        self.act(a)?;
        Ok(true)
    }
    /// One move by `a`, if it can move.
    pub fn step_by(&mut self, a: Actor) -> Result<bool, String> {
        self.fired.clear();
        if !self.can(a) { return Ok(false); }
        self.act(a)?;
        Ok(true)
    }
    /// One round: everything that can move at its start moves once (a fork sends off each half that
    /// waits); what the round makes waits for the next. Returns how many moves.
    pub fn round(&mut self) -> Result<usize, String> {
        self.fired.clear();
        self.rounds += 1;
        let mut n = 0;
        let mut served = std::collections::HashSet::new();
        for a in self.actors() {
            if let (true, Actor::Token(i)) = (self.exclusive, a) {
                if !served.insert(self.toks[i as usize].as_ref().unwrap().at.0) { continue; }
            }
            loop {
                let born = match a { Actor::Token(i) => self.toks[i as usize].as_ref().map(|t| t.born), Actor::Fork(j) => self.forks[j as usize].as_ref().map(|f| f.born) };
                if born.is_none_or(|b| b == self.rounds) || !self.can(a) { break; }
                self.act(a)?;
                n += 1;
                if matches!(a, Actor::Token(_)) { break; }
            }
        }
        Ok(n)
    }
    pub fn run(&mut self, most: u64) -> Result<u64, String> {
        let mut n = 0;
        while n < most && self.step()? { n += 1; }
        Ok(n)
    }

    /// Out of a definition's (or a run's) node: back to where it was called, as the stack says.
    fn leave(&mut self, r: u32) -> Result<(), String> {
        let f = self.pop().ok_or("left a definition with nothing on the stack")?;
        let (call, inv) = match f { Frame::Ret { call, inv } => (call, inv), Frame::Eval { call, inv, prog, code } => {
            if code != r { return Err("left a run's code at the wrong node".into()); }
            self.unplant(code);
            self.payload = self.dmk(DK::F, [prog, self.payload]);
            (call, inv)
        } Frame::Park(_) | Frame::Half { .. } => return Err("a tree, not a return point, on top of the stack".into()) };
        self.parity ^= inv;
        self.at = (call, if self.forward != self.parity { Way::Up } else { Way::Down });
        Ok(())
    }

    fn fwd(&mut self) -> Result<(), String> {
        use Way::*;
        let (n, way) = self.at;
        let ins = self.instr[&n].clone();
        if way == Down {
            if !matches!(ins.op, Op::Seq | Op::Note) { self.fired.push(n); }
            self.at = match ins.op {
                Op::Seq | Op::Note => if ins.kids.is_empty() { (n, Up) } else { (ins.kids[0], Down) },
                Op::Both => { self.open(n, Slot::Start)?; (n, Up) }
                Op::Re(l, r) => { self.rearrange(&l, &r)?; (n, Up) }
                Op::Dup(v) => { self.dup(v)?; (n, Up) }
                Op::On(p) => { self.split(&p)?; (ins.kids[0], Down) }
                Op::Case(cs) => {
                    let k = cs.iter().position(|(w, _)| self.fits(w, self.payload)).ok_or_else(|| format!("no arm takes {}", show(&self.carried())))?;
                    (ins.kids[k], Down)
                }
                Op::Loop(from, _) => { if !self.fits(&from, self.payload) { return Err(format!("{} does not start the loop", show(&self.carried()))); } (ins.kids[0], Down) }
                Op::Call(d, inv) => { self.push(Frame::Ret { call: n, inv }); self.parity ^= inv; (self.def_root[d], if inv { Up } else { Down }) }
                Op::Eval(inv) => { let c = self.open_eval(n, inv)?; self.parity ^= inv; (c, if inv { Up } else { Down }) }
                Op::Forget => { self.forget()?; (n, Up) }
            };
            return Ok(());
        }
        let Some((p, k)) = ins.up else { return self.leave(n) };
        let par = self.instr[&p].clone();
        self.at = match par.op {
            Op::Seq | Op::Note => if (k as usize) + 1 < par.kids.len() { (par.kids[k as usize + 1], Down) } else { (p, Up) },
            Op::On(path) => { self.fired.push(p); self.join(&path); (p, Up) }
            Op::Both => { self.fired.push(p); self.park(k, Slot::End)?; (p, Up) }
            Op::Case(cs) => {
                let g = &cs[k as usize].1;
                if !self.fits(g, self.payload) || cs[..k as usize].iter().any(|(_, g)| self.fits(g, self.payload)) {
                    return Err(format!("arm {k} gave {}, which would go back down another arm", show(&self.carried())));
                }
                (p, Up)
            }
            Op::Loop(from, until) => {
                self.fired.push(p);
                if k == 0 { if self.fits(&until, self.payload) { (p, Up) } else { (par.kids[1], Down) } }
                else { if self.fits(&from, self.payload) { return Err(format!("{} would start the loop again", show(&self.carried()))); } (par.kids[0], Down) }
            }
            _ => unreachable!("only these have parts"),
        };
        Ok(())
    }

    fn bwd(&mut self) -> Result<(), String> {
        use Way::*;
        let (n, way) = self.at;
        let ins = self.instr[&n].clone();
        if way == Up {
            if !matches!(ins.op, Op::Seq | Op::Note) { self.fired.push(n); }
            self.at = match ins.op {
                Op::Seq | Op::Note => if ins.kids.is_empty() { (n, Down) } else { (ins.kids[ins.kids.len() - 1], Up) },
                Op::Both => { self.open(n, Slot::End)?; (n, Down) }
                Op::Re(l, r) => { self.rearrange(&r, &l)?; (n, Down) }
                Op::Dup(v) => { self.dup(!v)?; (n, Down) }
                Op::On(p) => { self.split(&p)?; (ins.kids[0], Up) }
                Op::Case(cs) => {
                    let k = cs.iter().position(|(_, g)| self.fits(g, self.payload)).ok_or_else(|| format!("no arm gives {}", show(&self.carried())))?;
                    (ins.kids[k], Up)
                }
                Op::Loop(_, until) => { if !self.fits(&until, self.payload) { return Err(format!("{} does not end the loop", show(&self.carried()))); } (ins.kids[0], Up) }
                Op::Call(d, inv) => { self.push(Frame::Ret { call: n, inv }); self.parity ^= inv; (self.def_root[d], if inv { Down } else { Up }) }
                Op::Eval(inv) => { let c = self.open_eval(n, inv)?; self.parity ^= inv; (c, if inv { Down } else { Up }) }
                Op::Forget => return Err("what forget threw away cannot come back".into()),
            };
            return Ok(());
        }
        let Some((p, k)) = ins.up else { return self.leave(n) };
        let par = self.instr[&p].clone();
        self.at = match par.op {
            Op::Seq | Op::Note => if k > 0 { (par.kids[k as usize - 1], Up) } else { (p, Down) },
            Op::On(path) => { self.fired.push(p); self.join(&path); (p, Down) }
            Op::Both => { self.fired.push(p); self.park(k, Slot::Start)?; (p, Down) }
            Op::Case(cs) => {
                let w = &cs[k as usize].0;
                if !self.fits(w, self.payload) || cs[..k as usize].iter().any(|(w, _)| self.fits(w, self.payload)) {
                    return Err(format!("arm {k} came back to {}, which would have gone down another arm", show(&self.carried())));
                }
                (p, Down)
            }
            Op::Loop(from, until) => {
                self.fired.push(p);
                if k == 0 { if self.fits(&from, self.payload) { (p, Down) } else { (par.kids[1], Up) } }
                else { if self.fits(&until, self.payload) { return Err(format!("{} would have ended the loop", show(&self.carried()))); } (par.kids[0], Up) }
            }
            _ => unreachable!("only these have parts"),
        };
        Ok(())
    }

    /// Agents now: the program's, the trees the tokens carry and their stacks', and the stacks' frames
    /// and the forks.
    pub fn agents(&self) -> (usize, usize, usize) {
        let frames = |st: &[Frame]| st.iter().map(Frame::agents).sum::<usize>();
        (self.code.iter().flatten().count(), self.data.iter().flatten().count(),
         self.toks.iter().flatten().map(|t| frames(&t.stack)).sum::<usize>() + self.forks.iter().flatten().map(|f| 1 + frames(&f.stack)).sum::<usize>())
    }
    /// How many tokens there are.
    pub fn tokens(&self) -> usize { self.toks.iter().flatten().count() }
}
