//! Reversible rewriting of the tree-calculus interaction net (research/REVERSIBLE_PIPELINE.md). Every
//! rewrite is an event that can be undone exactly: it remembers the agents it consumed (their ids, so
//! undoing brings back the same agents) and, for each of their ports, either the consumed agent it was
//! wired to or the product port that took over its outside. The events and their links to what they
//! made are the run's history: which rewrite caused which (an occurrence net). An event can be undone
//! when everything it made is still there; independent events then undo in any order.
//!
//! Nothing is lost, so an eraser may take a value apart: that is events like any other, and what it
//! erased is in the history and comes back when they are undone. (Keeping erased values whole instead,
//! `erase: false`, stores about as much: fewer events on disp programs, more on the lambada benchmarks,
//! where a duplicator under a kept value goes on copying it for its other side.)
//! Where a rule joins two outside wires into one, a seam (`Tag::Seam`) marks the joined wire, so a
//! later rewrite that uses it depends on that event; interactions see straight through seams. Erasers
//! also clear up as the strand lattice's collection does, again as events:
//! - un-copy: an eraser on a duplicator's output; the duplicator becomes a seam from its input to its
//!   other output, and the value is never copied;
//! - dead work: an eraser on an apply's, triage's or dispatch's result; erasers go on its inputs instead;
//! - an unpair whose two outputs are both erased: one eraser on its input instead.
use crate::lattice::fresh_wanted_in;
use rust_ca_lattice::net::{Agent, Net, Ref};
use rust_ca_lattice::rules::{find_index_labelled, rule, End, Tag};

/// An agent as a rewrite consumed it, and whether it was wanted then.
#[derive(Clone, Debug)]
pub struct Was { pub id: u32, pub tag: Tag, pub label: u8, pub want: bool }

/// Where a consumed agent's port goes back to: another consumed agent's port, whatever is now at the
/// far end of a product's port (it took over the outside), or nothing.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Link { Inside(u8, u8), Handle(u32, u8), Free }

/// What an event did.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Kind {
    /// `rules::rule` number.
    Rule(usize),
    /// An eraser met a duplicator's output: the duplicator became a seam.
    Uncopy,
    /// An eraser met a computation's result: erasers on its inputs instead.
    Dead,
    /// Erasers on both outputs of an unpair: one on its input instead.
    Unpaired,
}

impl Kind {
    pub fn name(self) -> String {
        match self {
            Kind::Rule(i) => { let r = rule(i); format!("{}·{}", r.consumer.name(), r.producer.name()) }
            Kind::Uncopy => "un-copy".into(),
            Kind::Dead => "dead work".into(),
            Kind::Unpaired => "unpaired".into(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Event {
    pub kind: Kind,
    /// The agents consumed (for a rule: consumer, producer, then the seams between them), each with its ports' links.
    pub gone: Vec<(Was, [Link; 3])>,
    /// Products: fresh agents, then seams on wires it joined.
    pub made: Vec<u32>,
}

pub struct Rev {
    pub net: Net,
    pub want: Vec<bool>,
    /// Events not undone, by number (None once undone).
    pub events: Vec<Option<Event>>,
    /// The event that made each agent, if any (agents loaded with the program have none).
    pub maker: Vec<Option<u32>>,
    /// Forking S rules (`lattice::fresh_wanted_in`).
    pub fork: bool,
    /// Erasers take values apart (as events) rather than keeping them whole.
    pub erase: bool,
    /// Collection by erasers (un-copy, dead work, unpaired).
    pub collect: bool,
    /// Agents that may have something to do (`run`).
    todo: Vec<u32>,
    /// Events that may be undoable (`undo_any`), and the highest standing event is below `top`.
    ready: Vec<usize>,
    top: usize,
    /// Events undone that can be done again exactly (`redo`), by number; forgotten once a new event is made.
    pub undone: Vec<Option<Event>>,
    /// While redoing: the event's number and the ids it made before.
    redoing: Option<(usize, Vec<u32>)>,
    /// No event below this has a record in `undone`.
    redo_low: usize,
    /// Where each round (`round`) began, as an event number.
    pub rounds: Vec<usize>,
    /// While a round runs, its first event: what it makes cannot act till the next round.
    since: usize,
    /// A rewrite was put off till the next round.
    held: bool,
}

/// Makes the agents of a rewrite: new ids, or when redoing an event, the ids it made before.
struct Alloc<'a> { net: &'a mut Net, reuse: std::vec::IntoIter<u32> }
impl Alloc<'_> {
    fn mk(&mut self, tag: Tag, label: u8) -> u32 {
        let Some(id) = self.reuse.next() else { return self.net.mk_labelled(tag, label) };
        if self.net.agents.len() <= id as usize { self.net.agents.resize(id as usize + 1, None); }
        assert!(self.net.agents[id as usize].is_none(), "redoing onto a live agent");
        self.net.agents[id as usize] = Some(Agent { tag, label, ports: [None; 4] });
        id
    }
    /// A seam joining two outside ports.
    fn join(&mut self, a: Ref, b: Ref) -> u32 {
        let s = self.mk(Tag::Seam, 0);
        self.net.link(s, 0, a.0, a.1);
        self.net.link(s, 1, b.0, b.1);
        s
    }
}

impl Rev {
    /// A net to run reversibly; the normalizer at the root and the root start wanted.
    pub fn new(net: Net, fork: bool) -> Rev {
        let want = net.agents.iter().map(|a| matches!(a, Some(a) if matches!(a.tag, Tag::Nrm | Tag::Out))).collect();
        let maker = vec![None; net.agents.len()];
        let todo = (0..net.agents.len() as u32).rev().collect();
        Rev { net, want, events: vec![], maker, fork, erase: true, collect: true, todo, ready: vec![], top: 0, undone: vec![], redoing: None, redo_low: 0,
              rounds: vec![], since: usize::MAX, held: false }
    }

    pub fn agent(&self, id: u32) -> Option<&Agent> { self.net.agents.get(id as usize)?.as_ref() }
    fn port(&self, r: Ref) -> Option<Ref> { self.agent(r.0)?.ports[r.1 as usize] }
    fn tag(&self, id: u32) -> Option<Tag> { self.agent(id).map(|a| a.tag) }

    /// The far end of the wire from port r, through any seams, and the seams crossed.
    pub fn follow(&self, r: Ref) -> (Option<Ref>, Vec<u32>) {
        let (mut at, mut seams) = (self.port(r), vec![]);
        while let Some((id, q)) = at {
            if self.tag(id) != Some(Tag::Seam) || seams.len() > self.net.agents.len() { break; }
            seams.push(id);
            at = self.port((id, 1 - q));
        }
        (at, seams)
    }

    /// A consumer whose principal meets a producer's principal, through seams: (consumer, producer, seams).
    pub fn pair_at(&self, c: u32) -> Option<(u32, u32, Vec<u32>)> {
        let t = self.tag(c)?;
        if !t.is_consumer() || (t == Tag::Eps && !self.erase) { return None; }
        let (Some((p, 0)), seams) = self.follow((c, 0)) else { return None };
        self.tag(p)?.is_producer().then_some((c, p, seams))
    }

    /// Whether any of these agents was made in the round going on; if so the rewrite is put off.
    fn put_off(&mut self, ids: &[u32]) -> bool {
        let fresh = ids.iter().any(|&i| self.maker.get(i as usize).copied().flatten().is_some_and(|m| m as usize >= self.since));
        self.held |= fresh;
        fresh
    }

    /// Replace the agents `ids` by what `build` makes (it gets their ports as they were, wires its
    /// products to their outside and returns them), as an event. Returns its number.
    fn replace(&mut self, kind: Kind, ids: Vec<u32>, build: impl FnOnce(&mut Alloc, &[Agent]) -> Vec<u32>) -> usize {
        let snap: Vec<Agent> = ids.iter().map(|&i| self.agent(i).unwrap().clone()).collect();
        for &i in &ids { self.net.agents[i as usize] = None; }
        let reuse = self.redoing.as_ref().map_or(vec![], |r| r.1.clone()).into_iter();
        let made = build(&mut Alloc { net: &mut self.net, reuse }, &snap);
        // Each consumed port's way back: inside the consumed agents, or the product port now holding its outside.
        let gone = ids.iter().zip(&snap).map(|(&id, a)| {
            let mut links = [Link::Free; 3];
            for q in 0..a.tag.arity() {
                links[q] = match a.ports[q] {
                    None => Link::Free,
                    Some((o, oq)) => match ids.iter().position(|&x| x == o) {
                        Some(j) => Link::Inside(j as u8, oq),
                        None => { let h = self.port((o, oq)).expect("the outside was rewired to a product"); Link::Handle(h.0, h.1) }
                    },
                };
            }
            (Was { id, tag: a.tag, label: a.label, want: self.want.get(id as usize).copied().unwrap_or(false) }, links)
        }).collect();
        let n = match self.redoing.as_ref() {
            Some(&(e, _)) => e,
            None => {
                // A new event: what was undone is forgotten, and undone events at the end give up their numbers.
                self.undone.clear();
                while self.events.last().is_some_and(|e| e.is_none()) { self.events.pop(); }
                self.events.push(None);
                self.events.len() - 1
            }
        };
        self.want.resize(self.net.agents.len(), false);
        self.maker.resize(self.net.agents.len(), None);
        for &m in &made { self.maker[m as usize] = Some(n as u32); }
        self.events[n] = Some(Event { kind, gone, made });
        self.ready.push(n);
        self.top = self.top.max(n + 1);
        n
    }

    /// Rewrite the pair (c, p) met through `seams`, as an event. Returns its number.
    pub fn fire(&mut self, c: u32, p: u32, seams: Vec<u32>) -> usize {
        let (ca, pa) = (self.agent(c).unwrap(), self.agent(p).unwrap());
        let ri = find_index_labelled(ca.tag, ca.label, pa.tag, pa.label)
            .unwrap_or_else(|| panic!("no rule {}·{}", ca.tag.name(), pa.tag.name()));
        let (cl, pl) = (ca.label, pa.label);
        let r = rule(ri);
        let ids: Vec<u32> = [c, p].into_iter().chain(seams).collect();
        let e = self.replace(Kind::Rule(ri), ids, |net, snap| {
            let mut made: Vec<u32> = r.fresh.iter().enumerate().map(|(k, t)| net.mk(*t, r.label(k, cl, pl))).collect();
            // A template end, followed through the dying pair's own aux-to-aux wires, to a fresh port or an outside one.
            enum R { Fresh(u32, u8), Out(Ref) }
            let partner = |e: End| -> End {
                for (a, b) in r.wires { if *a == e { return *b; } if *b == e { return *a; } }
                unreachable!("every live port is wired")
            };
            let resolve = |start: End| -> R {
                let mut e = start;
                for _ in 0..64 {
                    match e {
                        End::Fresh(k, q) => return R::Fresh(made[k as usize], q),
                        End::CAux(i) | End::PAux(i) => {
                            let t = snap[matches!(e, End::PAux(_)) as usize].ports[i as usize].expect("aux wired");
                            e = match (t.0 == c, t.0 == p) { (true, _) => partner(End::CAux(t.1)), (_, true) => partner(End::PAux(t.1)), _ => return R::Out(t) };
                        }
                    }
                }
                panic!("vicious circle")
            };
            let ends: Vec<(R, R)> = r.wires.iter().map(|&(a, b)| (resolve(a), resolve(b))).collect();
            for (a, b) in ends {
                match (a, b) {
                    (R::Fresh(x, i), R::Fresh(y, j)) => net.net.link(x, i, y, j),
                    (R::Fresh(x, i), R::Out(t)) | (R::Out(t), R::Fresh(x, i)) => net.net.link(x, i, t.0, t.1),
                    (R::Out(t1), R::Out(t2)) => made.push(net.join(t1, t2)),
                }
            }
            made
        });
        // Fresh agents start wanted as the lattice has them (an eraser acts if erasing is on, wanted or not).
        let w = fresh_wanted_in(r, self.fork);
        for (k, &x) in self.events[e].as_ref().unwrap().made.iter().enumerate().take(r.fresh.len()) {
            self.want[x as usize] = w[k] && r.fresh[k] != Tag::Eps;
        }
        e
    }

    /// What an eraser can clear up where it is (see the module doc), as an event; None if nothing.
    pub fn collect_at(&mut self, e: u32) -> Option<usize> {
        if self.tag(e) != Some(Tag::Eps) { return None; }
        let (Some((d, q)), seams) = self.follow((e, 0)) else { return None };
        let td = self.tag(d)?;
        let ids: Vec<u32> = [e, d].into_iter().chain(seams).collect();
        if self.put_off(&ids) { return None; }
        let outside = |s: &Self, q: u8| s.agent(d).unwrap().ports[q as usize].filter(|o| !ids.contains(&o.0));
        match td {
            Tag::Dn if q > 0 => {
                let (input, other) = (outside(self, 0)?, outside(self, 3 - q)?);
                if input.0 == d || other.0 == d { return None; }
                Some(self.replace(Kind::Uncopy, ids, |net, _| vec![net.join(input, other)]))
            }
            Tag::A | Tag::T1 | Tag::Sel if q == 2 => {
                let (f, x) = (outside(self, 0)?, outside(self, 1)?);
                Some(self.replace(Kind::Dead, ids, |net, _| [f, x].iter().map(|t| { let k = net.mk(Tag::Eps, 0); net.net.link(k, 0, t.0, t.1); k }).collect()))
            }
            Tag::Unp if q > 0 => {
                let (Some((e2, 0)), seams2) = self.follow((d, 3 - q)) else { return None };
                if self.tag(e2) != Some(Tag::Eps) || e2 == e { return None; }
                let input = outside(self, 0)?;
                let ids: Vec<u32> = ids.into_iter().chain([e2]).chain(seams2).collect();
                if ids.contains(&input.0) || self.put_off(&ids) { return None; }
                Some(self.replace(Kind::Unpaired, ids, |net, _| { let k = net.mk(Tag::Eps, 0); net.net.link(k, 0, input.0, input.1); vec![k] }))
            }
            _ => None,
        }
    }

    /// Whether event e can be undone: every product is still there, unconsumed.
    pub fn undoable(&self, e: usize) -> bool {
        self.events.get(e).and_then(|e| e.as_ref()).is_some_and(|ev| ev.made.iter().all(|&m| self.agent(m).is_some()))
    }

    /// Undo event e: its products go, the agents it consumed come back, wired as they were inside and
    /// to whatever now holds the outside.
    pub fn undo(&mut self, e: usize) {
        assert!(self.undoable(e), "event {e} has a product that a later event consumed");
        let ev = self.events[e].take().unwrap();
        if self.undone.len() <= e { self.undone.resize(e + 1, None); }
        self.undone[e] = Some(ev.clone());
        self.redo_low = self.redo_low.min(e);
        let outside: Vec<[Option<Ref>; 3]> = ev.gone.iter().map(|(_, links)| links.map(|l| match l {
            Link::Handle(h, hq) => Some(self.port((h, hq)).expect("a product's port is wired")),
            _ => None,
        })).collect();
        for &m in &ev.made { self.net.agents[m as usize] = None; }
        for (was, _) in &ev.gone {
            self.net.agents[was.id as usize] = Some(Agent { tag: was.tag, label: was.label, ports: [None; 4] });
            self.want[was.id as usize] = was.want;
        }
        for (i, (was, links)) in ev.gone.iter().enumerate() {
            for q in 0..3 {
                match links[q] {
                    Link::Inside(j, jq) => self.net.link(was.id, q as u8, ev.gone[j as usize].0.id, jq),
                    Link::Handle(..) => { let x = outside[i][q].unwrap(); self.net.link(was.id, q as u8, x.0, x.1); }
                    Link::Free => {}
                }
            }
        }
        // What came back may act again, and the events that made it may be undoable now.
        for (was, _) in &ev.gone {
            self.todo.push(was.id);
            for q in 0..was.tag.arity() as u8 { if let (Some(r), _) = self.follow((was.id, q)) { self.todo.push(r.0); } }
            if let Some(m) = self.maker[was.id as usize] { self.ready.push(m as usize); }
        }
    }

    /// Do undone event e again, exactly: the same rewrite of the same agents, making agents with the
    /// ids it made before. Possible once the agents it used up are back.
    pub fn redo(&mut self, e: usize) -> bool {
        let Some(ev) = self.undone.get(e).cloned().flatten() else { return false };
        if ev.gone.iter().any(|(w, _)| self.agent(w.id).is_none()) { return false; }
        self.redoing = Some((e, ev.made.clone()));
        let first = ev.gone[0].0.id;
        let done = match ev.kind {
            Kind::Rule(_) => {
                let seams = ev.gone[2..].iter().map(|(w, _)| w.id).collect();
                Some(self.fire(first, ev.gone[1].0.id, seams))
            }
            _ => self.collect_at(first),
        };
        self.redoing = None;
        assert_eq!(done, Some(e), "event {e} did not happen again");
        debug_assert_eq!(self.events[e].as_ref().unwrap().made, ev.made);
        self.undone[e] = None;
        for &m in &ev.made { self.todo.push(m); for q in 0..self.agent(m).unwrap().tag.arity() as u8 { if let (Some(r), _) = self.follow((m, q)) { self.todo.push(r.0); } } }
        true
    }

    /// Redo the earliest undone event that can be done again (the next one, after undoing the latest). Returns it.
    pub fn redo_next(&mut self) -> Option<usize> {
        let e = self.next_undone()?;
        self.redo(e).then_some(e)
    }
    /// The earliest undone event that can be done again.
    fn next_undone(&mut self) -> Option<usize> {
        let at = self.undone.get(self.redo_low..)?.iter().position(|x| x.is_some())?;
        self.redo_low += at;
        Some(self.redo_low)
    }

    /// Go to the point where events before t are done and the rest undone: the standing events at or
    /// after t undone, latest first, and the undone ones before it done again, earliest first.
    pub fn seek(&mut self, t: usize) {
        loop {
            while self.top > 0 && self.events[self.top - 1].is_none() { self.top -= 1; }
            if self.top == 0 || self.top - 1 < t { break; }
            self.undo(self.top - 1);
        }
        while let Some(e) = self.next_undone().filter(|&e| e < t) { self.redo(e); }
    }

    /// Up to n events forward: what was undone done again while there is any, then new events. Returns how many.
    pub fn forward(&mut self, n: usize) -> usize {
        let mut k = 0;
        while k < n && self.redo_next().is_some() { k += 1; }
        if k < n && self.redo_low >= self.undone.len() { k += self.run(n - k); }
        k
    }

    /// Undo the latest event still standing (always undoable). Returns it.
    pub fn undo_latest(&mut self) -> Option<usize> {
        while self.top > 0 && self.events[self.top - 1].is_none() { self.top -= 1; }
        let e = self.top.checked_sub(1)?;
        self.undo(e);
        Some(e)
    }

    /// Undo an event chosen by `pick` (given how many to choose from) among those undoable now. Returns it.
    pub fn undo_any(&mut self, mut pick: impl FnMut(usize) -> usize) -> Option<usize> {
        while !self.ready.is_empty() {
            let k = pick(self.ready.len()).min(self.ready.len() - 1);
            let e = self.ready.swap_remove(k);
            if self.undoable(e) { self.undo(e); return Some(e); }
        }
        None
    }

    /// The rewrite agent i takes part in now, if any, as an event: collection by an eraser, or the pair
    /// it is in, wanted or not. Returns the event.
    pub fn fire_at(&mut self, i: u32) -> Option<usize> {
        if self.collect { if let Some(e) = self.collect_at(i) { return Some(e); } }
        let c = if self.tag(i)?.is_consumer() { i } else {
            let (Some((c, 0)), _) = self.follow((i, 0)) else { return None };
            c
        };
        let (c, p, seams) = self.pair_at(c)?;
        let e = self.fire(c, p, seams);
        let made = self.events[e].as_ref().unwrap().made.clone();
        for &m in &made { for q in 0..self.agent(m).unwrap().tag.arity() as u8 { if let (Some(r), _) = self.follow((m, q)) { self.todo.push(r.0); } } }
        self.todo.extend(made);
        Some(e)
    }

    /// The events that can be undone now.
    pub fn undoable_events(&self) -> Vec<usize> { (0..self.events.len()).filter(|&e| self.undoable(e)).collect() }

    /// One step forward from agent i, if one is there: collection by an eraser, a wanted pair, or demand
    /// passed on to the computation a wanted reader waits on. Returns the event, or the agent now wanted.
    fn step_at(&mut self, i: u32) -> Option<Result<usize, u32>> {
        let t = self.tag(i)?;
        if t == Tag::Eps && self.collect { if let Some(e) = self.collect_at(i) { return Some(Ok(e)); } }
        let active = self.want.get(i as usize).copied().unwrap_or(false) || t == Tag::Eps && self.erase;
        if !active || !t.is_consumer() { return None; }
        if let Some((c, p, seams)) = self.pair_at(i) {
            if self.put_off(&[[c, p].as_slice(), &seams].concat()) { return None; }
            return Some(Ok(self.fire(c, p, seams)));
        }
        let (Some((b, q)), _) = self.follow((i, 0)) else { return None };
        let bt = self.tag(b)?;
        if q > 0 && bt.is_consumer() && bt != Tag::Eps && crate::polarity_is_source(bt, q as usize) && !self.want[b as usize] {
            self.want[b as usize] = true;
            return Some(Err(b));
        }
        None
    }

    /// Run forward until nothing is left to do or `budget` events. Returns the events made.
    pub fn run(&mut self, budget: usize) -> usize {
        let mut todo = std::mem::take(&mut self.todo);
        let mut n = 0;
        while n < budget {
            let Some(i) = todo.pop() else { break };
            match self.step_at(i) {
                Some(Ok(e)) => {
                    n += 1;
                    // What the products meet, through seams, may have something to do now.
                    let made = self.events[e].as_ref().unwrap().made.clone();
                    for &m in &made {
                        for q in 0..self.agent(m).unwrap().tag.arity() as u8 { if let (Some(r), _) = self.follow((m, q)) { todo.push(r.0); } }
                    }
                    todo.extend(made);
                }
                Some(Err(b)) => { todo.push(i); todo.push(b); }
                None => {}
            }
        }
        self.todo = todo;
        n
    }

    /// One round: every rewrite there is now, at once. Only agents that were there when the round began
    /// take part (what the round makes waits for the next); demand passes on within it. Returns how many
    /// events it made.
    pub fn round(&mut self) -> usize {
        let start = self.events.len();
        self.rounds.retain(|&b| b < start);
        self.since = start;
        let (mut todo, mut later) = (std::mem::take(&mut self.todo), vec![]);
        let mut n = 0;
        while let Some(i) = todo.pop() {
            self.held = false;
            match self.step_at(i) {
                Some(Ok(e)) => {
                    n += 1;
                    let made = self.events[e].as_ref().unwrap().made.clone();
                    for &m in &made {
                        for q in 0..self.agent(m).unwrap().tag.arity() as u8 { if let (Some(r), _) = self.follow((m, q)) { later.push(r.0); } }
                    }
                    later.extend(made);
                }
                Some(Err(b)) => { todo.push(i); todo.push(b); }
                None if self.held => later.push(i),
                None => {}
            }
        }
        self.since = usize::MAX;
        self.todo = later;
        if n > 0 { self.rounds.push(start); }
        n
    }

    /// Whether `run` has looked at everything there is to do (it stopped short of its budget).
    pub fn idle(&self) -> bool { self.todo.is_empty() }

    /// The root's agent id.
    pub fn root(&self) -> Option<u32> { self.net.agents.iter().position(|a| matches!(a, Some(a) if a.tag == Tag::Out)).map(|i| i as u32) }

    /// The value at the root, read through seams, if it is a value tree.
    pub fn answer(&self) -> Option<rust_ca_lattice::oracle::Term> { self.value(self.follow((self.root()?, 0)).0) }
    fn value(&self, r: Option<Ref>) -> Option<rust_ca_lattice::oracle::Term> {
        use rust_ca_lattice::oracle::Term;
        use std::rc::Rc;
        let (id, 0) = r? else { return None };
        let kid = |q: usize| self.value(self.follow((id, q as u8)).0);
        Some(match self.tag(id)? { Tag::L => Term::L, Tag::S => Term::S(Rc::new(kid(1)?)), Tag::F => Term::F(Rc::new(kid(1)?), Rc::new(kid(2)?)), _ => return None })
    }

    /// Agents the root reaches without passing an eraser: the answer, or the work toward it.
    pub fn reached(&self) -> Vec<bool> {
        let mut seen = vec![false; self.net.agents.len()];
        let mut stack: Vec<u32> = self.root().into_iter().collect();
        while let Some(i) = stack.pop() {
            if std::mem::replace(&mut seen[i as usize], true) { continue; }
            for p in self.agent(i).unwrap().ports.iter().flatten() { if self.tag(p.0).is_some_and(|t| t != Tag::Eps) { stack.push(p.0); } }
        }
        seen
    }

    /// Events still standing.
    pub fn live_events(&self) -> usize { self.events.iter().flatten().count() }
}
