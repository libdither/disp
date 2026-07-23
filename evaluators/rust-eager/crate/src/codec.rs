//! Ternary interchange codec (preorder arity: leaf `"0"`, stem `"1"`+child, fork
//! `"2"`+l+r). This is the one string copy per term across the FFI (EVALUATOR.md).

use crate::arena::{Arena, Exhausted, Node, LEAF_ID};

impl Arena {
    /// Parse one preorder ternary term. Iterative (explicit frame stack) so a deep input
    /// can't overflow the wasm shadow stack on this FFI-reachable path — see
    /// .cargo/config.toml. Frames record what each pending parent still needs.
    pub(crate) fn parse(&mut self, s: &[u8], i: &mut usize) -> u32 {
        enum Frame {
            Stem,       // a stem awaiting its child
            ForkL,      // a fork awaiting its left child
            ForkR(u32), // a fork whose left child (carried) is built; awaiting the right
        }
        let mut stack: Vec<Frame> = Vec::new();
        loop {
            // Descend: read tokens, pushing frames, until a token completes a value.
            let mut value = loop {
                if *i >= s.len() {
                    break LEAF_ID; // truncated input ⇒ leaf
                }
                let c = s[*i];
                *i += 1;
                match c {
                    b'1' => stack.push(Frame::Stem),
                    b'2' => stack.push(Frame::ForkL),
                    _ => break LEAF_ID, // b'0' or unknown ⇒ a leaf value
                }
            };
            // Ascend: attach `value` to parent frames, completing stems and full forks.
            loop {
                match stack.pop() {
                    None => return value, // the whole term is built
                    Some(Frame::Stem) => value = self.stem(value),
                    Some(Frame::ForkL) => {
                        stack.push(Frame::ForkR(value)); // left done; go build the right
                        break;
                    }
                    Some(Frame::ForkR(left)) => value = self.fork(left, value),
                }
            }
        }
    }

    /// Encode an already-normalized value to ternary. `h` must be Susp-free (`nf` first).
    /// Recursive — test-only (`tc_dump_ternary` uses the iterative `dump_emit`).
    pub(crate) fn encode(&self, h: u32, out: &mut Vec<u8>) {
        match self.node(h) {
            Node::Leaf => out.push(b'0'),
            Node::Stem(c) => {
                out.push(b'1');
                self.encode(c, out);
            }
            Node::Fork(l, r) => {
                out.push(b'2');
                self.encode(l, out);
                self.encode(r, out);
            }
            Node::Susp(..) => out.push(b'0'), // unreachable after nf; defensive
        }
    }

    /// Force `h` to full NF and emit ternary directly — WITHOUT materializing the NF tree
    /// (a big result streams to `out` instead of being built as nodes; iterative, so deep
    /// results don't recurse). For eager handles `whnf` is the identity, so this is an
    /// iterative encode; lazy handles force as they descend.
    pub(crate) fn dump_emit(&mut self, h: u32, out: &mut Vec<u8>, budget: &mut i64) -> Result<(), Exhausted> {
        let mut stack: Vec<u32> = vec![h];
        while let Some(n) = stack.pop() {
            let w = self.whnf(n, budget)?;
            match self.node(w) {
                Node::Leaf => out.push(b'0'),
                Node::Stem(c) => {
                    out.push(b'1');
                    stack.push(c);
                }
                Node::Fork(l, r) => {
                    out.push(b'2');
                    stack.push(r); // pushed first ⇒ popped after the left child
                    stack.push(l);
                }
                Node::Susp(..) => unreachable!("whnf returns a constructor"),
            }
        }
        Ok(())
    }
}
