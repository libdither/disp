//! Ternary interchange codec (preorder arity: leaf `"0"`, stem `"1"`+child, fork
//! `"2"`+l+r) — the one string copy per term across the FFI. `parse` builds producer
//! nodes; `emit` serializes an already-normalized producer tree. Both iterate over an
//! explicit frame/work stack so a deep term can't overflow the wasm shadow stack.

use crate::net::Ctx;
use crate::port::*;

impl<'a> Ctx<'a> {
    /// Encode an already-normalized producer tree to ternary (iterative).
    pub(crate) fn emit(&self, root: u32, out: &mut Vec<u8>) {
        let mut stack = vec![root];
        while let Some(p) = stack.pop() {
            let p = self.resolve(p);
            match tag(p) {
                L => out.push(b'0'),
                S => {
                    out.push(b'1');
                    stack.push(self.nd(val(p) as u32));
                }
                F => {
                    out.push(b'2');
                    stack.push(self.nd(val(p) as u32 + 1)); // right popped after left
                    stack.push(self.nd(val(p) as u32));
                }
                _ => out.push(b'0'), // unreachable after full_nf; defensive
            }
        }
    }

    /// Parse one preorder ternary term (leaf "0", stem "1"+child, fork "2"+l+r).
    pub(crate) fn parse(&mut self, s: &[u8], i: &mut usize) -> u32 {
        enum Frame {
            Stem,
            ForkL,
            ForkR(u32),
        }
        let mut stack: Vec<Frame> = Vec::new();
        loop {
            let mut value = loop {
                if *i >= s.len() {
                    break self.leaf();
                }
                let ch = s[*i];
                *i += 1;
                match ch {
                    b'1' => stack.push(Frame::Stem),
                    b'2' => stack.push(Frame::ForkL),
                    _ => break self.leaf(),
                }
            };
            loop {
                match stack.pop() {
                    None => return value,
                    Some(Frame::Stem) => value = self.stem(value),
                    Some(Frame::ForkL) => {
                        stack.push(Frame::ForkR(value));
                        break;
                    }
                    Some(Frame::ForkR(left)) => value = self.fork(left, value),
                }
            }
        }
    }
}
