# What block.html calls, built to out/view.mjs: the design's run as the
# harness lays it out, its ticks, what it reads as, and the calculus's answer.
# The page only draws what these give back.

import Base
import ./calculus.bend as TW
import ./blocks.bend as B
import ./harness.bend as H
import ./design.bend as Design

def fields() -> +List<Nat>:
  Design.fields()

def reactions() -> Nat:
  List.length(&2, B.Reaction, Design.reactions())

# the rule, indexed for ticking
def rule() -> +List<+List<B.Reaction>>:
  H.rule()

# the grid a run of p on x starts from, and its port; None outside the design
def start(p: TW.Tree, x: TW.Tree) -> +Maybe<B.Layout>:
  H.start(p, x)

def tick(+idx: +List<+List<B.Reaction>>, +g: +List<+List<+List<Nat>>>, shifted: Bool) -> B.Cut:
  B.tick(idx, H.void(), shifted, g)

def read(+g: +List<+List<+List<Nat>>>, +port: Nat) -> +Maybe<TW.Tree>:
  Design.read(g, port)

def moved(+u: TW.Tree, +t: TW.Tree) -> Bool:
  TW.moved(u, t)

# the calculus's answer
def reference(p: TW.Tree, x: TW.Tree) -> +Maybe<TW.Tree>:
  TW.run(256n, p, x, True{}, None{})
