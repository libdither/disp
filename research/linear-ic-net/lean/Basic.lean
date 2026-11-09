/-! Shared by the rule tables and the net model: port polarity and the ends of a rule's wires. -/

/-- A port's polarity: a value flows in through it, or out through it. -/
inductive Pol | i | o
  deriving DecidableEq, Repr

def Pol.flip : Pol → Pol | .i => .o | .o => .i

/-- An end of a wire in a rule's right-hand side. `left n` and `right n` stand for whatever was
plugged into auxiliary port n of the rule's left or right agent; `new j n` is port n of the j-th
agent the rule makes. -/
inductive End
  | left (n : Nat) | right (n : Nat) | new (j n : Nat)
  deriving DecidableEq, Repr

def End.isSlot : End → Bool | .new .. => false | _ => true
