# The first block design. A tree is laid out as nodes joined by wires; a
# program goes in as tokens down the port column, one at a time, and each
# comes back up as an echo that lets the next one go.
#
# A cell is [kind, class, symbol, arity, orient, state]:
#   kind    0 empty, 1 wire down, 2 wire across, 3 tape, 4 node, 5 spare
#   class   its signal: 0 none, 1 going down, 2 going up, 3 inner: S a probe, U a
#           let-go, M, N and 13 turned back, T and A going on down (Q is A past
#           an upper spare), and A's check: L asks, R is a leaf, X is not
#   symbol  0 none, 1 L, 2 R, 3 S, 4 X, 5 U, 6 M, 7 T, 8 N, 9 A, 10 Q; 11 to 13
#           are on no token, only on what tokens turn into
#   arity   (nodes) 0 leaf, 1 stem, 2 fork
#   orient  (forks) 1 when the fork's parts are swapped; (spares, stems) 1 for
#           an upper spare, or a stem made from one
#   state   (nodes) 0 idle, 1 routed down, 2 routed across, 3 refusing
#
# Signals move by trades. In a wire two cells of a block trade signals, so a
# signal keeps going the way the alternating blocks carry it: down (or right)
# from a cell on the tick's parity, up (or left) from one off it. A wire is
# then two lanes, one each way, and signals going opposite ways pass.
#
# A node sits at a cell whose column and row have the same parity, so it is
# alternately the bottom right of a block (with the wire above it) and the top
# left (with its parts: the wire down below it, and for a fork the wire across
# to its right, whose end turns down at an empty corner into the right part).
# A node holds one signal: it trades it with the wire above on the first kind
# of block, and on the second it acts on it, or trades it with its routed part.
#
# A program of swaps, stems, units, maps, ; and † is a list of steps, each at a
# path into the tree, so it goes in as packets: a path of L (a fork's left
# part), R (its right part) and S (a stem's part), then the step, then a U for
# each step of the path. A path token at an idle node routes it to that part
# and goes on down as a probe, which the part turns back; a fork or stem whose
# kind does not fit refuses, turning back the rest of the packet until a U. U
# at the end of the path turns back as a let-go, which unroutes the routed node
# above it. U's past the root leave harmlessly. The steps are X (swap), M (stem),
# T (stem†), N (unit), A (unit†) and Q (a stem on the part of a stem).
#
# Tokens wait in the tape, the column above the tree, and only one is out at
# a time: every token comes back up as exactly one echo, which, reaching the
# tape, takes the place of the lowest held token and lets it go, then rises
# through the tape and out the top. A node meets one signal at a time, and a
# run ends with every echo gone.
#
# The rule undoes itself with no reaction that turns a node routed while
# sending a signal up: that could not be told from a routed node passing an
# echo up (and an echo must pass every routed node above it). A routing token
# goes on down as the probe instead.
#
# New matter comes from slack kept in the layout. The root and each part of a
# fork sit under two spares, nodes that read as a plain wire and pass signals
# like one: an upper one, with a box on its right (a wire across to a lower
# spare, a wire and a leaf), and a lower one. A chain of stems shares the
# spares above its top, so a stem always sits right above a node. M and N pass
# the spares, turn back at the first node, and the nearest spare that fits
# changes: M makes any spare a stem, N makes the upper one a flipped fork whose
# left part is the box. A reversible rule cannot let a spare send on a signal
# it passes anyway (each signal must have one past), so T and A send theirs
# down to the next node, which sends it back up; so a stem is only ever taken
# back with a node right below it.
#
# Stuck runs: T on a fork or leaf, and A on a stem, a leaf, or a fork whose left
# part is not a leaf, come back as echoes with the tree unchanged.
#
# Out of room: the slack is fixed, so a program can ask for more than a place
# has (a third stem, a second unit, a unit under a stem or on a box's leaf, a
# stem taken from under a routed stem and then the one above it, A on a fork
# laid unflipped). Its signal then has nowhere to go and bounces forever: the
# run never goes still, rather than reading as a wrong tree.

import Base
import ./calculus.bend as TW
import ./blocks.bend as B

def fields() -> +List<Nat>:
  [6n, 4n, 14n, 3n, 2n, 4n]

# Reactions
# ---------

def lit(+n: Nat) -> B.Field:
  B.Is{n}

def var(+n: Nat) -> B.Field:
  B.Any{n}

# a wire or tape cell of kind k with signal (class c, symbol s)
def sig(+k: Nat, c: B.Field, s: B.Field) -> +List<B.Field>:
  [B.Is{k}, c, s, B.Is{0n}, B.Is{0n}, B.Is{0n}]

def node(a: B.Field, o: B.Field, st: B.Field, c: B.Field, s: B.Field) -> +List<B.Field>:
  [B.Is{4n}, c, s, a, o, st]

def spare(o: B.Field, c: B.Field, s: B.Field) -> +List<B.Field>:
  [B.Is{5n}, c, s, B.Is{0n}, o, B.Is{0n}]

def none() -> +List<B.Field>:
  [B.Is{0n}, B.Is{0n}, B.Is{0n}, B.Is{0n}, B.Is{0n}, B.Is{0n}]

# a cell left as it is, its fields named v to v+5
def any(+v: Nat) -> +List<B.Field>:
  [B.Any{v}, B.Any{1n+v}, B.Any{2n+v}, B.Any{3n+v}, B.Any{4n+v}, B.Any{5n+v}]

# a reaction: a block fitting either side turns into the other
def rx(nw: +List<B.Field>, ne: +List<B.Field>, sw: +List<B.Field>, se: +List<B.Field>, nw2: +List<B.Field>, ne2: +List<B.Field>, sw2: +List<B.Field>, se2: +List<B.Field>) -> B.Reaction:
  B.Reaction{B.Quad{nw, ne, sw, se}, B.Quad{nw2, ne2, sw2, se2}}

# the common ones first, so most blocks find theirs early (no two reactions
# disagree on a block, so the order changes nothing else)
def reactions() -> +List<B.Reaction>:
  [
  # a node or a spare trades its signal with the wire above it
  rx(none(), sig(1n, var(0n), var(1n)), none(), node(var(2n), var(3n), var(4n), var(5n), var(6n)),
     none(), sig(1n, var(5n), var(6n)), none(), node(var(2n), var(3n), var(4n), var(0n), var(1n))),
  rx(none(), sig(1n, var(0n), var(1n)), none(), spare(var(2n), var(3n), var(4n)),
     none(), sig(1n, var(3n), var(4n)), none(), spare(var(2n), var(0n), var(1n))),
  # a wire down trades signals along itself, on either side of a block
  rx(sig(1n, var(0n), var(1n)), none(), sig(1n, var(2n), var(3n)), none(),
     sig(1n, var(2n), var(3n)), none(), sig(1n, var(0n), var(1n)), none()),
  rx(none(), sig(1n, var(0n), var(1n)), none(), sig(1n, var(2n), var(3n)),
     none(), sig(1n, var(2n), var(3n)), none(), sig(1n, var(0n), var(1n))),
  # an echo rises through the tape, every held token slipping down a cell
  rx(none(), sig(3n, var(0n), var(1n)), none(), sig(3n, lit(2n), var(2n)),
     none(), sig(3n, lit(2n), var(2n)), none(), sig(3n, var(0n), var(1n))),
  rx(sig(3n, var(0n), var(1n)), none(), sig(3n, lit(2n), var(2n)), none(),
     sig(3n, lit(2n), var(2n)), none(), sig(3n, var(0n), var(1n)), none()),
  rx(none(), sig(3n, var(0n), var(1n)), none(), sig(3n, lit(3n), var(2n)),
     none(), sig(3n, lit(3n), var(2n)), none(), sig(3n, var(0n), var(1n))),
  rx(sig(3n, var(0n), var(1n)), none(), sig(3n, lit(3n), var(2n)), none(),
     sig(3n, lit(3n), var(2n)), none(), sig(3n, var(0n), var(1n)), none()),
  # and on up through empty cells, and out of the grid
  rx(none(), none(), sig(3n, lit(2n), var(0n)), none(),
     sig(3n, lit(2n), var(0n)), none(), none(), none()),
  rx(none(), none(), none(), sig(3n, lit(2n), var(0n)),
     none(), sig(3n, lit(2n), var(0n)), none(), none()),
  rx(none(), none(), sig(3n, lit(3n), var(0n)), none(),
     sig(3n, lit(3n), var(0n)), none(), none(), none()),
  rx(none(), none(), none(), sig(3n, lit(3n), var(0n)),
     none(), sig(3n, lit(3n), var(0n)), none(), none()),
  # a corner (an empty cell) passes signals between the wire across and the wire down
  rx(sig(2n, var(0n), var(1n)), none(), none(), sig(1n, var(2n), var(3n)),
     sig(2n, var(2n), var(3n)), none(), none(), sig(1n, var(0n), var(1n))),
  # a wire across, likewise, on either row of a block
  rx(none(), none(), sig(2n, var(0n), var(1n)), sig(2n, var(2n), var(3n)),
     none(), none(), sig(2n, var(2n), var(3n)), sig(2n, var(0n), var(1n))),
  rx(sig(2n, var(0n), var(1n)), sig(2n, var(2n), var(3n)), none(), none(),
     sig(2n, var(2n), var(3n)), sig(2n, var(0n), var(1n)), none(), none()),
  # a wire across trades with a node or spare right after it (a box)
  rx(none(), none(), sig(2n, var(0n), var(1n)), node(var(2n), var(3n), var(4n), var(5n), var(6n)),
     none(), none(), sig(2n, var(5n), var(6n)), node(var(2n), var(3n), var(4n), var(0n), var(1n))),
  rx(none(), none(), sig(2n, var(0n), var(1n)), spare(var(2n), var(3n), var(4n)),
     none(), none(), sig(2n, var(3n), var(4n)), spare(var(2n), var(0n), var(1n))),
  # an echo reaching the tape takes the place of its lowest held token, which goes down the wire
  rx(sig(3n, lit(1n), var(0n)), none(), sig(1n, lit(2n), var(1n)), none(),
     sig(3n, lit(2n), var(1n)), none(), sig(1n, lit(1n), var(0n)), none()),
  rx(sig(3n, lit(1n), var(0n)), none(), sig(1n, lit(3n), lit(5n)), none(),
     sig(3n, lit(3n), lit(5n)), none(), sig(1n, lit(1n), var(0n)), none()),
  # or, the tape all let go, just goes up into it (of class 3, only a let-go may)
  rx(none(), none(), sig(1n, lit(2n), var(0n)), none(),
     sig(3n, lit(2n), var(0n)), none(), sig(1n, lit(0n), lit(0n)), none()),
  rx(none(), none(), sig(1n, lit(3n), lit(5n)), none(),
     sig(3n, lit(3n), lit(5n)), none(), sig(1n, lit(0n), lit(0n)), none()),
  # a node routed down passes signals down its wire, and echoes up from it; a let-go from below unroutes it, and its route goes up as an echo
  rx(node(var(0n), var(1n), lit(1n), lit(1n), var(2n)), any(3n), sig(1n, lit(0n), lit(0n)), none(),
     node(var(0n), var(1n), lit(1n), lit(0n), lit(0n)), any(3n), sig(1n, lit(1n), var(2n)), none()),
  rx(node(var(0n), var(1n), lit(1n), lit(0n), lit(0n)), any(2n), sig(1n, lit(2n), var(8n)), none(),
     node(var(0n), var(1n), lit(1n), lit(2n), var(8n)), any(2n), sig(1n, lit(0n), lit(0n)), none()),
  rx(node(var(0n), var(1n), lit(1n), lit(0n), lit(0n)), any(2n), sig(1n, lit(3n), lit(5n)), none(),
     node(var(0n), var(1n), lit(0n), lit(2n), lit(1n)), any(2n), sig(1n, lit(0n), lit(0n)), none()),
  # a fork routed across, likewise for its wire across
  rx(node(lit(2n), var(0n), lit(2n), lit(1n), var(1n)), sig(2n, lit(0n), lit(0n)), any(2n), none(),
     node(lit(2n), var(0n), lit(2n), lit(0n), lit(0n)), sig(2n, lit(1n), var(1n)), any(2n), none()),
  rx(node(lit(2n), var(0n), lit(2n), lit(0n), lit(0n)), sig(2n, lit(2n), var(1n)), any(2n), none(),
     node(lit(2n), var(0n), lit(2n), lit(2n), var(1n)), sig(2n, lit(0n), lit(0n)), any(2n), none()),
  rx(node(lit(2n), var(0n), lit(2n), lit(0n), lit(0n)), sig(2n, lit(3n), lit(5n)), any(1n), none(),
     node(lit(2n), var(0n), lit(0n), lit(2n), lit(2n)), sig(2n, lit(0n), lit(0n)), any(1n), none()),
  # an idle node turns U back as a let-go, for the node routed above it
  rx(node(var(0n), var(1n), lit(0n), lit(1n), lit(5n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(0n), lit(3n), lit(5n)), any(2n), any(8n), none()),
  # a probe meets an idle node and goes back up
  rx(node(var(0n), var(1n), lit(0n), lit(3n), lit(3n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(0n), lit(2n), lit(3n)), any(2n), any(8n), none()),
  # a refusing node turns every token but U back unused
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(1n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(3n), lit(2n), lit(1n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(2n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(3n), lit(2n), lit(2n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(3n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(3n), lit(2n), lit(3n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(4n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(3n), lit(2n), lit(4n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(6n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(3n), lit(2n), lit(6n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(7n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(3n), lit(2n), lit(7n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(8n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(3n), lit(2n), lit(8n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(9n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(3n), lit(2n), lit(9n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(10n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(3n), lit(2n), lit(10n)), any(2n), any(8n), none()),
  # U ends its refusing
  rx(node(var(0n), var(1n), lit(3n), lit(1n), lit(5n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(0n), lit(2n), lit(5n)), any(2n), any(8n), none()),
  # an idle fork routes on L and R, by its orient, and sends the token on
  rx(node(lit(2n), lit(0n), lit(0n), lit(1n), lit(1n)), any(0n), sig(1n, lit(0n), lit(0n)), none(),
     node(lit(2n), lit(0n), lit(1n), lit(0n), lit(0n)), any(0n), sig(1n, lit(3n), lit(3n)), none()),
  rx(node(lit(2n), lit(1n), lit(0n), lit(1n), lit(1n)), sig(2n, lit(0n), lit(0n)), any(0n), none(),
     node(lit(2n), lit(1n), lit(2n), lit(0n), lit(0n)), sig(2n, lit(3n), lit(3n)), any(0n), none()),
  rx(node(lit(2n), lit(0n), lit(0n), lit(1n), lit(2n)), sig(2n, lit(0n), lit(0n)), any(0n), none(),
     node(lit(2n), lit(0n), lit(2n), lit(0n), lit(0n)), sig(2n, lit(3n), lit(3n)), any(0n), none()),
  rx(node(lit(2n), lit(1n), lit(0n), lit(1n), lit(2n)), any(0n), sig(1n, lit(0n), lit(0n)), none(),
     node(lit(2n), lit(1n), lit(1n), lit(0n), lit(0n)), any(0n), sig(1n, lit(3n), lit(3n)), none()),
  # an idle fork refuses S, and swaps on X
  rx(node(lit(2n), var(0n), lit(0n), lit(1n), lit(3n)), any(1n), any(7n), none(),
     node(lit(2n), var(0n), lit(3n), lit(2n), lit(5n)), any(1n), any(7n), none()),
  rx(node(lit(2n), lit(0n), lit(0n), lit(1n), lit(4n)), any(0n), any(6n), none(),
     node(lit(2n), lit(1n), lit(0n), lit(2n), lit(4n)), any(0n), any(6n), none()),
  rx(node(lit(2n), lit(1n), lit(0n), lit(1n), lit(4n)), any(0n), any(6n), none(),
     node(lit(2n), lit(0n), lit(0n), lit(2n), lit(4n)), any(0n), any(6n), none()),
  # an idle stem routes on S; it refuses L and R; X does nothing to it
  rx(node(lit(1n), var(0n), lit(0n), lit(1n), lit(3n)), any(1n), sig(1n, lit(0n), lit(0n)), none(),
     node(lit(1n), var(0n), lit(1n), lit(0n), lit(0n)), any(1n), sig(1n, lit(3n), lit(3n)), none()),
  rx(node(lit(1n), var(0n), lit(0n), lit(1n), lit(1n)), any(1n), any(7n), none(),
     node(lit(1n), var(0n), lit(3n), lit(2n), lit(5n)), any(1n), any(7n), none()),
  rx(node(lit(1n), var(0n), lit(0n), lit(1n), lit(2n)), any(1n), any(7n), none(),
     node(lit(1n), var(0n), lit(3n), lit(2n), lit(11n)), any(1n), any(7n), none()),
  rx(node(lit(1n), var(0n), lit(0n), lit(1n), lit(4n)), any(1n), any(7n), none(),
     node(lit(1n), var(0n), lit(0n), lit(2n), lit(4n)), any(1n), any(7n), none()),
  # an idle leaf refuses L, R and S; X does nothing to it
  rx(node(lit(0n), lit(0n), lit(0n), lit(1n), lit(1n)), any(0n), any(6n), none(),
     node(lit(0n), lit(0n), lit(3n), lit(2n), lit(5n)), any(0n), any(6n), none()),
  rx(node(lit(0n), lit(0n), lit(0n), lit(1n), lit(2n)), any(0n), any(6n), none(),
     node(lit(0n), lit(0n), lit(3n), lit(2n), lit(11n)), any(0n), any(6n), none()),
  rx(node(lit(0n), lit(0n), lit(0n), lit(1n), lit(3n)), any(0n), any(6n), none(),
     node(lit(0n), lit(0n), lit(3n), lit(2n), lit(12n)), any(0n), any(6n), none()),
  rx(node(lit(0n), lit(0n), lit(0n), lit(1n), lit(4n)), any(0n), any(6n), none(),
     node(lit(0n), lit(0n), lit(0n), lit(2n), lit(4n)), any(0n), any(6n), none()),
  # a spare passes signals down and up like a wire, all but the ones that change it
  rx(spare(var(0n), lit(1n), var(1n)), any(2n), sig(1n, lit(0n), lit(0n)), none(),
     spare(var(0n), lit(0n), lit(0n)), any(2n), sig(1n, lit(1n), var(1n)), none()),
  rx(spare(var(0n), lit(2n), var(1n)), any(2n), sig(1n, lit(0n), lit(0n)), none(),
     spare(var(0n), lit(0n), lit(0n)), any(2n), sig(1n, lit(2n), var(1n)), none()),
  rx(spare(var(0n), lit(3n), lit(3n)), any(1n), sig(1n, lit(0n), lit(0n)), none(),
     spare(var(0n), lit(0n), lit(0n)), any(1n), sig(1n, lit(3n), lit(3n)), none()),
  rx(spare(var(0n), lit(3n), lit(5n)), any(1n), sig(1n, lit(0n), lit(0n)), none(),
     spare(var(0n), lit(0n), lit(0n)), any(1n), sig(1n, lit(3n), lit(5n)), none()),
  rx(spare(var(0n), lit(3n), lit(1n)), any(1n), sig(1n, lit(0n), lit(0n)), none(),
     spare(var(0n), lit(0n), lit(0n)), any(1n), sig(1n, lit(3n), lit(1n)), none()),
  rx(spare(var(0n), lit(3n), lit(2n)), any(1n), sig(1n, lit(0n), lit(0n)), none(),
     spare(var(0n), lit(0n), lit(0n)), any(1n), sig(1n, lit(3n), lit(2n)), none()),
  rx(spare(var(0n), lit(3n), lit(4n)), any(1n), sig(1n, lit(0n), lit(0n)), none(),
     spare(var(0n), lit(0n), lit(0n)), any(1n), sig(1n, lit(3n), lit(4n)), none()),
  rx(spare(lit(0n), lit(3n), lit(8n)), any(0n), sig(1n, lit(0n), lit(0n)), none(),
     spare(lit(0n), lit(0n), lit(0n)), any(0n), sig(1n, lit(3n), lit(8n)), none()),
  rx(spare(lit(0n), lit(3n), lit(9n)), any(0n), sig(1n, lit(0n), lit(0n)), none(),
     spare(lit(0n), lit(0n), lit(0n)), any(0n), sig(1n, lit(3n), lit(9n)), none()),
  rx(spare(lit(0n), lit(3n), lit(10n)), any(0n), sig(1n, lit(0n), lit(0n)), none(),
     spare(lit(0n), lit(0n), lit(0n)), any(0n), sig(1n, lit(3n), lit(10n)), none()),
  rx(spare(lit(1n), lit(3n), lit(9n)), any(0n), sig(1n, lit(0n), lit(0n)), none(),
     spare(lit(1n), lit(0n), lit(0n)), any(0n), sig(1n, lit(3n), lit(10n)), none()),
  # M and N turn back at the first node they meet
  rx(node(var(0n), var(1n), lit(0n), lit(1n), lit(6n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(0n), lit(3n), lit(6n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(0n), lit(1n), lit(8n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(0n), lit(3n), lit(8n)), any(2n), any(8n), none()),
  # the turned back M makes the nearest spare a stem
  rx(spare(var(0n), lit(0n), lit(0n)), any(1n), sig(1n, lit(3n), lit(6n)), none(),
     node(lit(1n), var(0n), lit(0n), lit(2n), lit(6n)), any(1n), sig(1n, lit(0n), lit(0n)), none()),
  # Q, a stem on the part of a stem: only a stem turns it back, and it is made above that stem (a stem above or below reads the same); forks and leaves refuse it
  rx(node(lit(1n), var(0n), lit(0n), lit(1n), lit(10n)), any(1n), any(7n), none(),
     node(lit(1n), var(0n), lit(0n), lit(3n), lit(13n)), any(1n), any(7n), none()),
  rx(spare(var(0n), lit(0n), lit(0n)), any(1n), sig(1n, lit(3n), lit(13n)), none(),
     node(lit(1n), var(0n), lit(0n), lit(2n), lit(10n)), any(1n), sig(1n, lit(0n), lit(0n)), none()),
  rx(node(lit(2n), var(0n), lit(0n), lit(1n), lit(10n)), any(1n), any(7n), none(),
     node(lit(2n), var(0n), lit(0n), lit(2n), lit(10n)), any(1n), any(7n), none()),
  rx(node(lit(0n), var(0n), lit(0n), lit(1n), lit(10n)), any(1n), any(7n), none(),
     node(lit(0n), var(0n), lit(0n), lit(2n), lit(10n)), any(1n), any(7n), none()),
  # T makes an idle stem a spare, and goes on down to the next node, which sends it back up; forks and leaves refuse it
  rx(node(lit(1n), var(0n), lit(0n), lit(1n), lit(7n)), any(1n), sig(1n, lit(0n), lit(0n)), none(),
     spare(var(0n), lit(0n), lit(0n)), any(1n), sig(1n, lit(3n), lit(7n)), none()),
  rx(node(var(0n), var(1n), lit(0n), lit(3n), lit(7n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(0n), lit(2n), lit(11n)), any(2n), any(8n), none()),
  rx(node(lit(2n), var(0n), lit(0n), lit(1n), lit(7n)), any(1n), any(7n), none(),
     node(lit(2n), var(0n), lit(0n), lit(2n), lit(7n)), any(1n), any(7n), none()),
  rx(node(lit(0n), var(0n), lit(0n), lit(1n), lit(7n)), any(1n), any(7n), none(),
     node(lit(0n), var(0n), lit(0n), lit(2n), lit(7n)), any(1n), any(7n), none()),
  # the turned back N makes the nearest upper spare a fork, its box the left part
  rx(spare(lit(1n), lit(0n), lit(0n)), any(0n), sig(1n, lit(3n), lit(8n)), none(),
     node(lit(2n), lit(1n), lit(0n), lit(2n), lit(8n)), any(0n), sig(1n, lit(0n), lit(0n)), none()),
  # A first asks if the left part (the part across) is a leaf
  rx(node(lit(2n), lit(1n), lit(0n), lit(1n), lit(9n)), sig(2n, lit(0n), lit(0n)), any(0n), none(),
     node(lit(2n), lit(1n), lit(2n), lit(0n), lit(0n)), sig(2n, lit(3n), lit(1n)), any(0n), none()),
  rx(node(lit(0n), lit(0n), lit(0n), lit(3n), lit(1n)), any(0n), any(6n), none(),
     node(lit(0n), lit(0n), lit(0n), lit(3n), lit(2n)), any(0n), any(6n), none()),
  rx(node(lit(1n), var(0n), lit(0n), lit(3n), lit(1n)), any(1n), any(7n), none(),
     node(lit(1n), var(0n), lit(0n), lit(3n), lit(4n)), any(1n), any(7n), none()),
  rx(node(lit(2n), var(0n), lit(0n), lit(3n), lit(1n)), any(1n), any(7n), none(),
     node(lit(2n), var(0n), lit(0n), lit(3n), lit(4n)), any(1n), any(7n), none()),
  # if so the fork goes spare, and A goes on down to the next node, past at most an upper spare, which sends it back up
  rx(node(lit(2n), lit(1n), lit(2n), lit(0n), lit(0n)), sig(2n, lit(3n), lit(2n)), sig(1n, lit(0n), lit(0n)), none(),
     spare(lit(1n), lit(0n), lit(0n)), sig(2n, lit(0n), lit(0n)), sig(1n, lit(3n), lit(9n)), none()),
  # if not, the tree stays and A goes back up; an unflipped fork asks the same below it, and with a leaf there is out of room (nothing answers)
  rx(node(lit(2n), lit(1n), lit(2n), lit(0n), lit(0n)), sig(2n, lit(3n), lit(4n)), any(0n), none(),
     node(lit(2n), lit(1n), lit(0n), lit(2n), lit(13n)), sig(2n, lit(0n), lit(0n)), any(0n), none()),
  rx(node(var(0n), var(1n), lit(0n), lit(3n), lit(9n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(0n), lit(2n), lit(12n)), any(2n), any(8n), none()),
  rx(node(var(0n), var(1n), lit(0n), lit(3n), lit(10n)), any(2n), any(8n), none(),
     node(var(0n), var(1n), lit(0n), lit(2n), lit(9n)), any(2n), any(8n), none()),
  rx(node(lit(2n), lit(0n), lit(0n), lit(1n), lit(9n)), any(0n), sig(1n, lit(0n), lit(0n)), none(),
     node(lit(2n), lit(0n), lit(1n), lit(0n), lit(0n)), any(0n), sig(1n, lit(3n), lit(1n)), none()),
  rx(node(lit(2n), lit(0n), lit(1n), lit(0n), lit(0n)), any(0n), sig(1n, lit(3n), lit(4n)), none(),
     node(lit(2n), lit(0n), lit(0n), lit(2n), lit(13n)), any(0n), sig(1n, lit(0n), lit(0n)), none()),
  rx(node(lit(1n), var(0n), lit(0n), lit(1n), lit(9n)), any(1n), any(7n), none(),
     node(lit(1n), var(0n), lit(0n), lit(2n), lit(13n)), any(1n), any(7n), none()),
  rx(node(lit(0n), var(0n), lit(0n), lit(1n), lit(9n)), any(1n), any(7n), none(),
     node(lit(0n), var(0n), lit(0n), lit(2n), lit(13n)), any(1n), any(7n), none())
  ]

# Layout
# ------

def cell(+k: Nat, +c: Nat, +s: Nat, +a: Nat, +o: Nat, +st: Nat) -> +List<Nat>:
  [k, c, s, a, o, st]

def empty() -> +List<Nat>:
  cell(0n, 0n, 0n, 0n, 0n, 0n)

def wire(+k: Nat) -> +List<Nat>:
  cell(k, 0n, 0n, 0n, 0n, 0n)

# a cell placed at (x, y)
type Put is Data:
  Put{x: Nat, y: Nat, c: +List<Nat>}

# how far right of a fork its right part goes: past the left part and an empty
# column, by an odd step, so the corner is on the other parity and the right
# part's node on the node parity
def gap(+w: Nat) -> Nat:
  +g = Nat.max(3n, 1n+w)
  Nat.add(g, Nat.mod(1n+g, 2n))

# the columns a laid out tree takes (a leaf's take in the box of its spares)
def width(t: TW.Tree) -> Nat:
  match t:
    case TW.L{}:
      3n
    case TW.S{c}:
      width(c)
    case TW.F{TW.L{}, r}:
      Nat.add(gap(width(r)), 3n)
    case TW.F{l, r}:
      Nat.add(gap(width(l)), width(r))

def across(n: Nat, +x: Nat, +y: Nat) -> +List<Put>:
  match n:
    case 0n:
      Nil{}
    case 1n+k:
      Put{x, y, wire(2n)} <> across(k, 1n+x, y)

# the two spares above a part at (x, y + 4): the upper with its box, the lower
def spares(+x: Nat, +y: Nat) -> +List<Put>:
  [Put{x, y, cell(5n, 0n, 0n, 0n, 1n, 0n)}, Put{1n+x, y, wire(2n)}, Put{2n+x, y, cell(5n, 0n, 0n, 0n, 0n, 0n)}, Put{2n+x, 1n+y, wire(1n)}, Put{2n+x, 2n+y, cell(4n, 0n, 0n, 0n, 0n, 0n)},
   Put{x, 1n+y, wire(1n)}, Put{x, 2n+y, cell(5n, 0n, 0n, 0n, 0n, 0n)}, Put{x, 3n+y, wire(1n)}]

# a fork at (x, y), of orient o, whose part below is at (x, y + 6) and part
# across at (cx, y + 7), both under their spares
def fork_at(+o: Nat, +x: Nat, +y: Nat, +cx: Nat, below: +List<Put>, right: +List<Put>) -> +List<Put>:
  Put{x, y, cell(4n, 0n, 0n, 2n, o, 0n)} <> Put{x, 1n+y, wire(1n)} <> List.append(&2, Put, spares(x, 2n+y), List.append(&2, Put, below, List.append(&2, Put, across(Nat.sub(cx, 1n+x), 1n+x, y), Put{cx, 1n+y, wire(1n)} <> Put{cx, 2n+y, wire(1n)} <> List.append(&2, Put, spares(cx, 3n+y), right))))

# a tree with its node at (x, y): a stem's part right below it, a fork's parts
# under their spares. A fork with a leaf for its left part is laid flipped: the
# other part below, the leaf across
def place(t: TW.Tree, +x: Nat, +y: Nat) -> +List<Put>:
  match t:
    case TW.L{}:
      [Put{x, y, cell(4n, 0n, 0n, 0n, 0n, 0n)}]
    case TW.S{c}:
      Put{x, y, cell(4n, 0n, 0n, 1n, 0n, 0n)} <> Put{x, 1n+y, wire(1n)} <> place(c, x, 2n+y)
    case TW.F{TW.L{}, +r}:
      +cx = Nat.add(x, gap(width(r)))
      fork_at(1n, x, y, cx, place(r, x, 6n+y), place(TW.L{}, cx, 7n+y))
    case TW.F{+l, r}:
      +cx = Nat.add(x, gap(width(l)))
      fork_at(0n, x, y, cx, place(l, x, 6n+y), place(r, cx, 7n+y))

def found(ps: +List<Put>, +x: Nat, +y: Nat) -> +List<Nat>:
  match ps:
    case Nil{}:
      empty()
    case Put{px, py, +c} <> t:
      B.pick_cell(Bool.and(Nat.is_eq(px, x), Nat.is_eq(py, y)), c, found(t, x, y))

def render_row(w: Nat, +x: Nat, +y: Nat, +ps: +List<Put>) -> +List<+List<Nat>>:
  match w:
    case 0n:
      Nil{}
    case 1n+k:
      found(ps, x, y) <> render_row(k, 1n+x, y, ps)

def render(h: Nat, +y: Nat, +w: Nat, +ps: +List<Put>) -> +List<+List<+List<Nat>>>:
  match h:
    case 0n:
      Nil{}
    case 1n+k:
      render_row(w, 0n, y, ps) <> render(k, 1n+y, w, ps)

def most_x(ps: +List<Put>) -> Nat:
  match ps:
    case Nil{}:
      0n
    case Put{x, y, c} <> t:
      Nat.max(x, most_x(t))

def most_y(ps: +List<Put>) -> Nat:
  match ps:
    case Nil{}:
      0n
    case Put{x, y, c} <> t:
      Nat.max(y, most_y(t))

def even_up(+n: Nat) -> Nat:
  Nat.add(n, Nat.mod(n, 2n))

# the root under its spares at the top of column 1, the port (the tape above
# it is an odd number of rows, so in a run the upper spare is on an odd row and
# column)
def lay(x: TW.Tree) -> B.Layout:
  +ps = List.append(&2, Put, spares(1n, 0n), place(x, 1n, 4n))
  B.Layout{render(even_up(1n+most_y(ps)), 0n, even_up(1n+most_x(ps)), ps), 1n}

# Programs
# --------

# the step run the other way: M and T, N and A undo each other (Q is M on a part)
def last_back(+s: Nat) -> +List<Nat>:
  match s:
    case 6n:
      [7n]
    case 7n:
      [6n]
    case 8n:
      [9n]
    case 9n:
      [8n]
    case 10n:
      [3n, 7n]
    case _:
      [s]

def back(q: +List<Nat>) -> +List<Nat>:
  match q:
    case Nil{}:
      Nil{}
    case +s <> Nil{}:
      last_back(s)
    case +s <> t:
      s <> back(t)

def backs(qs: +List<+List<Nat>>) -> +List<+List<Nat>>:
  match qs:
    case Nil{}:
      Nil{}
    case q <> t:
      back(q) <> backs(t)

def flip(m: +Maybe<+List<+List<Nat>>>) -> +Maybe<+List<+List<Nat>>>:
  match m:
    case Some{qs}:
      Some{backs(List.reverse(&2, +List<Nat>, qs))}
    case None{}:
      None{}

def both(a: +Maybe<+List<+List<Nat>>>, b: +Maybe<+List<+List<Nat>>>) -> +Maybe<+List<+List<Nat>>>:
  match a b:
    case Some{x} Some{y}:
      Some{List.append(&2, +List<Nat>, x, y)}
    case _ _:
      None{}

def is_m(q: +List<Nat>) -> Bool:
  match q:
    case 6n <> Nil{}:
      True{}
    case _:
      False{}

# a packet sent one step down, by d (M under S is the one token Q)
def under_one(+d: Nat, +q: +List<Nat>) -> +List<Nat>:
  B.pick_cell(Bool.and(Nat.is_eq(d, 3n), is_m(q)), [10n], d <> q)

def under(+d: Nat, qs: +List<+List<Nat>>) -> +List<+List<Nat>>:
  match qs:
    case Nil{}:
      Nil{}
    case +q <> t:
      under_one(d, q) <> under(d, t)

def under_maybe(+d: Nat, m: +Maybe<+List<+List<Nat>>>) -> +Maybe<+List<+List<Nat>>>:
  match m:
    case Some{qs}:
      Some{under(d, qs)}
    case None{}:
      None{}

# a program as packets, each a path then a step; None outside the design. †
# runs the packets the other way round, each step undone; a map sends its
# parts' packets under S, L and R
def packets(p: TW.Tree) -> +Maybe<+List<+List<Nat>>>:
  match p:
    case TW.L{}:
      Some{Nil{}}
    case TW.S{TW.S{TW.L{}}}:
      Some{[[4n]]}
    case TW.S{TW.S{TW.S{TW.S{TW.S{TW.L{}}}}}}:
      Some{[[6n]]}
    case TW.S{TW.S{TW.S{TW.S{TW.S{TW.S{TW.L{}}}}}}}:
      Some{[[8n]]}
    case TW.F{TW.L{}, b}:
      flip(packets(b))
    case TW.F{TW.S{a}, b}:
      both(packets(a), packets(b))
    case TW.S{TW.F{TW.F{a, b}, c}}:
      both(under_maybe(3n, packets(a)), both(under_maybe(1n, packets(b)), under_maybe(2n, packets(c))))
    case _:
      None{}

def lets_go(n: Nat) -> +List<Nat>:
  match n:
    case 0n:
      Nil{}
    case 1n+k:
      5n <> lets_go(k)

# each packet with a U per step of its path
def tokens(qs: +List<+List<Nat>>) -> +List<Nat>:
  match qs:
    case Nil{}:
      Nil{}
    case +q <> t:
      List.append(&2, Nat, q, List.append(&2, Nat, lets_go(Nat.sub(List.length(&2, Nat, q), 1n)), tokens(t)))

def held(ts: +List<Nat>) -> +List<+List<Nat>>:
  match ts:
    case Nil{}:
      Nil{}
    case s <> t:
      cell(3n, 1n, s, 0n, 0n, 0n) <> held(t)

# the first token on the wire going down, the rest held in the tape above
# it, the last at the top
def column(ts: +List<Nat>) -> +List<+List<Nat>>:
  match ts:
    case Nil{}:
      Nil{}
    case s <> t:
      List.append(&2, +List<Nat>, List.reverse(&2, +List<Nat>, held(t)), [cell(1n, 1n, s, 0n, 0n, 0n)])

def odd_or(b: Bool, cs: +List<+List<Nat>>) -> +List<+List<Nat>>:
  match b:
    case True{}:
      cs
    case False{}:
      empty() <> cs

# an odd number of rows, so the first token is on an even row, going down at
# tick 0
def odd(+cs: +List<+List<Nat>>) -> +List<+List<Nat>>:
  odd_or(Nat.is_eq(Nat.mod(List.length(&2, +List<Nat>, cs), 2n), 1n), cs)

def inject_of(m: +Maybe<+List<+List<Nat>>>) -> +Maybe<+List<+List<Nat>>>:
  match m:
    case Some{qs}:
      Some{odd(column(tokens(qs)))}
    case None{}:
      None{}

def inject(p: TW.Tree) -> +Maybe<+List<+List<Nat>>>:
  inject_of(packets(p))

# Reading
# -------

def row_at(g: +List<+List<+List<Nat>>>, n: Nat) -> +List<+List<Nat>>:
  match g n:
    case Nil{} _:
      Nil{}
    case +r <> t 0n:
      r
    case r <> t 1n+k:
      row_at(t, k)

def cell_at(r: +List<+List<Nat>>, n: Nat) -> +List<Nat>:
  match r n:
    case Nil{} _:
      empty()
    case +c <> t 0n:
      c
    case c <> t 1n+k:
      cell_at(t, k)

def at(+g: +List<+List<+List<Nat>>>, +x: Nat, +y: Nat) -> +List<Nat>:
  cell_at(row_at(g, y), x)

def fork_of(o: Nat, d: +Maybe<TW.Tree>, r: +Maybe<TW.Tree>) -> +Maybe<TW.Tree>:
  match o:
    case 0n:
      TW.then_fork(d, r)
    case 1n+k:
      TW.then_fork(r, d)

def is_empty(c: +List<Nat>) -> Bool:
  match c:
    case 0n <> t:
      True{}
    case _:
      False{}

# the tree reached from (x, y), whose cell is c, by following wires: down an
# empty cell, the tape, a wire down or a spare; right along a wire across, and
# down past its end if a corner is there; a node is the tree's top
def walk(fuel: Nat, +g: +List<+List<+List<Nat>>>, +x: Nat, +y: Nat, c: +List<Nat>) -> +Maybe<TW.Tree>:
  match fuel:
    case 0n:
      None{}
    case 1n++k:
      match c:
        case 0n <> t:
          walk(k, g, x, 1n+y, at(g, x, 1n+y))
        case 1n <> t:
          walk(k, g, x, 1n+y, at(g, x, 1n+y))
        case 3n <> t:
          walk(k, g, x, 1n+y, at(g, x, 1n+y))
        case 5n <> t:
          walk(k, g, x, 1n+y, at(g, x, 1n+y))
        case 2n <> t:
          +y2 = B.pick(is_empty(at(g, 1n+x, y)), 1n+y, y)
          walk(k, g, 1n+x, y2, at(g, 1n+x, y2))
        case 4n <> cl <> s <> 0n <> t:
          Some{TW.L{}}
        case 4n <> cl <> s <> 1n <> t:
          TW.then_stem(walk(k, g, x, 1n+y, at(g, x, 1n+y)))
        case 4n <> cl <> s <> 2n <> o <> t:
          fork_of(o, walk(k, g, x, 1n+y, at(g, x, 1n+y)), walk(k, g, 1n+x, y, at(g, 1n+x, y)))
        case _:
          None{}

# the tree under the port column: the tape and wire above the root are passed
# over
def read(+g: +List<+List<+List<Nat>>>, +port: Nat) -> +Maybe<TW.Tree>:
  walk(Nat.add(2n, Nat.add(List.length(&2, +List<+List<Nat>>, g), B.width(g))), g, port, 0n, at(g, port, 0n))
