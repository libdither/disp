# The laws: what must stay true of the game however its code changes.
# PROOF.bend proves them, and `bend PROOF.bend` fails as soon as one breaks.
import Base
import ./tetris.bend as Tetris

# LAW: four quarter turns bring every piece back to its spawn shape.
law four_turns:
  for kind: Tetris.Tetromino
  {Tetris.turns(4n, Tetris.box(kind), Tetris.shape(kind)) == Tetris.shape(kind) : List<&2, Tetris.Cell>}

# LAW: every piece spawns fitting on an empty board.
law spawns_fit:
  for kind: Tetris.Tetromino
  {Tetris.fits(List.replicate(U32, 20n, 0), Tetris.cells(Tetris.spawn(kind))) == True{} : Bool}

# LAW: R starts a fresh game, at zero points, from any game.
law r_restarts:
  for g: Tetris.Game
  {Tetris.score(Tetris.press(114, g)) == 0 : U32}

# LAW: a full row (153391689 is ten I blocks) clears, and the row above drops
# into its place.
law clears_a_row:
  {Tetris.clear(List.append(&2, U32, List.replicate(U32, 18n, 0), [5, 153391689]))
    == (1, List.append(&2, U32, List.replicate(U32, 19n, 0), [5])) : U32 & List<&2, U32>}

# LAW: an I hard-dropped from the top into the four-wide gap of the bottom row
# (37449 is six I blocks on the left) scores 136: 100 for the line at level 1,
# and 2 for each of the 18 rows it fell.
law drop_scores_a_line:
  {Tetris.score(Tetris.hard_drop(Tetris.Game{List.append(&2, U32, List.replicate(U32, 19n, 0), [37449]),
    Tetris.Piece{Tetris.I{}, 0, 0, 6}, Tetris.O{}, 0, 1, 0, 0, 0, Tetris.Play{}})) == 136 : U32}
