# The proofs of LAWS.bend: a case per piece, or per mode, each by computation.
import Base
import ./tetris.bend as Tetris
import ./LAWS.bend as Laws

def Laws.four_turns(kind):
  match kind:
    case Tetris.I{}:
      {==}
    case Tetris.O{}:
      {==}
    case Tetris.T{}:
      {==}
    case Tetris.S{}:
      {==}
    case Tetris.Z{}:
      {==}
    case Tetris.J{}:
      {==}
    case Tetris.L{}:
      {==}

def Laws.spawns_fit(kind):
  match kind:
    case Tetris.I{}:
      {==}
    case Tetris.O{}:
      {==}
    case Tetris.T{}:
      {==}
    case Tetris.S{}:
      {==}
    case Tetris.Z{}:
      {==}
    case Tetris.J{}:
      {==}
    case Tetris.L{}:
      {==}

def Laws.r_restarts(g):
  match g:
    case Tetris.Game{rows, piece, next, bag, seed, score, lines, clock, mode}:
      match mode:
        case Tetris.Play{}:
          {==}
        case Tetris.Pause{}:
          {==}
        case Tetris.Over{}:
          {==}

def Laws.clears_a_row():
  {==}

def Laws.drop_scores_a_line():
  {==}
