# Tetris in Bend.
#
# The game is two pure functions, `step(game, key)` and `draw(game)`.
# `main` is the only part that does IO: read a byte from stdin, step, redraw, repeat.
# A 0 byte is a gravity tick; the host (host/host.c in a terminal, or the web page) sends one every 50 ms.

object Piece { kind: u24, rot: u24, row: u24, col: u24 }

type Mode:
  Playing
  Paused
  Over

# board: 20 rows of 10 cells, 0 is empty and k+1 is a block of piece kind k.
# bag: the kinds left in the current shuffled set of all 7. clock: ticks since the piece last fell.
object Game { board: List(List(u24)), piece: Piece, next: u24, bag: List(u24), seed: u24, score: u24, lines: u24, clock: u24, mode: Mode }

TICK : u24 = 0

# Cell codes besides 0 (empty) and k+1 (a block of kind k), used only for drawing.
GHOST : u24 = 8
BLANK : u24 = 9
WALL : u24 = 10

# Pieces
# ------

# Each kind's spawn cells (row, col) inside the n×n box it turns in: I O T S Z J L.
def shape(kind: u24) -> (u24, List((u24, u24))):
  switch kind:
    case 0:
      return (4, [(1, 0), (1, 1), (1, 2), (1, 3)])
    case 1:
      return (2, [(0, 0), (0, 1), (1, 0), (1, 1)])
    case 2:
      return (3, [(0, 1), (1, 0), (1, 1), (1, 2)])
    case 3:
      return (3, [(0, 1), (0, 2), (1, 0), (1, 1)])
    case 4:
      return (3, [(0, 0), (0, 1), (1, 1), (1, 2)])
    case 5:
      return (3, [(0, 0), (1, 0), (1, 1), (1, 2)])
    case _:
      return (3, [(0, 2), (1, 0), (1, 1), (1, 2)])

# Turns cells a quarter clockwise inside an n×n box, `times` times.
def turn(n: u24, cells: List((u24, u24)), times: u24) -> List((u24, u24)):
  switch times:
    case 0:
      return cells
    case _:
      return turn(n, [turn_cell(n, cell) for cell in cells], times-1)

def turn_cell(n: u24, cell: (u24, u24)) -> (u24, u24):
  (r, c) = cell
  return (c, n - 1 - r)

def offset(cell: (u24, u24), row: u24, col: u24) -> (u24, u24):
  (r, c) = cell
  return (r + row, c + col)

def cells(p: Piece) -> List((u24, u24)):
  open Piece: p
  (n, base) = shape(p.kind)
  return [offset(cell, p.row, p.col) for cell in turn(n, base, p.rot)]

def place(p: Piece, row: u24, col: u24, rot: u24) -> Piece:
  open Piece: p
  return Piece { kind: p.kind, rot: rot % 4, row: row, col: col }

def spawn(kind: u24) -> Piece:
  if kind == 1:
    return Piece { kind: kind, rot: 0, row: 0, col: 4 }
  else:
    return Piece { kind: kind, rot: 0, row: 0, col: 3 }

# The board
# ---------

def empty_rows(n: u24) -> List(List(u24)):
  bend i = 0:
    when i < n:
      rows = List/Cons([0, 0, 0, 0, 0, 0, 0, 0, 0, 0], fork(i + 1))
    else:
      rows = List/Nil
  return rows

def at(xs: List(T), i: u24) -> T:
  match xs:
    case List/Cons:
      switch i:
        case 0:
          return xs.head
        case _:
          return at(xs.tail, i-1)
    case List/Nil:
      return unreachable()

def update(xs: List(T), i: u24, f: T -> T) -> List(T):
  match xs:
    case List/Cons:
      switch i:
        case 0:
          return List/Cons(f(xs.head), xs.tail)
        case _:
          return List/Cons(xs.head, update(xs.tail, i-1, f))
    case List/Nil:
      return List/Nil

# Each row as a number with bit c set when column c is filled, for cheap collision tests.
def solid(board: List(List(u24))) -> List(u24):
  return [row_bits(row) for row in board]

def row_bits(row: List(u24)) -> u24:
  fold row:
    case List/Cons:
      return (row.head != 0) | (row.tail << 1)
    case List/Nil:
      return 0

# Columns left of the board wrap around to huge u24 values, so `c < 10` checks both walls.
def free(rows: List(u24), cell: (u24, u24)) -> u24:
  (r, c) = cell
  if (r < 20) & (c < 10):
    return ((at(rows, r) >> c) & 1) == 0
  else:
    return 0

def fits(rows: List(u24), cs: List((u24, u24))) -> u24:
  match cs:
    case List/Cons:
      return free(rows, cs.head) & fits(rows, cs.tail)
    case List/Nil:
      return 1

def stamp(board: List(List(u24)), cs: List((u24, u24)), code: u24) -> List(List(u24)):
  match cs:
    case List/Cons:
      (r, c) = cs.head
      board = update(board, r, lambda row: update(row, c, lambda old: code))
      return stamp(board, cs.tail, code)
    case List/Nil:
      return board

def has_gap(row: List(u24)) -> u24:
  fold row:
    case List/Cons:
      return (row.head == 0) | row.tail
    case List/Nil:
      return 0

# Removes full rows and refills from the top, returning how many were cleared.
def clear_lines(board: List(List(u24))) -> (u24, List(List(u24))):
  kept = [row for row in board if has_gap(row)]
  (n, kept) = List/length(kept)
  return (20 - n, List/concat(empty_rows(20 - n), kept))

# Falls until the next row down is blocked.
def landing(rows: List(u24), p: Piece) -> Piece:
  open Piece: p
  below = place(p, p.row + 1, p.col, p.rot)
  if fits(rows, cells(below)):
    return landing(rows, below)
  else:
    return p

# Randomness
# ----------

def next_seed(seed: u24) -> u24:
  return seed * 1664525 + 7271263

def take(xs: List(u24), i: u24) -> (u24, List(u24)):
  match xs:
    case List/Cons:
      switch i:
        case 0:
          return (xs.head, xs.tail)
        case _:
          (x, rest) = take(xs.tail, i-1)
          return (x, List/Cons(xs.head, rest))
    case List/Nil:
      return (0, List/Nil)

# Takes a random kind out of the bag, refilling it with all 7 when it runs out.
def pick(bag: List(u24), seed: u24) -> (u24, List(u24), u24):
  (n, bag) = List/length(bag)
  if n == 0:
    return pick([0, 1, 2, 3, 4, 5, 6], seed)
  else:
    seed = next_seed(seed)
    (kind, bag) = take(bag, (seed >> 12) % n)
    return (kind, bag, seed)

# Rules
# -----

def level(lines: u24) -> u24:
  return lines / 10 + 1

def gravity_ticks(lines: u24) -> u24:
  if level(lines) < 15:
    return 16 - level(lines)
  else:
    return 1

def line_score(cleared: u24) -> u24:
  switch cleared:
    case 0:
      return 0
    case 1:
      return 100
    case 2:
      return 300
    case 3:
      return 500
    case _:
      return 800

def new_game(seed: u24) -> Game:
  (first, bag, seed) = pick([], seed)
  (next, bag, seed) = pick(bag, seed)
  return Game { board: empty_rows(20), piece: spawn(first), next: next, bag: bag, seed: seed, score: 0, lines: 0, clock: 0, mode: Mode/Playing }

def with_piece(g: Game, p: Piece) -> Game:
  open Game: g
  return Game { board: g.board, piece: p, next: g.next, bag: g.bag, seed: g.seed, score: g.score, lines: g.lines, clock: g.clock, mode: g.mode }

def with_clock(g: Game, clock: u24) -> Game:
  open Game: g
  return Game { board: g.board, piece: g.piece, next: g.next, bag: g.bag, seed: g.seed, score: g.score, lines: g.lines, clock: clock, mode: g.mode }

def with_mode(g: Game, mode: Mode) -> Game:
  open Game: g
  return Game { board: g.board, piece: g.piece, next: g.next, bag: g.bag, seed: g.seed, score: g.score, lines: g.lines, clock: g.clock, mode: mode }

def add_score(g: Game, points: u24) -> Game:
  open Game: g
  return Game { board: g.board, piece: g.piece, next: g.next, bag: g.bag, seed: g.seed, score: g.score + points, lines: g.lines, clock: g.clock, mode: g.mode }

# Moves to the first candidate that fits, or stays put.
def first_fit(g: Game, candidates: List(Piece)) -> Game:
  match candidates:
    case List/Cons:
      open Game: g
      if fits(solid(g.board), cells(candidates.head)):
        return with_piece(g, candidates.head)
      else:
        return first_fit(g, candidates.tail)
    case List/Nil:
      return g

# Rotation tries the spot in place, then nudges sideways off walls and stacks.
def rotate(g: Game) -> Game:
  open Game: g
  p = g.piece
  open Piece: p
  r = p.rot + 1
  return first_fit(g, [place(p, p.row, p.col, r), place(p, p.row, p.col - 1, r), place(p, p.row, p.col + 1, r), place(p, p.row, p.col - 2, r), place(p, p.row, p.col + 2, r)])

def shift(g: Game, col: u24) -> Game:
  open Game: g
  p = g.piece
  open Piece: p
  return first_fit(g, [place(p, p.row, col, p.rot)])

# Merges the piece into the board, clears and scores lines, and brings in the next piece.
def lock(g: Game) -> Game:
  open Game: g
  p = g.piece
  open Piece: p
  board = stamp(g.board, cells(p), p.kind + 1)
  (cleared, board) = clear_lines(board)
  score = g.score + line_score(cleared) * level(g.lines)
  piece = spawn(g.next)
  (next, bag, seed) = pick(g.bag, g.seed)
  if fits(solid(board), cells(piece)):
    mode = Mode/Playing
  else:
    mode = Mode/Over
  return Game { board: board, piece: piece, next: next, bag: bag, seed: seed, score: score, lines: g.lines + cleared, clock: 0, mode: mode }

def fall(g: Game) -> Game:
  open Game: g
  p = g.piece
  open Piece: p
  below = place(p, p.row + 1, p.col, p.rot)
  if fits(solid(g.board), cells(below)):
    return with_piece(g, below)
  else:
    return lock(g)

def hard_drop(g: Game) -> Game:
  open Game: g
  p = g.piece
  open Piece: p
  landed = landing(solid(g.board), p)
  open Piece: landed
  return lock(add_score(with_piece(g, landed), 2 * (landed.row - p.row)))

def tick(g: Game) -> (u24, Game):
  open Game: g
  if g.clock + 1 < gravity_ticks(g.lines):
    return (0, with_clock(g, g.clock + 1))
  else:
    return (1, fall(with_clock(g, 0)))

# Arrow keys arrive as ESC [ A..D; the ESC and [ fall through as ignored keys.
def is_left(key: u24) -> u24:
  return (key == 'a') | (key == 'h') | (key == 'D')

def is_right(key: u24) -> u24:
  return (key == 'd') | (key == 'l') | (key == 'C')

def is_down(key: u24) -> u24:
  return (key == 's') | (key == 'j') | (key == 'B')

def is_turn(key: u24) -> u24:
  return (key == 'w') | (key == 'k') | (key == 'A') | (key == 'x')

# Returns whether the screen changed, and the next game.
def step(g: Game, key: u24) -> (u24, Game):
  open Game: g
  match mode = g.mode:
    case Mode/Playing:
      p = g.piece
      open Piece: p
      if key == TICK:
        return tick(g)
      elif is_left(key):
        return (1, shift(g, p.col - 1))
      elif is_right(key):
        return (1, shift(g, p.col + 1))
      elif is_turn(key):
        return (1, rotate(g))
      elif is_down(key):
        return (1, add_score(fall(with_clock(g, 0)), 1))
      elif key == ' ':
        return (1, hard_drop(g))
      elif key == 'p':
        return (1, with_mode(g, Mode/Paused))
      elif key == 'r':
        return (1, new_game(g.seed))
      else:
        return (0, g)
    case Mode/Paused:
      if key == 'p':
        return (1, with_mode(g, Mode/Playing))
      elif key == 'r':
        return (1, new_game(g.seed))
      else:
        return (0, g)
    case Mode/Over:
      if key == 'r':
        return (1, new_game(g.seed))
      else:
        return (0, g)

# Drawing
# -------

def cat(a: String, b: String) -> String:
  match a:
    case String/Cons:
      return String/Cons(a.head, cat(a.tail, b))
    case String/Nil:
      return b

def join(parts: List(String)) -> String:
  fold parts:
    case List/Cons:
      return cat(parts.head, parts.tail)
    case List/Nil:
      return ""

# 256-color ANSI escapes for: empty, I O T S Z J L, the ghost.
def ink(code: u24) -> String:
  switch code:
    case 0:
      return "\u{1b}[38;5;236m"
    case 1:
      return "\u{1b}[38;5;51m"
    case 2:
      return "\u{1b}[38;5;226m"
    case 3:
      return "\u{1b}[38;5;171m"
    case 4:
      return "\u{1b}[38;5;46m"
    case 5:
      return "\u{1b}[38;5;196m"
    case 6:
      return "\u{1b}[38;5;33m"
    case 7:
      return "\u{1b}[38;5;208m"
    case 8:
      return "\u{1b}[38;5;241m"
    case _:
      return ""

def glyph(code: u24) -> String:
  if code == 0:
    return " ·"
  elif code == GHOST:
    return "░░"
  elif code == BLANK:
    return "  "
  else:
    return "██"

# Two characters per cell, switching color only when it changes from the last cell.
def paint(row: List(u24), last: u24) -> List(String):
  match row:
    case List/Cons:
      if row.head == last:
        return List/Cons(glyph(row.head), paint(row.tail, last))
      else:
        return List/Cons(ink(row.head), List/Cons(glyph(row.head), paint(row.tail, row.head)))
    case List/Nil:
      return []

def preview(kind: u24) -> List(String):
  (n, base) = shape(kind)
  mini = stamp([[BLANK, BLANK, BLANK, BLANK], [BLANK, BLANK, BLANK, BLANK]], base, kind + 1)
  return [join(paint(row, BLANK)) for row in mini]

def label(text: String) -> String:
  return cat("\u{1b}[0;38;5;245m", text)

def value(n: u24) -> String:
  return join(["\u{1b}[1;38;5;255m", u24/to_string(n)])

def status(mode: Mode) -> String:
  match mode:
    case Mode/Playing:
      return ""
    case Mode/Paused:
      return "\u{1b}[1;38;5;226mPAUSED"
    case Mode/Over:
      return "\u{1b}[1;38;5;196mGAME OVER\u{1b}[0;38;5;245m  r: again"

# One line of text beside each board row.
def panel(g: Game) -> List(String):
  open Game: g
  return List/flatten([[label("NEXT")], preview(g.next), [
    "",
    label("SCORE"), value(g.score), "",
    label("LINES"), value(g.lines), "",
    label("LEVEL"), value(level(g.lines)), "",
    status(g.mode), "",
    label("←→  move    ↑  rotate"),
    label("↓   soft drop"),
    label("spc hard drop"),
    label("p   pause   r  restart"),
    label("q   quit")
  ]])

def uncons(side: List(String)) -> (String, List(String)):
  match side:
    case List/Cons:
      return (side.head, side.tail)
    case List/Nil:
      return ("", [])

def draw_rows(rows: List(List(u24)), side: List(String)) -> List(String):
  match rows:
    case List/Cons:
      (text, rest) = uncons(side)
      line = List/flatten([["\u{1b}[38;5;240m  ║"], paint(rows.head, WALL), ["\u{1b}[38;5;240m║   ", text, "\u{1b}[0m\u{1b}[K\n"]])
      return List/Cons(join(line), draw_rows(rows.tail, rest))
    case List/Nil:
      return []

# The whole screen, drawn over the last one from the top-left corner.
def draw(g: Game) -> String:
  open Game: g
  p = g.piece
  open Piece: p
  view = stamp(g.board, cells(landing(solid(g.board), p)), GHOST)
  view = stamp(view, cells(p), p.kind + 1)
  top = "\u{1b}[H\u{1b}[38;5;240m  ╔════════════════════╗   \u{1b}[1;38;5;255mBEND TETRIS\u{1b}[0m\u{1b}[K\n"
  bottom = "\u{1b}[38;5;240m  ╚════════════════════╝\u{1b}[0m\u{1b}[J"
  return join(List/flatten([[top], draw_rows(view, panel(g)), [bottom]]))

# IO
# --

# The first byte read, or 'q' once the input ends.
def first_byte(input: Result(List(u24), u24)) -> u24:
  match input:
    case Result/Ok:
      match bytes = input.val:
        case List/Cons:
          return bytes.head
        case List/Nil:
          return 'q'
    case Result/Err:
      return 'q'

def loop(g: Game) -> IO(u24):
  with IO:
    input <- IO/FS/read(IO/FS/STDIN, 1)
    key = first_byte(input)
    if key == 'q':
      open Game: g
      return wrap(g.score)
    else:
      (changed, g) = step(g, key)
      if changed:
        * <- IO/print(draw(g))
        return loop(g)
      else:
        return loop(g)

def main() -> IO(u24):
  with IO:
    now <- IO/get_time
    (hi, lo) = now
    g = new_game(lo)
    * <- IO/print(cat("\u{1b}[2J\u{1b}[?25l", draw(g)))
    score <- loop(g)
    * <- IO/print(join(["\u{1b}[?25h\n  Final score: ", u24/to_string(score), "\n"]))
    return wrap(score)
