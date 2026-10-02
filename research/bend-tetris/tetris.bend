# Tetris: ten columns, twenty rows, seven pieces dealt from a shuffled bag.
#
# The game is pure. `update` folds one frame's key events into the next game,
# and `picture` draws a game as an Image. `main` hands both to App.run for a
# native window; the web page calls the same defs once per animation frame.
#
#   bend tetris.bend -o tetris && ./tetris
import Base

type Cell is Data:
  Cell{row: U32, col: U32}

type Tetromino is Data:
  I{}
  O{}
  T{}
  S{}
  Z{}
  J{}
  L{}

type Piece is Data:
  Piece{kind: Tetromino, turn: U32, row: U32, col: U32}

type Mode is Data:
  Play{}
  Pause{}
  Over{}

# rows: twenty packed rows, top first (see `at`). bag: one bit per kind not yet
# dealt this round. clock: frames since the piece last fell.
type Game is Data:
  Game{rows: List<&2, U32>, piece: Piece, next: Tetromino, bag: U32, seed: U32,
    score: U32, lines: U32, clock: U32, mode: Mode}

# Pieces
# ------

# Each kind's cells as it spawns, inside the n×n box it turns in.
def shape(kind: Tetromino) -> List<&2, Cell>:
  match kind:
    case I{}:
      [Cell{1, 0}, Cell{1, 1}, Cell{1, 2}, Cell{1, 3}]
    case O{}:
      [Cell{0, 0}, Cell{0, 1}, Cell{1, 0}, Cell{1, 1}]
    case T{}:
      [Cell{0, 1}, Cell{1, 0}, Cell{1, 1}, Cell{1, 2}]
    case S{}:
      [Cell{0, 1}, Cell{0, 2}, Cell{1, 0}, Cell{1, 1}]
    case Z{}:
      [Cell{0, 0}, Cell{0, 1}, Cell{1, 1}, Cell{1, 2}]
    case J{}:
      [Cell{0, 0}, Cell{1, 0}, Cell{1, 1}, Cell{1, 2}]
    case L{}:
      [Cell{0, 2}, Cell{1, 0}, Cell{1, 1}, Cell{1, 2}]

def box(kind: Tetromino) -> U32:
  match kind:
    case I{}:
      4
    case O{}:
      2
    case _:
      3

# A kind's code on the board (see `at`), and the kind dealt as the n-th, from 0.
def code(kind: Tetromino) -> U32:
  match kind:
    case I{}:
      1
    case O{}:
      2
    case T{}:
      3
    case S{}:
      4
    case Z{}:
      5
    case J{}:
      6
    case L{}:
      7

def tetromino(n: U32) -> Tetromino:
  match n:
    case 0:
      I{}
    case 1:
      O{}
    case 2:
      T{}
    case 3:
      S{}
    case 4:
      Z{}
    case 5:
      J{}
    case _:
      L{}

# A quarter turn clockwise inside an n×n box.
def turn(+n: U32, cells: List<&2, Cell>) -> List<&2, Cell>:
  match cells:
    case []:
      []
    case c <> rest:
      Cell{r, k} = c
      Cell{k, (n - 1 - r : U32)} <> turn(n, rest)

def turns(times: Nat, +n: U32, cells: List<&2, Cell>) -> List<&2, Cell>:
  match times:
    case 0n:
      cells
    case 1n+t:
      turns(t, n, turn(n, cells))

def offset(+row: U32, +col: U32, cells: List<&2, Cell>) -> List<&2, Cell>:
  match cells:
    case []:
      []
    case c <> rest:
      Cell{r, k} = c
      Cell{(r + row : U32), (k + col : U32)} <> offset(row, col, rest)

def cells(p: Piece) -> List<&2, Cell>:
  Piece{+kind, turn, row, col} = p
  offset(row, col, turns(U32.to_nat((turn .&. 3 : U32)), box(kind), shape(kind)))

# Spawns centred: columns 3 to 5, or 4 and 5 for the O.
def spawn(+kind: Tetromino) -> Piece:
  Piece{kind, 0, 0, (5 - box(kind) / 2 : U32)}

def moved(p: Piece, +rows: U32, +cols: U32, +turns: U32) -> Piece:
  Piece{kind, turn, row, col} = p
  Piece{kind, (turn + turns : U32), (row + rows : U32), (col + cols : U32)}

# The board
# ---------

# A row packs ten cells into 3-bit fields, column c in bits 3c to 3c+2: 0 is
# empty, otherwise the code of the kind of block there.
def at(+row: U32, c: U32) -> U32:
  ((row >> U32.to_nat((c * 3 : U32))) .&. 7 : U32)

# Every field has some bit set; 153391689 has bit 3c set for each column c.
def full(+row: U32) -> Bool:
  U32.is_eq(((row .|. (row >> 1n) .|. (row >> 2n)) .&. 153391689 : U32), 153391689)

def row_at(rows: List<&2, U32>, r: Nat) -> U32:
  match rows:
    case []:
      0
    case h <> t:
      match r:
        case 0n:
          h
        case 1n+q:
          row_at(t, q)

def free.inside(inside: Bool, +rows: List<&2, U32>, +r: U32, +k: U32) -> Bool:
  match inside:
    case True{}:
      U32.is_eq(at(row_at(rows, U32.to_nat(r)), k), 0)
    case False{}:
      False{}

# Inside the walls and empty. Columns left of the board wrap around to huge
# U32 values, so `c < 10` checks both walls.
def free(+rows: List<&2, U32>, c: Cell) -> Bool:
  Cell{+r, +k} = c
  free.inside(((r < 20 : U32) && (k < 10 : U32)), rows, r, k)

def fits(+rows: List<&2, U32>, cs: List<&2, Cell>) -> Bool:
  match cs:
    case []:
      True{}
    case c <> rest:
      free(rows, c) && fits(rows, rest)

def put(rows: List<&2, U32>, r: Nat, +k: U32, +code: U32) -> List<&2, U32>:
  match rows:
    case []:
      []
    case h <> t:
      match r:
        case 0n:
          (h .|. (code << U32.to_nat((k * 3 : U32))) : U32) <> t
        case 1n+q:
          h <> put(t, q, k, code)

def stamp(cs: List<&2, Cell>, rows: List<&2, U32>, +code: U32) -> List<&2, U32>:
  match cs:
    case []:
      rows
    case c <> rest:
      Cell{r, k} = c
      stamp(rest, put(rows, U32.to_nat(r), k, code), code)

def gap(row: U32) -> Bool:
  Bool.not(full(row))

# Drops full rows and refills from the top; answers how many were cleared.
def clear(rows: List<&2, U32>) -> U32 & List<&2, U32>:
  +kept = List.filter(~U32, ~gap, rows)
  +cleared = Nat.sub(20n, List.length(&2, U32, kept))
  (U32.from_nat(cleared), List.append(&2, U32, List.replicate(U32, cleared, 0), kept))

# Falls while the next row down is free, at most `fuel` rows.
def landing(fuel: Nat, below_free: Bool, +rows: List<&2, U32>, +p: Piece) -> Piece:
  match fuel:
    case 0n:
      p
    case 1n+f:
      match below_free:
        case False{}:
          p
        case True{}:
          +q = moved(p, 1, 0, 0)
          landing(f, fits(rows, cells(moved(q, 1, 0, 0))), rows, q)

def drop(+rows: List<&2, U32>, +p: Piece) -> Piece:
  landing(22n, fits(rows, cells(moved(p, 1, 0, 0))), rows, p)

# The first candidate that fits, or `p` if none does.
def first_fit(cands: List<&2, Piece>, +rows: List<&2, U32>, p: Piece) -> Piece:
  match cands:
    case []:
      p
    case +c <> rest:
      Bool.pick(Piece, fits(rows, cells(c)), c, first_fit(rest, rows, p))

# Dealing
# -------

type Deal is Data:
  Deal{kind: Tetromino, bag: U32, seed: U32}

def count(fuel: Nat, +bag: U32) -> U32:
  match fuel:
    case 0n:
      0
    case 1n+f:
      ((bag .&. 1) + count(f, (bag >> 1n : U32)) : U32)

# The n-th kind (from 0) still in the bag, looking from kind k up.
def nth(fuel: Nat, +bag: U32, +k: U32, +n: U32) -> U32:
  match fuel:
    case 0n:
      k
    case 1n+f:
      +here = U32.is_eq(((bag >> U32.to_nat(k)) .&. 1 : U32), 1)
      Bool.pick(U32, here && U32.is_eq(n, 0), k,
        nth(f, bag, (k + 1 : U32), Bool.pick(U32, here, (n - 1 : U32), n)))

def deal.from(+bag: U32, +seed: U32) -> Deal:
  +n = nth(7n, bag, 0, ((seed >> 16n) % count(7n, bag) : U32))
  Deal{tetromino(n), (bag .^. (1 << U32.to_nat(n)) : U32), seed}

# Takes a random kind out of the bag, refilling it with all seven when empty.
def deal(+bag: U32, seed: U32) -> Deal:
  deal.from(Bool.pick(U32, U32.is_eq(bag, 0), 127, bag), (seed * 1664525 + 1013904223 : U32))

# Rules
# -----

def level(lines: U32) -> U32:
  (lines / 10 + 1 : U32)

# Frames per row of gravity at each level, at 60 frames a second.
def frames(level: U32) -> U32:
  match level:
    case 1:
      48
    case 2:
      42
    case 3:
      36
    case 4:
      30
    case 5:
      24
    case 6:
      18
    case 7:
      13
    case 8:
      9
    case 9:
      6
    case 10:
      4
    case _:
      2

def points(cleared: U32) -> U32:
  match cleared:
    case 0:
      0
    case 1:
      100
    case 2:
      300
    case 3:
      500
    case _:
      800

def start.with(+first: Tetromino, d: Deal) -> Game:
  Deal{next, bag, seed} = d
  Game{List.replicate(U32, 20n, 0), spawn(first), next, bag, seed, 0, 0, 0, Play{}}

def start.from(d: Deal) -> Game:
  Deal{first, bag, seed} = d
  start.with(first, deal(bag, seed))

def start(seed: U32) -> Game:
  start.from(deal(0, seed))

def lock.with(cleared: U32 & List<&2, U32>, +next: Tetromino, d: Deal, score: U32, +lines: U32) -> Game:
  (+n, +rows) = cleared
  Deal{next2, bag2, seed2} = d
  +fresh = spawn(next)
  Game{rows, fresh, next2, bag2, seed2, (score + points(n) * level(lines) : U32),
    (lines + n : U32), 0, Bool.pick(Mode, fits(rows, cells(fresh)), Play{}, Over{})}

# Merges the piece into the board, clears and scores lines, brings in the next
# piece, and ends the game if it has no room.
def lock(g: Game) -> Game:
  Game{rows, +piece, next, bag, seed, score, lines, clock, mode} = g
  Piece{kind, turn, row, col} = piece
  lock.with(clear(stamp(cells(piece), rows, code(kind))), next, deal(bag, seed), score, lines)

def score(g: Game) -> U32:
  Game{rows, piece, next, bag, seed, score, lines, clock, mode} = g
  score

def with_mode(g: Game, m: Mode) -> Game:
  Game{rows, piece, next, bag, seed, score, lines, clock, mode} = g
  Game{rows, piece, next, bag, seed, score, lines, clock, m}

# Moves the piece to the first candidate that fits.
def shift(g: Game, cands: Piece -> List<&2, Piece>) -> Game:
  Game{+rows, +piece, next, bag, seed, score, lines, clock, mode} = g
  Game{rows, first_fit(cands(piece), rows, piece), next, bag, seed, score, lines, clock, mode}

# Rotation tries the spot in place, then nudges sideways off walls and stacks.
def kicks(p: Piece) -> List<&2, Piece>:
  +q = p
  [moved(q, 0, 0, 1), moved(q, 0, 4294967295, 1), moved(q, 0, 1, 1),
    moved(q, 0, 4294967294, 1), moved(q, 0, 2, 1)]

def fall.at(free_below: Bool, g: Game) -> Game:
  match free_below:
    case True{}:
      Game{rows, piece, next, bag, seed, score, lines, clock, mode} = g
      Game{rows, moved(piece, 1, 0, 0), next, bag, seed, score, lines, 0, mode}
    case False{}:
      lock(g)

def fall(+g: Game) -> Game:
  Game{rows, piece, next, bag, seed, score, lines, clock, mode} = g
  fall.at(fits(rows, cells(moved(piece, 1, 0, 0))), g)

# Two points for each row dropped.
def hard_drop.to(landed: Piece, g: Game) -> Game:
  Piece{kind, turn, +row, col} = landed
  Game{rows, piece, next, bag, seed, score, lines, clock, mode} = g
  Piece{k0, t0, row0, c0} = piece
  lock(Game{rows, Piece{kind, turn, row, col}, next, bag, seed, (score + 2 * (row - row0) : U32),
    lines, clock, mode})

def hard_drop(+g: Game) -> Game:
  Game{rows, piece, next, bag, seed, score, lines, clock, mode} = g
  hard_drop.to(drop(rows, piece), g)

def soft_drop(g: Game) -> Game:
  Game{rows, piece, next, bag, seed, score, lines, clock, mode} = g
  fall(Game{rows, piece, next, bag, seed, (score + 1 : U32), lines, clock, mode})

def tick.at(due: Bool, g: Game) -> Game:
  match due:
    case True{}:
      fall(g)
    case False{}:
      Game{rows, piece, next, bag, seed, score, lines, clock, mode} = g
      Game{rows, piece, next, bag, seed, score, lines, (clock + 1 : U32), mode}

def tick(g: Game) -> Game:
  Game{rows, piece, next, bag, seed, score, +lines, +clock, mode} = g
  match mode:
    case Play{}:
      tick.at((clock + 1 >= frames(level(lines)) : U32),
        Game{rows, piece, next, bag, seed, score, lines, clock, Play{}})
    case Pause{}:
      Game{rows, piece, next, bag, seed, score, lines, clock, Pause{}}
    case Over{}:
      Game{rows, piece, next, bag, seed, score, lines, clock, Over{}}

# Keys: arrows (63232 up, 63233 down, 63234 left, 63235 right) or WASD, space
# drops, P pauses, R starts over, Esc quits.
def play(+code: U32, g: Game) -> Game:
  match code:
    case 32:
      hard_drop(g)
    case 97:
      shift(g, p => [moved(p, 0, 4294967295, 0)])
    case 100:
      shift(g, p => [moved(p, 0, 1, 0)])
    case 112:
      with_mode(g, Pause{})
    case 115:
      soft_drop(g)
    case 119:
      shift(g, kicks)
    case 63232:
      shift(g, kicks)
    case 63233:
      soft_drop(g)
    case 63234:
      shift(g, p => [moved(p, 0, 4294967295, 0)])
    case 63235:
      shift(g, p => [moved(p, 0, 1, 0)])
    case _:
      g

# R starts over in any mode (from the current seed, so with a fresh deal).
def press(+code: U32, g: Game) -> Game:
  Game{rows, piece, next, bag, +seed, score, lines, clock, mode} = g
  match mode:
    case Play{}:
      Bool.pick(Game, U32.is_eq(code, 114), start(seed),
        play(code, Game{rows, piece, next, bag, seed, score, lines, clock, Play{}}))
    case Pause{}:
      Bool.pick(Game, U32.is_eq(code, 114), start(seed), Game{rows, piece, next, bag, seed, score,
        lines, clock, Bool.pick(Mode, U32.is_eq(code, 112), Play{}, Pause{})})
    case Over{}:
      Bool.pick(Game, U32.is_eq(code, 114), start(seed),
        Game{rows, piece, next, bag, seed, score, lines, clock, Over{}})

def event(e: Event, g: Game) -> Maybe<Game>:
  match e:
    case Key{27, True{}}:
      None{}
    case Key{code, True{}}:
      Some{press(code, g)}
    case Close{}:
      None{}
    case _:
      Some{g}

# One frame: its key presses in order, then gravity. None quits.
def update(events: List<Event>, g: Game) -> Maybe<Game>:
  match events:
    case []:
      Some{tick(g)}
    case e <> rest:
      Maybe.bind(&1, Game, Game, event(e, g), g2 => update(rest, g2))

# Drawing
# -------
# The window is 512 pixels square: a quadtree of 32 × 32 squares of 16 pixels.
# The board fills squares 5..14 × 6..25 inside a one-square frame, and the next
# piece squares 18..21 × 8..9. Blocks are split into 4 × 4 pixels of 4 for a
# bevel, and so are squares under text, set in a 3 × 5 font on that 4-pixel grid.

def paper() -> U32:
  921878 # 0e1116

def well() -> U32:
  658448 # 0a0c10

def well.alt() -> U32:
  856085 # 0d1015

def frame() -> U32:
  2765115 # 2a313b

def label() -> U32:
  8226710 # 7d8796

def value() -> U32:
  15922680 # f2f5f8

def amber() -> U32:
  16436245 # facc15

def rose() -> U32:
  16281969 # f87171

def color(kind: Tetromino) -> U32:
  match kind:
    case I{}:
      2282478 # 22d3ee
    case O{}:
      16436245 # facc15
    case T{}:
      12616956 # c084fc
    case S{}:
      4906624 # 4ade80
    case Z{}:
      16281969 # f87171
    case J{}:
      6333946 # 60a5fa
    case L{}:
      16486972 # fb923c

# Halfway to white, and halfway to black.
def light(+c: U32) -> U32:
  (((c >> 1n) .&. 8355711) + 8421504 : U32)

def dark(+c: U32) -> U32:
  ((c >> 1n) .&. 8355711 : U32)

# 4 × 4 pixels from a frame color `e` and a fill `f`: e on the rim, f inside.
def rim(+e: U32, +f: U32) -> Image:
  Qua{Qua{Pix{e}, Pix{e}, Pix{e}, Pix{f}}, Qua{Pix{e}, Pix{e}, Pix{f}, Pix{e}},
    Qua{Pix{e}, Pix{f}, Pix{e}, Pix{e}}, Qua{Pix{f}, Pix{e}, Pix{e}, Pix{e}}}

# A block: lit along the top and left, shaded along the bottom and right.
def block(+c: U32) -> Image:
  +l = light(c)
  +d = dark(c)
  Qua{Qua{Pix{l}, Pix{l}, Pix{l}, Pix{c}}, Qua{Pix{l}, Pix{l}, Pix{c}, Pix{d}},
    Qua{Pix{l}, Pix{c}, Pix{l}, Pix{d}}, Qua{Pix{c}, Pix{d}, Pix{d}, Pix{d}}}

def has(cs: List<&2, Cell>, +r: U32, +k: U32) -> Bool:
  match cs:
    case []:
      False{}
    case c <> rest:
      Cell{cr, ck} = c
      (U32.is_eq(cr, r) && U32.is_eq(ck, k)) || has(rest, r, k)

# What every square needs: the board with the piece and its ghost, the next
# piece, and the lines of text.
type Line is Data:
  Line{x: U32, y: U32, color: U32, text: String}

type Scene is Data:
  Scene{rows: List<&2, U32>, piece: List<&2, Cell>, ghost: List<&2, Cell>, kind: Tetromino,
    next: List<&2, Cell>, next_kind: Tetromino, text: List<&2, Line>}

def cell.at(on_piece: Bool, on_ghost: Bool, +kind: Tetromino, +code: U32, +shade: U32) -> Image:
  match on_piece:
    case True{}:
      block(color(kind))
    case False{}:
      match on_ghost:
        case True{}:
          rim(dark(color(kind)), shade)
        case False{}:
          Bool.pick(Image, U32.is_eq(code, 0), Pix{shade}, block(color(tetromino((code - 1 : U32)))))

def cell(+s: Scene, +r: U32, +k: U32) -> Image:
  Scene{rows, piece, ghost, kind, next, next_kind, text} = s
  cell.at(has(piece, r, k), has(ghost, r, k), kind, at(row_at(rows, U32.to_nat(r)), k),
    Bool.pick(U32, U32.is_eq(((r + k) .&. 1 : U32), 0), well(), well.alt()))

# Text
# ----

# 3 × 5 glyphs as 15 bits, top row first: the digits and the letters in use.
def glyph(c: U32) -> U32:
  match c:
    case 48:
      31599 # 0  ### #.# #.# #.# ###
    case 49:
      11415 # 1  .#. ##. .#. .#. ###
    case 50:
      25255 # 2  ##. ..# .#. #.. ###
    case 51:
      25230 # 3  ##. ..# .#. ..# ##.
    case 52:
      23497 # 4  #.# #.# ### ..# ..#
    case 53:
      31118 # 5  ### #.. ##. ..# ##.
    case 54:
      14831 # 6  .## #.. ### #.# ###
    case 55:
      29330 # 7  ### ..# .#. .#. .#.
    case 56:
      31727 # 8  ### #.# ### #.# ###
    case 57:
      31694 # 9  ### #.# ### ..# ##.
    case 65:
      11245 # A  .#. #.# ### #.# #.#
    case 66:
      27566 # B  ##. #.# ##. #.# ##.
    case 67:
      14627 # C  .## #.. #.. #.. .##
    case 68:
      27502 # D  ##. #.# #.# #.# ##.
    case 69:
      31143 # E  ### #.. ##. #.. ###
    case 71:
      14699 # G  .## #.. #.# #.# .##
    case 73:
      29847 # I  ### .#. .#. .#. ###
    case 76:
      18727 # L  #.. #.. #.. #.. ###
    case 77:
      24557 # M  #.# ### ### #.# #.#
    case 78:
      27501 # N  ##. #.# #.# #.# #.#
    case 79:
      11114 # O  .#. #.# #.# #.# .#.
    case 80:
      27556 # P  ##. #.# ##. #.. #..
    case 82:
      27565 # R  ##. #.# ##. #.# #.#
    case 83:
      14478 # S  .## #.. .#. ..# ##.
    case 84:
      29842 # T  ### .#. .#. .#. .#.
    case 85:
      23407 # U  #.# #.# #.# #.# ###
    case 86:
      23402 # V  #.# #.# #.# #.# .#.
    case 88:
      23213 # X  #.# #.# .#. #.# #.#
    case _:
      0

def char_at(s: String, i: Nat) -> U32:
  match s:
    case SNil{}:
      32
    case SCon{c, rest}:
      match i:
        case 0n:
          Char.to_u32(c)
        case 1n+j:
          char_at(rest, j)

def lit(+mask: U32, +row: U32, +col: U32) -> Bool:
  U32.is_eq(((mask >> U32.to_nat((14 - row * 3 - col : U32))) .&. 1 : U32), 1)

# A text pixel, once known to lie in its line's band (under 32 characters).
def ink.band(band: Bool, +dx: U32, +dy: U32, +color: U32, text: String, +rest: U32) -> U32:
  match band:
    case True{}:
      Bool.pick(U32, ((dx % 4 : U32) < 3 : U32)
        && lit(glyph(char_at(text, U32.to_nat((dx / 4 : U32)))), dy, (dx % 4 : U32)), color, rest)
    case False{}:
      rest

# The color of the text pixel at fx, fy (in 4-pixel units), or the paper.
def ink(lines: List<&2, Line>, +fx: U32, +fy: U32) -> U32:
  match lines:
    case []:
      paper()
    case l <> rest:
      Line{x, y, color, text} = l
      +dx = (fx - x : U32)
      +dy = (fy - y : U32)
      ink.band(((dx < 128 : U32) && (dy < 5 : U32)), dx, dy, color, text, ink(rest, fx, fy))

# Whether some line of text reaches into the square at x, y.
def touches(lines: List<&2, Line>, +x: U32, +y: U32) -> Bool:
  match lines:
    case []:
      False{}
    case l <> rest:
      Line{+lx, +ly, color, text} = l
      +n = U32.from_nat(String.length(text))
      ((4 * x + 3 >= lx) && (4 * x < lx + 4 * n) && (4 * y + 3 >= ly) && (4 * y < ly + 5) : U32)
        || touches(rest, x, y)

def inks(+lines: List<&2, Line>, +fx: U32, +fy: U32) -> Image:
  Qua{Pix{ink(lines, fx, fy)}, Pix{ink(lines, (fx + 1 : U32), fy)},
    Pix{ink(lines, fx, (fy + 1 : U32))}, Pix{ink(lines, (fx + 1 : U32), (fy + 1 : U32))}}

def text.at(near: Bool, +lines: List<&2, Line>, +x: U32, +y: U32) -> Image:
  match near:
    case True{}:
      +fx = (4 * x : U32)
      +fy = (4 * y : U32)
      Qua{inks(lines, fx, fy), inks(lines, (fx + 2 : U32), fy), inks(lines, fx, (fy + 2 : U32)),
        inks(lines, (fx + 2 : U32), (fy + 2 : U32))}
    case False{}:
      Pix{paper()}

def title(m: Mode) -> String:
  match m:
    case Play{}:
      "BEND TETRIS"
    case Pause{}:
      "PAUSED"
    case Over{}:
      "GAME OVER"

def title.color(m: Mode) -> U32:
  match m:
    case Play{}:
      value()
    case Pause{}:
      amber()
    case Over{}:
      rose()

def hint(m: Mode) -> String:
  match m:
    case Over{}:
      "PRESS R"
    case _:
      "NEXT"

# The picture
# -----------

def square.at(on_board: Bool, on_frame: Bool, on_next: Bool, +s: Scene, +x: U32, +y: U32) -> Image:
  match on_board:
    case True{}:
      cell(s, (y - 6 : U32), (x - 5 : U32))
    case False{}:
      match on_frame:
        case True{}:
          Pix{frame()}
        case False{}:
          match on_next:
            case True{}:
              Scene{rows, piece, ghost, kind, next, next_kind, text} = s
              Bool.pick(Image, has(next, (y - 8 : U32), (x - 18 : U32)), block(color(next_kind)), Pix{paper()})
            case False{}:
              Scene{rows, piece, ghost, kind, next, next_kind, text} = s
              text.at(touches(text, x, y), text, x, y)

def square(+s: Scene, +x: U32, +y: U32) -> Image:
  square.at(((x - 5 < 10 : U32) && (y - 6 < 20 : U32)), ((x - 4 < 12 : U32) && (y - 5 < 22 : U32)),
    ((x - 18 < 4 : U32) && (y - 8 < 2 : U32)), s, x, y)

def draw(d: Nat, +x: U32, +y: U32, +s: Scene) -> Image:
  match d:
    case 0n:
      square(s, x, y)
    case 1n++e:
      +h = U32.shln(1, e)
      tl tr bl br = draw(e, x, y, s) draw(e, (x + h : U32), y, s)
        draw(e, x, (y + h : U32), s) draw(e, (x + h : U32), (y + h : U32), s)
      Qua{tl, tr, bl, br}

def scene(g: Game) -> Scene:
  Game{+rows, +piece, +next, bag, seed, +score, +lines, clock, +mode} = g
  Piece{+kind, turn, row, col} = piece
  Scene{rows, cells(piece), cells(drop(rows, piece)), kind, shape(next), next,
    [Line{16, 8, title.color(mode), title(mode)}, Line{72, 24, label(), hint(mode)},
      Line{72, 48, label(), "SCORE"}, Line{72, 56, value(), U32.show(score)},
      Line{72, 72, label(), "LINES"}, Line{72, 80, value(), U32.show(lines)},
      Line{72, 96, label(), "LEVEL"}, Line{72, 104, value(), U32.show(level(lines))},
      Line{16, 114, label(), "P PAUSE   R RESTART"}]}

def picture(g: Game) -> Image:
  draw(5n, 0, 0, scene(g))

def view(g: Game) -> Game & Image:
  +h = g
  (h, picture(h))

def step(events: List<Event>, g: Game) -> IO(Maybe<Game>):
  IO.pure(Maybe<Game>, update(events, g))

def main() -> IO(Unit):
  do IO<Unit>:
    seed : U32 <- IO.try(U32, IO.random_u32())
    App.run(~Game, ~App{view, step}, "Bend Tetris", 512, 512, start(seed))
