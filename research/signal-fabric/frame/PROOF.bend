# Proofs of LAWS.bend. The "small" laws and port_is_core hold by running
# them: {==} asks the checker to compute both sides. The others split the
# tree, or the cell, until each side computes.

import Base
import ./twoway.bend as TW
import ./lattice.bend as Lat
import ./loopnet.bend as Design
import ./cases.bend as Cases
import ./LAWS.bend as Laws

def Laws.port_is_core():
  {==}

def Laws.swap_undoes(x):
  match x:
    case TW.L{}:
      {==}
    case TW.S{c}:
      {==}
    case TW.F{l, r}:
      {==}

def Laws.cross_undoes(x):
  match x:
    case TW.L{}:
      {==}
    case TW.S{c}:
      {==}
    case TW.F{TW.L{}, r}:
      {==}
    case TW.F{TW.S{c}, r}:
      {==}
    case TW.F{TW.F{a, b}, c}:
      {==}

def Laws.sink_undoes(x):
  match x:
    case TW.L{}:
      {==}
    case TW.S{TW.L{}}:
      {==}
    case TW.S{TW.S{c}}:
      {==}
    case TW.S{TW.F{a, b}}:
      {==}
    case TW.F{TW.L{}, r}:
      {==}
    case TW.F{TW.S{a}, b}:
      {==}
    case TW.F{TW.F{a, b}, c}:
      {==}

def Laws.two_way_small():
  {==}

def Laws.matter_stays(nb):
  match nb:
    case Lat.Nb{c, n, e, s, w}:
      match c:
        case Design.Void{}:
          {==}
        case Design.Thing{p}:
          {==}

def Laws.sound_small():
  {==}

def Laws.loops_small():
  {==}

def Laws.binary_small():
  {==}
