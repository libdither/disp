# Proofs of LAWS.bend. The laws on the rule and the "small" laws hold by
# computing them: {==} asks the checker to compute both sides. The moves'
# laws split the tree until each side computes.

import Base
import ./calculus.bend as TW
import ./blocks.bend as B
import ./harness.bend as H
import ./design.bend as Design
import ./LAWS.bend as Laws

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

def Laws.fits_fields():
  {==}

def Laws.linear():
  {==}

def Laws.undoes_itself():
  {==}

def Laws.void_stays():
  {==}

def Laws.lays_small():
  {==}

def Laws.always_reads_small():
  {==}

def Laws.moves_only_small():
  {==}

def Laws.sealed_small():
  {==}

def Laws.swaps_small():
  {==}

def Laws.maps_small():
  {==}

def Laws.stems_small():
  {==}

def Laws.units_small():
  {==}
