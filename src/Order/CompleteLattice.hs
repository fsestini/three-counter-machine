module Order.CompleteLattice where

import Order.Lattice

class CompleteLattice l where
  top :: l
  bot :: l
