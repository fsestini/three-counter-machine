module Order.CompleteLattice where

import Order.Lattice

class Lattice l => CompleteLattice l where
  top :: l
  bot :: l
