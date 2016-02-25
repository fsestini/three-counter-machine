module Order.Lattice where

import Order.Poset

class (Poset l) => Lattice l where
  join :: l -> l -> l
  meet :: l -> l -> l
