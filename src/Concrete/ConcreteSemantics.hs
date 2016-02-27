{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Concrete.ConcreteSemantics where

import Data.Set
import Order.Poset
import Order.Lattice
import Order.CompleteLattice
import Lang.Statement
import Domain.PowerSet

type Collecting = PowerSet State
