{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Concrete.ConcreteSemantics where

import Data.Set
import Order.Poset
import Order.Lattice
import Order.CompleteLattice
import Lang.Statement

type PowerSet = Set
type Collecting = PowerSet State

instance Poset Collecting where
  leq = isSubsetOf

instance Lattice Collecting where
  join = union
  meet = intersection

instance CompleteLattice Collecting where
  top = error "cannot have infinite set"
  bot = empty
