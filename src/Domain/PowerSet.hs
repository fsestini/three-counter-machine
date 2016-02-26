{-# LANGUAGE TypeSynonymInstances #-}

module Domain.PowerSet where

import Data.Set as Set
import Order.Poset
import Order.Lattice
import Order.CompleteLattice

type PowerSet = Set

instance Ord a => Poset (PowerSet a) where
  leq = isSubsetOf

instance Ord a => Lattice (PowerSet a) where
  join = union
  meet = intersection

instance Ord a => CompleteLattice (PowerSet a) where
  top = error "cannot have infinite set"
  bot = empty
