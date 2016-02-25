{-# LANGUAGE GADTSyntax, TypeSynonymInstances, FlexibleInstances #-}


module Abstract.AbstractState where

import Data.Map as Map
import Data.Set as Set
import Concrete.ConcreteSemantics
import Order.Poset
import Order.Lattice
import Order.CompleteLattice
import Lang.Statement

mapKeysSet :: Ord k => Map k v -> Set k
mapKeysSet map = Set.fromList $ Prelude.map fst (Map.toList map)

ascendingValues :: Map k v -> [v]
ascendingValues map = Prelude.map snd (Map.toAscList map)

type AbstractState a = Map PC (AbstractTriple a)

data AbstractTriple a where {
  AbsTriple :: a -> a -> a -> AbstractTriple a
} deriving (Eq,Show)
  --AbsTriple :: CompleteLattice a => a -> a -> a -> AbstractTriple a

instance Poset a => Poset (AbstractTriple a) where
  (AbsTriple a1 a2 a3) `leq` (AbsTriple a1' a2' a3') =
      a1 `leq` a1' && a2 `leq` a2' && a3 `leq` a3'

instance (Lattice a) => Lattice (AbstractTriple a) where
  join (AbsTriple a1 a2 a3) (AbsTriple a1' a2' a3') =
    AbsTriple (a1 `join` a1') (a2 `join` a2') (a3 `join` a3')
  meet (AbsTriple a1 a2 a3) (AbsTriple a1' a2' a3') =
    AbsTriple (a1 `meet` a1') (a2 `meet` a2') (a3 `meet` a3')

instance CompleteLattice a => CompleteLattice (AbstractTriple a) where
  top = AbsTriple top top top
  bot = AbsTriple bot bot bot

instance Poset a => Poset (AbstractState a) where
  state1 `leq` state2 =
      mapKeysSet state1 == mapKeysSet state2
      && (and (zipWith (leq) (ascendingValues state1) (ascendingValues state2)))

instance Lattice a => Lattice (AbstractState a) where
  join = unionWith join
  meet = unionWith meet

topMap :: CompleteLattice a => Set PC -> AbstractState a
topMap pcs = Map.fromList $ pure (,) <*> (Set.toList pcs) <*> [top]

botMap :: CompleteLattice a => Set PC -> AbstractState a
botMap pcs = Map.fromList $ pure (,) <*> (Set.toList pcs) <*> [bot]

botAbsState :: CompleteLattice a => Set PC -> AbstractState a
botAbsState pcs = let list = Set.toList pcs
                  in Map.fromList $ pure (,) <*> list <*> [bot]

topAbsState :: CompleteLattice a => Set PC -> AbstractState a
topAbsState pcs = let list = Set.toList pcs
                  in Map.fromList $ pure (,) <*> list <*> [top]

singletonAbsState :: CompleteLattice a =>
    Set PC -> PC -> AbstractTriple a -> AbstractState a
singletonAbsState pcs pc triple = if pc `elem` pcs
                                     then Map.insert pc triple (botAbsState pcs)
                                     else error "pc out of pcs"
