{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}

module Abstract.Abstraction where

import Data.Map as Map
import Data.Set as Set
import Lang.Statement
import Concrete.ConcreteSemantics
import Abstract.AbstractState
import Order.GaloisConnection

-- Establishes the soundness criterion between concrete and abstract semantics
instance (GaloisConnection (PowerSet Int) a) =>
    GaloisConnection Collecting (AbstractState a) where
  alpha states = let createAlpha pc = let dlt = delta' states pc
                                          fff = (alpha . collectFirst) dlt
                                          sss = (alpha . collectSecond) dlt
                                          ttt = (alpha . collectThird) dlt
                                      in (AbsTriple fff sss ttt)
                     pcs = Set.toList (programCounters states)
                     triples = (Prelude.map createAlpha pcs)
                 in Map.fromList $ zip pcs triples
  gamma absState = undefined

programCounters :: Set State -> Set PC
programCounters = Set.map programCounter

delta' :: PowerSet State -> PC -> PowerSet (Int, Int, Int)
delta' states pc = let filtered = Set.filter ((pc==) . programCounter) states
                   in Set.map (\(_,x,y,z) -> (x,y,z)) filtered

collectFirst :: Ord a => Set (a,b,c) -> Set a
collectFirst = let first (a,_,_) = a in Set.map first
collectSecond :: Ord b => Set (a,b,c) -> Set b
collectSecond = let second (_,b,_) = b in Set.map second
collectThird :: Ord c => Set (a,b,c) -> Set c
collectThird = let third (_,_,c) = c in Set.map third
