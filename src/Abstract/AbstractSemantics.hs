module Abstract.AbstractSemantics where

import Order.Lattice
import Order.CompleteLattice
import Data.Set as Set hiding(map,filter,foldr)
import Data.Map as Map hiding(foldr,map,filter)
import Abstract.AbstractState
import Abstract.IntegerAbstraction
import Lang.Statement

abstractF :: (CompleteLattice a, IntegerAbstraction a) =>
    Program -> AbstractState a -> AbstractState a -> AbstractState a
abstractF prog initial absState = initial `join` (abstractTau prog absState)

abstractSemantics :: (Eq a, CompleteLattice a, IntegerAbstraction a) =>
    Program -> AbstractState a -> AbstractState a
abstractSemantics prog initial =
  let absF = abstractF prog initial
      fixpoint old | old == absF old = old
                   | otherwise = fixpoint (absF old)
  in fixpoint (botMap (programPoints prog))

withProgPoints :: Program -> [(PC, Statement)]
withProgPoints prog = let pcs = [1..(length prog)]
                          in zip pcs prog

incXProgPoints :: Program -> [PC]
incXProgPoints prog = map fst $ filter (incX . snd) (withProgPoints prog)
incYProgPoints :: Program -> [PC]
incYProgPoints prog = map fst $ filter (incY . snd) (withProgPoints prog)
incZProgPoints :: Program -> [PC]
incZProgPoints prog = map fst $ filter (incZ . snd) (withProgPoints prog)
decXProgPoints :: Program -> [PC]
decXProgPoints prog = map fst $ filter (decX . snd) (withProgPoints prog)
decYProgPoints :: Program -> [PC]
decYProgPoints prog = map fst $ filter (decY . snd) (withProgPoints prog)
decZProgPoints :: Program -> [PC]
decZProgPoints prog = map fst $ filter (decZ . snd) (withProgPoints prog)
zeroXProgPoints :: Program -> [PC]
zeroXProgPoints prog = map fst $ filter (zeroX . snd) (withProgPoints prog)
zeroYProgPoints :: Program -> [PC]
zeroYProgPoints prog = map fst $ filter (zeroY . snd) (withProgPoints prog)
zeroZProgPoints :: Program -> [PC]
zeroZProgPoints prog = map fst $ filter (zeroZ . snd) (withProgPoints prog)
absTauIncX :: (CompleteLattice a, IntegerAbstraction a) =>
    Set PC -> AbstractState a -> PC -> AbstractState a
absTauIncX pcs absState pc =
  case Map.lookup pc absState of
       Nothing -> error "pc not in pcs"
       Just triple -> singletonAbsState pcs (pc + 1) (xPlusPlus triple)
absTauIncY :: (CompleteLattice a, IntegerAbstraction a) =>
    Set PC -> AbstractState a -> PC -> AbstractState a
absTauIncY pcs absState pc =
  case Map.lookup pc absState of
       Nothing -> error "pc not in pcs"
       Just triple -> singletonAbsState pcs (pc + 1) (yPlusPlus triple)
absTauIncZ :: (CompleteLattice a, IntegerAbstraction a) =>
    Set PC -> AbstractState a -> PC -> AbstractState a
absTauIncZ pcs absState pc =
  case Map.lookup pc absState of
       Nothing -> error "pc not in pcs"
       Just triple -> singletonAbsState pcs (pc + 1) (zPlusPlus triple)

absTauDecX :: (CompleteLattice a, IntegerAbstraction a) =>
    Set PC -> AbstractState a -> PC -> AbstractState a
absTauDecX pcs absState pc =
  case Map.lookup pc absState of
       Nothing -> error "pc not in pcs"
       Just triple -> singletonAbsState pcs (pc + 1) (xMinusMinus triple)
absTauDecY :: (CompleteLattice a, IntegerAbstraction a) =>
    Set PC -> AbstractState a -> PC -> AbstractState a
absTauDecY pcs absState pc =
  case Map.lookup pc absState of
       Nothing -> error "pc not in pcs"
       Just triple -> singletonAbsState pcs (pc + 1) (yMinusMinus triple)
absTauDecZ :: (CompleteLattice a, IntegerAbstraction a) =>
    Set PC -> AbstractState a -> PC -> AbstractState a
absTauDecZ pcs absState pc =
  case Map.lookup pc absState of
       Nothing -> error "pc not in pcs"
       Just triple -> singletonAbsState pcs (pc + 1) (zMinusMinus triple)

absTauZeroX :: (CompleteLattice a, IntegerAbstraction a) =>
    Set PC -> PC -> PC -> AbstractState a -> PC -> AbstractState a
absTauZeroX pcs pc' pc'' absState pc =
  case Map.lookup pc absState of
       Nothing -> error "pc not in pcs"
       Just triple -> join (singletonAbsState pcs pc' (xEqualsZero triple))
                           (singletonAbsState pcs pc'' (xNequalsZero triple))

absTauZeroY :: (CompleteLattice a, IntegerAbstraction a) =>
    Set PC -> PC -> PC -> AbstractState a -> PC -> AbstractState a
absTauZeroY pcs pc' pc'' absState pc =
  case Map.lookup pc absState of
       Nothing -> error "pc not in pcs"
       Just triple -> join (singletonAbsState pcs pc' (yEqualsZero triple))
                           (singletonAbsState pcs pc'' (yNequalsZero triple))

absTauZeroZ :: (CompleteLattice a, IntegerAbstraction a) =>
    Set PC -> PC -> PC -> AbstractState a -> PC -> AbstractState a
absTauZeroZ pcs pc' pc'' absState pc =
  case Map.lookup pc absState of
       Nothing -> error "pc not in pcs"
       Just triple -> join (singletonAbsState pcs pc' (zEqualsZero triple))
                           (singletonAbsState pcs pc'' (zNequalsZero triple))

abstractTau :: (CompleteLattice a, IntegerAbstraction a) =>
    Program -> AbstractState a -> AbstractState a
abstractTau prog absState =
  let pcs = programPoints prog
      withPP = withProgPoints prog
      incXPcs = incXProgPoints prog
      incYPcs = incYProgPoints prog
      incZPcs = incZProgPoints prog
      decXPcs = decXProgPoints prog
      decYPcs = decYProgPoints prog
      decZPcs = decZProgPoints prog
      joinsIncX = foldr (join . absTauIncX pcs absState) (botMap pcs) incXPcs
      joinsIncY = foldr (join . absTauIncY pcs absState) (botMap pcs) incYPcs
      joinsIncZ = foldr (join . absTauIncZ pcs absState) (botMap pcs) incZPcs
      joinsDecX = foldr (join . absTauDecX pcs absState) (botMap pcs) decXPcs
      joinsDecY = foldr (join . absTauDecY pcs absState) (botMap pcs) decYPcs
      joinsDecZ = foldr (join . absTauDecZ pcs absState) (botMap pcs) decZPcs
      jzx1 = filter (\(_,stmt) -> zeroX stmt) (withProgPoints prog)
      jzx2 = map (\(pc,Zero X pc' pc'') -> absTauZeroX pcs pc' pc'' absState pc) jzx1
      joinsZeroX = foldr join (botMap pcs) jzx2
      jzy1 = filter (\(_,stmt) -> zeroY stmt) (withProgPoints prog)
      jzy2 = map (\(pc,Zero Y pc' pc'') -> absTauZeroY pcs pc' pc'' absState pc) jzy1
      joinsZeroY = foldr join (botMap pcs) jzy2
      jzz1 = filter (\(_,stmt) -> zeroZ stmt) (withProgPoints prog)
      jzz2 = map (\(pc,Zero Z pc' pc'') -> absTauZeroZ pcs pc' pc'' absState pc) jzz1
      joinsZeroZ = foldr join (botMap pcs) jzz2
  in foldr join (botMap pcs) [joinsIncX, joinsIncY, joinsIncZ
                              , joinsDecX, joinsDecY, joinsDecZ
                              , joinsZeroX, joinsZeroY, joinsZeroZ]
