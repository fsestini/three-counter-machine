module Abstract.IntegerAbstraction where

import Abstract.AbstractState

class IntegerAbstraction a where
  plusOne :: a -> a
  minusOne :: a -> a
  equalsZero :: a -> Bool
  nequalsZero :: a -> Bool

xPlusPlus :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
xPlusPlus (AbsTriple a1 a2 a3) = AbsTriple (plusOne a1) a2 a3
yPlusPlus :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
yPlusPlus (AbsTriple a1 a2 a3) = AbsTriple a1 (plusOne a2) a3
zPlusPlus :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
zPlusPlus (AbsTriple a1 a2 a3) = AbsTriple a1 a2 (plusOne a3)

xMinusMinus :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
xMinusMinus (AbsTriple a1 a2 a3) = AbsTriple (minusOne a1) a2 a3
yMinusMinus :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
yMinusMinus (AbsTriple a1 a2 a3) = AbsTriple a1 (minusOne a2) a3
zMinusMinus :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
zMinusMinus (AbsTriple a1 a2 a3) = AbsTriple a1 a2 (minusOne a3)

xEqualsZero :: IntegerAbstraction a => AbstractTriple a -> Bool
xEqualsZero (AbsTriple a1 a2 a3) = equalsZero a1
yEqualsZero :: IntegerAbstraction a => AbstractTriple a -> Bool
yEqualsZero (AbsTriple a1 a2 a3) = equalsZero a2
zEqualsZero :: IntegerAbstraction a => AbstractTriple a -> Bool
zEqualsZero (AbsTriple a1 a2 a3) = equalsZero a3

xNequalsZero :: IntegerAbstraction a => AbstractTriple a -> Bool
xNequalsZero (AbsTriple a1 a2 a3) = nequalsZero a1
yNequalsZero :: IntegerAbstraction a => AbstractTriple a -> Bool
yNequalsZero (AbsTriple a1 a2 a3) = nequalsZero a2
zNequalsZero :: IntegerAbstraction a => AbstractTriple a -> Bool
zNequalsZero (AbsTriple a1 a2 a3) = nequalsZero a3
