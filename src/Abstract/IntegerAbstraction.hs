module Abstract.IntegerAbstraction where

import Abstract.AbstractState

class IntegerAbstraction a where
  plusOne :: a -> a
  minusOne :: a -> a
  equalsZero :: a -> a
  nequalsZero :: a -> a

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

xEqualsZero :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
xEqualsZero (AbsTriple a1 a2 a3) = AbsTriple (equalsZero a1) a2 a3
yEqualsZero :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
yEqualsZero (AbsTriple a1 a2 a3) = AbsTriple a1 (equalsZero a2) a3
zEqualsZero :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
zEqualsZero (AbsTriple a1 a2 a3) = AbsTriple a1 a2 (equalsZero a3)

xNequalsZero :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
xNequalsZero (AbsTriple a1 a2 a3) = AbsTriple (nequalsZero a1) a2 a3
yNequalsZero :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
yNequalsZero (AbsTriple a1 a2 a3) = AbsTriple a1 (nequalsZero a2) a3
zNequalsZero :: IntegerAbstraction a => AbstractTriple a -> AbstractTriple a
zNequalsZero (AbsTriple a1 a2 a3) = AbsTriple a1 a2 (nequalsZero a3)
