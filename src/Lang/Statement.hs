module Lang.Statement where

import Data.Set as Set

type PC = Int
type State = (PC, Int, Int, Int)

data Var = X | Y | Z deriving(Show)

data Statement = Stop
               | Inc Var
               | Dec Var
               | Zero Var PC PC deriving(Show)

type Program = [Statement]

programPoints :: Program -> Set PC
programPoints [] = error "illegal empty program"
programPoints prog = Set.fromList [1..(length prog)]

programCounter :: State -> PC
programCounter (pc,_,_,_) = pc
xRegister :: State -> Int
xRegister (_,x,_,_) = x
yRegister :: State -> Int
yRegister (_,_,y,_) = y
zRegister :: State -> Int
zRegister (_,_,_,z) = z

incX :: Statement -> Bool
incX (Inc X) = True
incX _ = False

incY :: Statement -> Bool
incY (Inc Y) = True
incY _ = False

incZ :: Statement -> Bool
incZ (Inc Z) = True
incZ _ = False

decX :: Statement -> Bool
decX (Dec X) = True
decX _ = False

decY :: Statement -> Bool
decY (Dec Y) = True
decY _ = False

decZ :: Statement -> Bool
decZ (Dec Z) = True
decZ _ = False

zeroX :: Statement -> Bool
zeroX (Zero X _ _) = True
zeroX _ = False

zeroY :: Statement -> Bool
zeroY (Zero Y _ _) = True
zeroY _ = False

zeroZ :: Statement -> Bool
zeroZ (Zero Z _ _) = True
zeroZ _ = False
