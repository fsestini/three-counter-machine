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
