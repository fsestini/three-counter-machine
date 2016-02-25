module Lang.Transition where

import Lang.Statement

statementAt :: Program -> Int -> Statement
statementAt [] _ = error "invalid program counter"
statementAt (stmt:rest) pc = if pc == 1 then stmt else statementAt rest (pc - 1)

-- Transition relation τ ⊆ State × State
tau :: Program -> State -> State
tau [] _ = error "invalid empty program"
tau prog (pc,x,y,z) = case (statementAt prog pc) of
                           (Inc X) -> (pc+1, x+1, y, z)
                           (Inc Y) -> (pc+1, x, y+1, z)
                           (Inc Z) -> (pc+1, x, y, z+1)
                           (Dec X) -> (pc+1, x-1, y, z)
                           (Dec Y) -> (pc+1, x, y-1, z)
                           (Dec Z) -> (pc+1, x, y, z-1)
                           (Zero X pc' pc'') -> if x == 0
                                                   then (pc', x, y, z)
                                                   else (pc'', x, y, z)
                           (Zero Y pc' pc'') -> if y == 0
                                                   then (pc', x, y, z)
                                                   else (pc'', x, y, z)
                           (Zero Z pc' pc'') -> if z == 0
                                                   then (pc', x, y, z)
                                                   else (pc'', x, y, z)
                           Stop -> error "invalid tau on Stop statement"

-- Initial-to-final transition relation
transclo :: Program -> State -> State
transclo program state@(pc, x, y, z) =
    case statementAt program pc of
         Stop -> state
         otherwise -> transclo program (tau program state)

-- Program interpreter
eval :: Program -> Int
eval program = yRegister $ transclo program (1, 0, 0, 0)
