module Order.Poset where

class Poset p where
  leq :: p -> p -> Bool
