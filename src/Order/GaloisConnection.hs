{-# LANGUAGE MultiParamTypeClasses #-}

module Order.GaloisConnection where

import Order.Poset

class (Poset c, Poset a) => GaloisConnection c a where
  alpha :: c -> a
  gamma :: a -> c
