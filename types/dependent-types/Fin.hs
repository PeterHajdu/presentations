{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Fin where

import Nat

data Fin :: Nat -> * where
  Z :: Fin ('Succ n)
  S :: Fin n -> Fin ('Succ n)

