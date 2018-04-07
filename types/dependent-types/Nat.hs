{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Nat where

data Nat =
    Zero
  | Succ Nat

type family Add n m where
  Add 'Zero m = m
  Add ('Succ n) m = 'Succ (Add n m)

data SNat :: Nat -> * where
  SZ :: SNat 'Zero
  SSucc :: (SNat n) -> SNat ('Succ n)

