{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Nat where

data Nat =
    Zero
  | Succ Nat

type family Add n m where
  Add 'Zero m = m
  Add ('Succ n) m = 'Succ (Add n m)
