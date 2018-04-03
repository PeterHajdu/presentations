{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

data Nat =
    Zero
  | Succ Nat

type family Add n m where
  Add 'Zero m = m
  Add ('Succ n) m = 'Succ (Add n m)

data Vector :: Nat -> a -> * where
  Nil :: Vector 'Zero a
  Cons :: a -> Vector n a -> Vector ('Succ n) a

data Fin :: Nat -> * where
  Z :: Fin ('Succ n)
  S :: Fin n -> Fin ('Succ n)

atVec :: Vector n a -> Fin n -> a
atVec (Cons a _) Z = a
atVec (Cons a rest) (S n) = atVec rest n

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append Nil r = r
append (Cons a lrest) r = Cons a (append lrest r)

vecHead :: Vector ('Succ n) a -> a
vecHead (Cons a _) = a

vecTail :: Vector ('Succ n) a -> Vector n a
vecTail (Cons a rest) = rest

oneSize = (Cons 1 Nil)
twoSize = (Cons 2 oneSize)
threeSize = append oneSize twoSize

a = atVec threeSize (S (S Z))

main :: IO ()
main = do
  print $ vecHead twoSize

