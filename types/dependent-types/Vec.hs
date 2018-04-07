{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Vec where

import Nat
import Fin

data Vector :: Nat -> a -> * where
  Nil :: Vector 'Zero a
  Cons :: a -> Vector n a -> Vector ('Succ n) a

headVec :: Vector ('Succ n) a -> a
headVec (Cons a _) = a

tailVec :: Vector ('Succ n) a -> Vector n a
tailVec (Cons _ rest) = rest

appendVec :: Vector n a -> Vector m a -> Vector (Add n m) a
appendVec Nil r = r
appendVec (Cons a lrest) r = Cons a (appendVec lrest r)

zipVec :: Vector n a -> Vector n b -> Vector n (a, b)
zipVec (Cons l lrest) (Cons r rrest) = Cons (l, r) $ zipVec lrest rrest
zipVec Nil _ = Nil

atVec :: Vector n a -> Fin n -> a
atVec (Cons a _) Z = a
atVec (Cons _ rest) (S n) = atVec rest n
atVec Nil _ = undefined

data SomeVector :: a -> * where
  MkSomeVector :: SNat n -> Vector n a -> SomeVector a

consSomeVec :: a -> SomeVector a -> SomeVector a
consSomeVec item (MkSomeVector n oldVec) = MkSomeVector (SSucc n) (Cons item oldVec)

replicateVec :: a -> SNat n -> Vector n a
replicateVec _ SZ = Nil
replicateVec item (SSucc rest) = Cons item $ replicateVec item rest
