{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main where

import Vec
import Nat

simpleVec :: Vector ('Succ ('Succ ('Succ 'Zero))) Int
simpleVec = Cons (10 :: Int) (Cons 20 (Cons 30 Nil))

printVec :: Show a => Vector n a -> IO ()
printVec Nil = pure ()
printVec (Cons a rest) = print a >> printVec rest

printSomeVector :: Show a => SomeVector a -> IO ()
printSomeVector (MkSomeVector (SSucc n) (Cons a rest)) = do
  print a
  printSomeVector $ MkSomeVector n rest
printSomeVector _ = pure ()

--readVec :: IO (Vector n String)
--readVec = do
--  line <- getLine
--  if null line
--  then pure Nil
--  else do
--    rest <- readVec
--    pure $ Cons line rest

readVec :: IO (SomeVector String)
readVec = do
  line <- getLine
  if null line
  then pure $ MkSomeVector SZ Nil
  else do
    rest <- readVec
    pure $ consSomeVec line rest

main :: IO ()
main = do
  vec <- readVec
  printSomeVector vec
