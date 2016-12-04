module Main where

import Data.List.NonEmpty hiding (length, head, tail)
import qualified Data.List.NonEmpty as NE (length, nonEmpty)

average1 :: Fractional a => [a] -> a
average1 xs
  | null xs = realToFrac 0
  | otherwise = sum xs / realToFrac (length xs)


average2 :: Fractional a => [a] -> Maybe a
average2 xs
  | null xs = Nothing
  | otherwise = Just $ sum xs / realToFrac (length xs)


average3 :: Fractional a => NonEmpty a -> a
average3 xs = sum xs / realToFrac (NE.length xs)

maybeAvg xs = average3 <$> (NE.nonEmpty xs)

main :: IO ()
main = do
  print $ NE.nonEmpty ([1..10] :: [Double])
  print $ NE.nonEmpty ([] :: [Double])

  print $ maybeAvg ([1..10] :: [Double])
  print $ maybeAvg ([] :: [Double])
