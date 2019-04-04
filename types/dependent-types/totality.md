---
author: 'Peter Hajdu'
title: 'Totality'
...

# Setting the stage

 * Why do we use types?

# Types talk

``` haskell
question1 :: a -> a
question2 :: (b, a) -> a
question3 :: NonEmpty a -> a
```

# Without totality 1

``` haskell
but :: [a] -> a
```

``` scala
"".head
```

``` haskell
id :: a -> a
id x = undefined
```

# Without totality 2

Knowing that a function is total allows you to make much stronger claims about its behavior
based on its type.

 * total?

# Partial functions 1

``` haskell
  f :: D -> R
```

# Partial functions 2

 * All well-typed inputs are covered

``` scala
sealed trait Nat
case object Z extends Nat
final case class S(pred: Nat) extends Nat

def dec(x: Nat): Nat = x match {
  case S(p) => p
}
```

# Partial functions 3

 * Always returns a well-typed result

``` scala
def head(): Char =
  if ( isEmpty() ) throw NoSuchElementException()
  else ...
```

# Partial functions 4

``` haskell
inc :: Int -> Int
inc x = x + 1

inc' :: Int -> Int
inc' x = inc x
```

 * Always return a well-typed result within a *finite time*.

# Impact on non dependently typed languages

 * runtime errors

# Impact on dependently typed languages

``` idris
append : a -> Vect len a -> Vect (len + 1) a
```

 * compile time calculations

# Impact on Curry-Howard correspondence 1.

``` idris
data Less: a ->  b -> Type  where
  LessZ : Less Z (S n)
  LessS : Less a b -> Less (S a) (S b)
```

``` haskell
loop :: Int -> Int
loop n = 1 + loop n

-- loop 0 = 1 + loop 0
-- 0 = 1
```

 * nontermination is equivalent to logical inconsistency

# Almost totality

For all well-typed inputs a total function always returns a well-typed result within a finite time.

# Halting problem

 * Determine if a program will finish running or continue to run forever for a given input.
 * Alan Turing 1936

# Termination checking 1

``` idris
total
inc : Int -> Int
inc x = inc x
```

 * Main.inc is possibly not total due to recursive path Main.inc --> Main.inc

# Termination checking 2

``` idris
total
inc : List Int -> List Int
inc Nil = Nil
inc (x::xs) = (x + 1)::(inc xs)
```

 * A conservative approximation by analyzing a function's syntax.

# Non-terminating programs

 * servers
 * interactive programs
 * "Even if a program generates an infinite amount of data, each piece will be generated
   in finite time."
 * productivity

# Terminating or productive

A total function is a function that, for all well-typed inputs, does one of the following:

 * Terminates with a well-typed result
 * Produces a non-empty finite prefix of a well-typed infinite result in finite time

# Infinite data structures in lazy languages

``` haskell
ones :: [Int]
ones = 1 : ones

twos :: [Int]
twos = 2 : tail twos
```

# Infinite data structures in strict languages 1.

``` idris
data Stream : Type -> Type where
  (::) : (e : a) -> Inf (Stream a) -> Stream a
```

# Infinite data structures in strict languages 2.

``` idris
total
ones : Stream Nat
ones = 1 :: ones

total
twos : Stream Nat
twos = 2 :: (tail twos)

-- Main.twos is possibly not total due to recursive path Main.twos
```

 * codata
 * corecursion
 * productivity: in practice, all recursive calls are constructor guarded (idris wiki)

# Interactive total programs (from TDD book) 1.

``` idris
module Main

quiz : Stream Int -> IO ()
quiz (rnd1 :: rnd2 :: rest) = do
  putStr $ show rnd1 ++ " * " ++ show rnd2 ++ "? "
  answer <- getLine
  if answer == (show $ rnd1 * rnd2)
    then do putStrLn "Correct!"
    else do putStrLn "Nop!"
  quiz rest

main : IO ()
main = quiz (iterate (+1) 0)
```

  * iterate: terminating or productive?
  * quiz: terminating or productive?

# Interactive total programs 2.

quiz:

  * reads user input
  * produces an infinite sequence of IO actions
  * total version?

# Interactive total programs 3.

Describe the program as a:

  * potentially non terminating
  * but productive function
  * with an infinite data structure

# Interactive total programs 4.

``` idris
data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO
```

# Interactive total programs 5.

``` idris
data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

total
loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg) (\_ => loopPrint msg)
```

 * interpreter

# Interactive total programs 6.

``` idris
data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

total
loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg) (\_ => loopPrint msg)

run : InfIO -> IO ()
run (Do action cont) = do
  res <- action
  run (cont res)
```

 * terminating or productive?
 * how would you solve it?


# Interactive total programs 7.

``` idris
total
run : Nat -> InfIO -> IO ()
run (S fuel) (Do action cont) = do
  res <- action
  run fuel (cont res)
run Z _ = pure ()
```

 * terminating or productive?
 * but it terminates after n iterations

# Interactive total programs 8.

``` idris
data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever
```

# Interactive total programs 9.

``` idris
module Main

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

total
loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg) (\_ => loopPrint msg)

total
run : Fuel -> InfIO -> IO ()
run (More fuel) (Do action cont) = do
  res <- action
  run fuel (cont res)
run Dry _ = pure ()

partial
main : IO ()
main = run forever (loopPrint "hello")
```

# Interactive total programs 10.

``` idris
--data Fuel = Dry | More (Lazy Fuel)
data Fuel = Dry | More (Inf Fuel)

total
forever : Fuel
forever = More forever
```

 * Lazy != Inf
 * termination checker for terminating and productive functions

# Interactive total programs 11.

``` idris
data Fuel = Dry | More (Inf Fuel)

total
forever : Fuel
forever = More forever

partial
run : Fuel -> InfIO -> IO ()
run (More fuel) (Do action cont) = do
  res <- action
  run fuel (cont res)
run Dry _ = pure ()
```
