---
author: 'Peter Hajdu'
title: 'Introduction to dependent types'
...

# Dependent types

 * types indexed by data values
 * types expressing logical formulas

# Dependent type capabilities

 * type computation
 * indexed types
 * double-duty data
 * equivalence proofs

Stephanie Weirich

# Fixed size vectors

``` idris
aVector : Vec 3 Int
aVector = [1, 2, 3]
```

 * vector / list
 * head []
 * zip [42] ["meaning", "of", "life"]
 * if null $ [42] ++ [100, 200] then ... else ...

# Peano numbers

``` haskell
data Nat =
    Zero
  | Succ Nat
```

 * Succ Zero ?
 * 2 ?

# Nat as a kind

``` haskell
data Nat =
    Zero
  | Succ Nat
```

# Nat as a kind

 * :k Int
 * :k Nat
 * :t Zero
 * :k 'Zero
 * :k ('Succ ('Succ 'Zero))
 * :t ('Succ ('Succ 'Zero))
 * :t (Succ (Succ Zero))

# Vector / GADTs

``` haskell
data Vector :: Nat -> a -> * where
  Nil :: Vector 'Zero a
  Cons :: a -> Vector n a -> Vector ('Succ n) a
```

 * :t Nil
 * :t Cons 10 Nil
 * :t Cons 10 (Cons 20 Nil)

# head 1

``` haskell
head :: Vector ??? a -> a
head (Cons a _) = a
```

# head 2

``` haskell
head :: Vector ('Succ n) a -> a
head (Cons a _) = a
```

 * head Nil?
 * incomplete pattern match?

# tail

``` haskell
tail :: Vector ('Succ n) a -> Vector n a
tail (Cons _ rest) = rest
```

# zip 1

``` haskell
zip :: Vector ??? a -> Vector ??? b -> Vector ??? (a, b)
zip (Cons l lrest) (Cons r rrest) = Cons (l, r) $ zip lrest rrest
zip Nil _ = Nil
```

# zip 2

``` haskell
zip :: Vector n a -> Vector n b -> Vector n (a, b)
zip (Cons l lrest) (Cons r rrest) = Cons (l, r) $ zip lrest rrest
zip Nil _ = Nil
```

 * zip (Cons 1 Nil) (Cons "a" Nil)
 * zip (Cons 1 Nil) (Cons "a" (Cons "b" Nil))


# nth element 1

``` haskell
at :: Vector n a -> Nat -> Maybe a
```

# nth element 2

``` haskell
at :: Vector n a -> Nat -> Maybe a
at :: Vector n a -> Fin n -> a
```

# Fin

``` haskell
data Fin :: Nat -> * where
  Z :: Fin ('Succ n)
  S :: Fin n -> Fin ('Succ n)
```

 * 0 ?
 * 2 ?
 * Fin 'Zero ?
 * :t S Z
 * :t S (S Z)

# nth element 3

``` haskell
at :: Vector n a -> Fin n -> a
at (Cons a _) Z = a
at (Cons _ rest) (S n) = at rest n
at Nil _ = impossible

impossible = undefined
```

# append 1

``` haskell
append :: Vector n a -> Vector m a -> Vector ??? a
```

# append 2

``` haskell
append :: Vector n a -> Vector m a -> Vector ??? a
append :: Vector n a -> Vector m a -> Vector (Add n m) a
```

 * :t append Nil (Cons 10 Nil)
 * :t append (Cons 30 (Cons 20 Nil)) (Cons 10 Nil)

# Add

```haskell
type family Add n m where
  Add 'Zero m = m
  Add ('Succ n) m = 'Succ (Add n m)
```

# append 3

``` haskell
append :: Vector n a -> Vector m a -> Vector (Add n m) a
append Nil r = r
append (Cons a lrest) r = Cons a (append lrest r)
```

