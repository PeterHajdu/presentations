---
author: 'Peter Hajdu'
title: 'Curry Howard correspondence'
...

# Curry Howard correspondence

 * Haskell Curry
 * William Alvin Howard

# Curry Howard correspondence

 * link between logic and computation

# Nat

``` idris
data Nat : Type where
  Z : Nat
  S : Nat -> Nat
```

# Equality 1

``` idris
data (=) : a -> b -> Type where
  Refl : x = x
```

type, value, valid:

 * (Nat = Nat)
 * the (Nat = Nat) Refl
 * (Nat = String)
 * the (Nat = String) Refl
 * (3 = 3)
 * the (3 = 3) Refl
 * (2 = 3)
 * the (2 = 3) Refl
 * ("kutyus" = "kutyus")
 * the ("kutyus" = "kutyus") Refl

# Equality 2

``` idris
checkEqNat : Nat -> Nat -> Maybe (n1 = n2)
```

# Equality 3

``` idris
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
```

# Equality 4

``` idris
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z (S n) = Nothing
checkEqNat (S n) Z = Nothing
checkEqNat Z Z = ?
checkEqNat (S n) (S m) = ?
```

# Equality 5

``` idris
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z (S n) = Nothing
checkEqNat (S n) Z = Nothing
checkEqNat Z Z = Just $ Refl
checkEqNat (S n) (S m) = ?
```

# Equality 6

``` idris
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z (S n) = Nothing
checkEqNat (S n) Z = Nothing
checkEqNat Z Z = Just $ Refl
checkEqNat (S n) (S m) = case checkEqNat n m of
                              Nothing => Nothing
                              Just p1 => Just p2
```

 * :t p1 => (n = m)
 * :t p2 => (S n = S m)

# Equality 7

``` idris
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z (S n) = Nothing
checkEqNat (S n) Z = Nothing
checkEqNat Z Z = Just $ Refl
checkEqNat (S n) (S m) = case checkEqNat n m of
                              Nothing => Nothing
                              Just p1 => Just (sameS p1)
```

 * :t p1 => (n = m)
 * :t (sameS p1) => (S n = S m)
 * :t sameS => (n = m) -> (S n = S m)

# Equality 8

``` idris
(n = m) -> (S n = S m)
```

?

 * equality proposition
 * implication

# Equality 9

``` idris
sameS : (n = m) -> (S n = S m)
sameS Refl = Refl
```

Curry Howard correspondence:

 * types are propositions
 * values are proofs

# Example reverse

 * rewrite
 * :t sym
 * :t cong

# Negation

expressing negation:

 * not p
 * p -> impossible

# Stating the impossible 1

``` idris
data Void : Type where
```

# Stating the impossible 2

 * not (x = S x)
 * (x = S x) -> impossible
 * (x = S x) -> Void

# Stating the impossible 3

 * f: (x = S x) -> Void
 * referential transparency

# Stating the impossible 4

``` idris
valueNotSucc : x = S x -> Void
valueNotSucc Refl impossible
```

# Principle of explosion 1

``` idris
t1 : A -> Void

something : A -> B
something a = t1 a

-- Void ? B
```

# Principle of explosion 2

 * from contradiction, anything follows
 * a contradictory hypothesis entails anything, even false things

# Principle of explosion 3

``` idris
void : Void -> a
```

# Principle of explosion 4


``` idris
void : Void -> a

t1 : A -> Void

something : A -> B
something a = void $ t1 a
```

# Decidability 1

``` idris
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
```

# Decidability 2

``` idris
data Dec : (prop : Type) -> Type where
  Yes : (proof : prop) -> Dec prop
  No : (contra : prop -> Void) -> Dec prop
```

# Decidability 3

``` idris
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat2 : (n1 : Nat) -> (n2 : Nat) -> Dec (n1 = n2)
```

# Decidability 4

``` idris
checkEqNat2 : (n1 : Nat) -> (n2 : Nat) -> Dec (n1 = n2)
checkEqNat2 Z Z = Yes Refl
checkEqNat2 Z (S n) = No zeroNotSucc
checkEqNat2 (S n) Z = No succNotZero
checkEqNat2 (S i) (S j) = case checkEqNat2 i j of
                               Yes i_eq_j => Yes (cong i_eq_j)
                               No i_eq_j_void => No (noRec i_eq_j_void)

cong : (a = b) -> (f a = f b)

zeroNotSucc : (0 = S n) -> Void
zeroNotSucc Refl impossible

succNotZero : (S n = 0) -> Void
succNotZero Refl impossible

noRec : ((k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl
```

# Further reading

 * https://softwarefoundations.cis.upenn.edu/
 * Type-Driven Development with Idris by Edwin Brady
 * Verified Functional Programming in Agda by Aaron Stump

