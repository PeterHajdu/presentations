---
-> Adventures in Monadia
-> monoid

---
-> algebraic structure with the following properties

---
-> algebraic structure with the following properties
-> single associative binary opertation: <>
-> (a<>b)<>c == a<>(b<>c)

---
-> algebraic structure with the following properties
-> single associative binary opertation: <>
-> (a<>b)<>c == a<>(b<>c)
-> identity element: mempty
-> mempty<>a == a<>mempty == a

---
-> Representation in haskell
class Monoid where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

---
-> Representation in scala
trait Monoid[@sp(Int, Long, Float, Double) A] extends Any with Semigroup[A] {
      def empty: A
      ...
}

trait Semigroup[@sp(Int, Long, Float, Double) A] extends Any with Serializable {
      def combine(x: A, y: A): A
      ...
}

---
-> Examples with numbers

---
-> Examples with numbers
-> sum, product

---
newtype Sum a = Sum {getSum :: a}

instance Num a => Monoid(Sum a) where
  mappend (Sum a) (Sum b) = Sum(a+b)
  mempty = Sum 0

---
-> Examples with numbers
-> sum, product
-> max, min

---
-> Examples with booleans

---
-> Examples with booleans
-> all, any

---
-> Examples with lists

---
-> Examples with lists
-> [1..50] <> [51..100] == [1..100]

---
-> Examples with Maybe (Optional)

---
-> Examples with Maybe (Optional)
-> default behavior

---
-> Examples with Maybe (Optional)
-> default behavior
-> first, last

---
-> Examples with tuples

---
-> Examples with tuples
-> (a, b) <> (c, d) == (a<>c, b<>d)

---
-> Examples with functions
-> Monoid e, a -> e
-> f :: a -> e
-> g :: a -> e
-> h = f <> g :: a -> e

---
-> Examples with functions
-> Monoid e, a -> e
-> f :: a -> e
-> g :: a -> e
-> h = f <> g :: a -> e
-> What is mempty?

---
-> Examples with functions
-> f n = [1..n]
-> g n = tail [1..n]
-> h = f <> g

---
-> Examples with functions
-> f :: a -> a
-> g :: a -> a
-> h = f <> g :: a -> a

---
-> Examples with functions
-> f :: a -> a
-> g :: a -> a
-> h = f <> g :: a -> a
-> What is mempty?

---
newtype Endo a = Endo {appEndo :: Int -> Int}
instance Monoid (Endo a) where
  mempty = Endo id
  mappend (Endo f) (Endo g) = Endo (f.g)

---
-> Semigroup
class Semigroup where
  mappend :: a -> a -> a

mappend has to be associative

---
-> A semigroup that is not a monoid?

---
-> A semigroup that is not a monoid?
data NonEmpty a = a :| [a]

---
-> Where are they used?
-> Foldable

---
-> Where are they used?
-> Foldable
-> Writer monad

---
-> Where are they used?
-> Cabal:
-> config files
-> package databases
-> sets of command line flags

---
-> Where are they used?
-> Xmonad:
-> configuration hooks

---
-> Where are they used?
-> diagrams (vectorial image generation):
-> graphic elements

---
-> What use are monoids to a programmer?
-> Associativity

---
-> What use are monoids to a programmer?
-> Associativity
-> divide and conquer algorithms

---
-> What use are monoids to a programmer?
-> Associativity
-> divide and conquer algorithms
-> parallelization

---
-> What use are monoids to a programmer?
-> Associativity
-> divide and conquer algorithms
-> parallelization
-> incrementalism

---
-> What use are monoids to a programmer?
-> Identity

---
-> What use are monoids to a programmer?
-> Identity
-> empty lists

---
-> What use are monoids to a programmer?
-> Identity
-> empty lists
-> divide and conquer algorithm: leaf

---
-> What use are monoids to a programmer?
-> Identity
-> empty lists
-> divide and conquer algorithm: leaf
-> incremental algorithms: start element
