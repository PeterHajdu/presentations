---
-> monoid

---
-> algebraic structure with the following properties

---
-> algebraic structure with the following properties
-> single associative binary opertation
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

--
data Sum a = Sum {getSum :: a}

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
-> first, last

---
-> Examples with tuples

---
-> Examples with tuples
-> (a, b) <> (c, d) == (a<>b), c<>d)

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
-> f lst = lst <> lst
-> g lst = tail lst
-> h = f <> g

---
-> Examples with functions
-> f :: a -> a
-> g :: a -> a
-> h = f <> g

---
-> Examples with functions
-> f :: a -> a
-> g :: a -> a
-> h = f <> g :: a -> a
-> What is mempty?

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
