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
-> Examples with numbers

---
-> Examples with numbers
-> sum, product

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
