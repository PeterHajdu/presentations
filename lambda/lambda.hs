import Prelude hiding (mult, sum, succ, pred, pair, fst, snd, id, True, False, or, not, and)

true = \x -> \y -> x
false = \x -> \y -> y

not = \b -> b false true

assert = \b -> print (b "Success" "Fail")
refute = \b -> assert (not b)

or = \x y -> x true y


pair = \x y b -> b x y
fst = \p -> p true
snd = \p -> p false

zero = \s z -> z
isZero = \n -> n (\x -> false) true
succ = \n s z -> s (n s z)

pred = \n s z -> n (\g h -> h (g s)) (\u -> z) (\u -> u)

one = succ zero
sum = \x y s z -> x s (y s z)

two = succ one
mult = \x y s z -> x (y s) z

main = do
  assert true
  assert (not false)
  refute false

  assert (or true false)
  refute (or false false)
  assert (or false true)

  assert (fst (pair true false))
  refute (fst (pair false true))
  refute (snd (pair true false))
  assert (snd (pair false true))

  assert (isZero zero)
  refute (isZero (succ zero))
  assert (isZero (pred (succ zero)))
  assert (isZero (pred (pred (sum one one))))
  assert (isZero (pred (pred (mult two one))))
