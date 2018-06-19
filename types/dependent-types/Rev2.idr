import Data.Vect

reverseProof : Vect (n + 1) a -> Vect (S n) a
reverseProof {n} vec = rewrite (plusCommutative 1 n) in vec

myReverse : Vect a n -> Vect a n
myReverse Nil = Nil
myReverse (x::xs) = let result = myReverse xs ++ [x]
                     in reverseProof result

-- :t plusCommutative
-- :printdef plusCommutative

main : IO ()
main = putStrLn "hello"
