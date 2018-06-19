import Data.Vect

myReverse : Vect a n -> Vect a n
myReverse Nil = Nil
myReverse (x::xs) = myReverse xs ++ [x]

-- :printdef plus

main : IO ()
main = putStrLn "hello"
