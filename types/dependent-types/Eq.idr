%default total

sameS : (n = m) -> (S n = S m)
sameS Refl = Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z (S n) = Nothing
checkEqNat (S n) Z = Nothing
checkEqNat Z Z = Just Refl
checkEqNat (S n) (S m) = case checkEqNat n m of
                              Nothing => Nothing
                              Just eq => Just (sameS eq)


valueNotSucc : x = S x -> Void
valueNotSucc Refl impossible

zeroNotSucc : (0 = S n) -> Void
zeroNotSucc Refl impossible

succNotZero : (S n = 0) -> Void
succNotZero Refl impossible

noRec : ((k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat2 : (n1 : Nat) -> (n2 : Nat) -> Dec (n1 = n2)
checkEqNat2 Z Z = Yes Refl
checkEqNat2 Z (S n) = No zeroNotSucc
checkEqNat2 (S n) Z = No succNotZero
checkEqNat2 (S i) (S j) = case checkEqNat2 i j of
                               Yes i_eq_j => Yes (cong i_eq_j)
                               No i_eq_j_void => No (noRec i_eq_j_void)


main : IO ()
main = putStrLn "hello"
