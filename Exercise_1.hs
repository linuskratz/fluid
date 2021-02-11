module Exercise_1 where

import Test.QuickCheck

{-H1.1-}
myPair :: Integer -> Integer -> Integer
myPair x y =  (2^x) *(2*y+1)

{-H1.2-}
myFst :: Integer -> Integer
myFst n =
  myFst_help n  0

myFst_help :: Integer -> Integer -> Integer
myFst_help n x
    | n `mod` 2 == 0  =  myFst_help (n `div` 2)  (x+1)
    | otherwise = x

{-H1.3-}
mySnd :: Integer -> Integer
mySnd n
      | n `mod` 2 == 0  =  mySnd (n `div` 2)
      | otherwise = (n-1) `div` 2

{-H1.4-}
prop_myPair :: Integer -> Integer -> Property
prop_myPair n x = (n >= 0) && (x>= 0) ==> n ==  myFst (myPair n x) && x == mySnd (myPair n x)

{-H2.1-}
equivMod :: Integer -> Integer -> Integer -> Bool
equivMod n a b =
  a `mod`n == b `mod` n

{-H2.2-}
quadRes :: Integer -> Integer -> Bool
{-WETT-}
quadRes n a =
  quadRes_help n a 0

quadRes_help :: Integer -> Integer -> Integer -> Bool
quadRes_help n a x
    | n > x = if  equivMod n a (x^2) then True else quadRes_help n a (x+1)
    | otherwise = False

{-TTEW-}

--
quadRes_1 n a =
  null [x | x <-[0.. (n -1)], not(equivMod n a (x^2)) ]

{-
quadRes_help :: Integer -> Integer -> Integer -> Bool
quadRes_help n a x
    | n > x = if  equivMod n a (x^2) then True else quadRes_help n a (x+1)
    | otherwise = False

  -}

{-H2.3-}
legendre :: Integer -> Integer -> Integer
legendre n a
  | a `mod`n == 0 = 0
  | otherwise = if quadRes n a then 1 else -1


{-H2.4-}
prime :: Integer -> Bool
prime a = prime_help 2 a

prime_help :: Integer ->Integer -> Bool
prime_help n a
  | a <= 1 = False
  | n <= (a `div` 2) = if (a`mod`n) == 0 then False else prime_help (n+1) a
  | otherwise = True

{-H2.5-}
prop_eulersCrit :: Integer -> Integer -> Property
prop_eulersCrit p a = (p > 2) && prime p ==> equivMod p (legendre p a) (a^((p-1)`div`2))
