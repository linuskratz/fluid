module TUT2 where

import Test.QuickCheck

allSums :: [Integer] -> [Integer] -> [Integer]
allSums xs ys =
  [x+y | x <- xs, y<-ys]

evens :: [Integer] -> [Integer]
evens xs =
  [x | x <- xs, x `mod` 2 == 0]



nLists :: [Integer] -> [[Integer]]
nLists xs =
  [[1..x] | x<-xs]


allEvenSumLists :: [ Integer ] -> [ Integer ] -> [[ Integer ]]
allEvenSumLists xs ys =
         [[1..(x+y)]| x<-xs, y<-ys , (x+y)`mod`2 == 0]

prop_allEvenSumLists :: [Integer]-> [Integer]-> Bool
prop_allEvenSumLists xs ys =
  allEvenSumLists xs ys == (nLists  $ evens  $ allSums xs ys)

toSet :: [Integer] -> [Integer]
toSet xs
  |null xs = []
  |otherwise =  if (head xs) `elem` tail(xs)
                then toSet $ tail xs
                else (head xs) : (toSet $ tail xs)


isSet :: [Integer] -> Bool
isSet xs
  | null xs = True
  | otherwise = if (head xs) `elem` tail (xs)
                then False
                else isSet $ tail(xs)

isSet2 :: [Integer] ->Bool
isSet2 xs =
  (length $ toSet xs) == length xs


union :: [Integer] -> [Integer] -> [Integer]
union xs ys = toSet (xs ++ ys)


intersection :: [Integer] -> [Integer] -> [Integer]
intersection xs ys = toSet[x | x<-xs, x `elem` ys]

diff :: [Integer] -> [Integer] -> [Integer]
diff xs ys = toSet[x | x<-xs, not(x `elem` ys)]

prop_union :: [Integer] -> [Integer] -> Integer -> Property
prop_union s t a =
  a `elem` (s `union` t) ==> (a `elem` s || a `elem` t)

eqFrac :: ( Integer , Integer ) -> ( Integer , Integer ) -> Bool
eqFrac (a, b) (c, d) =
  a * d == b*c


pow2 :: Integer -> Integer
pow2 0 = 1
pow2 a
 | a `mod` 2 == 0 = (pow2 (a `div`2))^2
 | otherwise = 2 * pow2 (a-1)

prop_sym ::( Integer , Integer ) -> ( Integer , Integer ) -> Bool
prop_sym (a,b) (c,d) =
  eqFrac (a,b)(c,d) == eqFrac (c,d)(a,b)
