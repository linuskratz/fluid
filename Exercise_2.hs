module Exercise_2 where

import Data.Ratio
import Test.QuickCheck hiding (choose)
import Data.List hiding (union)

{-H2.1.1-}
removeInvalidGuesses :: [(String,Int)] -> [(String,Int)]
removeInvalidGuesses xs =
  [(x,y) | (x,y)<-xs, y>=0, y<=100, x /= ""]
{-H2.1.2-}

average :: [(String,Int)] -> Int
average [] = 0
average xs =  sum (extractNumbers xs) `div` length (extractNumbers xs)

extractNumbers :: [(String,Int)] -> [Int]
extractNumbers xs = [y | (x,y)<-xs]

{-H2.1.3-}
winners :: [(String,Int)] -> [String]
winners xs =
  [x | (x,y) <- noInvalid, y `elem` winnerVal]
  where winnerVal = winnersValues noInvalid avg
        avg = average noInvalid
        noInvalid = removeInvalidGuesses xs

winnersValues :: [(String,Int)] -> Int -> [Int]
winnersValues [] x = []
winnersValues (x:xs) avg =
  minDistance (snd(x)) (winnersValues xs avg) avg


minDistance ::Int -> [Int] ->Int ->[Int]
minDistance x ys avg
  | null ys = [x]
  | abs (x-avg) < abs (head(ys)-avg) = [x]
  | abs (x-avg) == abs (head(ys)-avg) = [x] ++ ys
  | otherwise = ys


{-H2.2-}
-- Computes the binomial coefficient "n choose k"
choose :: Integer -> Integer -> Integer
n `choose` k = product [n-k+1..n] `div` product [1..k]

bernoulli :: Integer -> Rational
{-WETT-}
bernoulli 0 = 1
bernoulli n = sum [(fromIntegral(n `choose` k) * bernoulli k) / fromIntegral(k-n-1) | k <-[0..n-1]]
{-TTEW-}

{-H2.3-}
-- toSet l removes the duplicates of a list l
toSet :: Eq a => [a] -> [a]
toSet = nub

-- union s t builds the union of s and t
union :: Eq a => [a] -> [a] -> [a]
union xs ys = toSet $ xs ++ ys


{-H2.3.1-}
power :: [Integer] -> [[Integer]]
power [] = [[]]
power xs=
  union (reduceBrakets([power(delete y xs)|y<-xs])) [xs]

reduceBrakets [] = [[]]
reduceBrakets (x:xs) =
  x ++ reduceBrakets xs

subsetEq :: [Integer] -> [Integer] -> Bool
subsetEq s t = sort (t) == sort (union s t)

{-H2.3.2-}
comparable :: [Integer] -> [Integer] -> Bool
comparable xs xt= (sort(union xs xt) == sort xs || sort(union xs xt) == sort(xt))

{-H2.3.3-}
isAntichain :: [[Integer]] -> Bool
isAntichain xss = not(True `elem`[comparable xs ys | ys<-xss, xs <-xss, xs /=ys])


{-H2.3.4-}
antichains :: Integer -> [[[Integer]]]
antichains n
  | n < 1 = [[], [[]]]

{-H2.3.5-}
maxAntichainSize :: [[[Integer]]] -> Int
maxAntichainSize = undefined

prop_spernersTheorem :: Integer -> Property
prop_spernersTheorem = undefined
