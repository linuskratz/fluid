module TUT3 where

import Test.QuickCheck

toSet_1 :: [Integer]->[Integer]
toSet_1 [] = []
toSet_1 (x:xs) =
  if x `elem` xs
  then toSet1 xs
  else x : toSet1 xs


dimensions :: [[a]] -> (Int,Int)
dimensions [] = (0,0)
dimensions (x:xs) =
  let l = length x in
  if and [length ys == l | ys<-xs]
  then length (xs +1, l)
  else (-1,-1)

{- and [True, True] = True and [True, False] = False-}

isSquare :: [[a]] -> Bool
isSquare [] = True
isSquare (xs:xss) =
  dimensions xss /= (-1,-1) && length (xs:xss) == length xs

isSquare_2 xs =
  let (m,n) = dimensions xs
  in m >= 0 && m == n

canAdd :: [[a]] -> [[a]] -> Bool
canAdd xs ys = dimensions xs /= (-1,-1) && dimensions xs == dimensions ys

canMult :: [[a]] -> [[a]] -> Bool
canMult xs ys =
  let (_,n) = dimensions xs
      (b,_) = dimensions ys
  in n >= 0 && n == b

{- [1,2,3,4] !! 0 -> return index 0-}
diagonal :: [[a]] -> [a]
diagonal xss = [xs !! y |y <-[0..length(xss)], xs<-(xss!!y)]

diagonal' a = aux 0
  where
    aux i
      | i < length a = (a !! i !! i): (aux i+1)
      | otherwise = []

prop_diagonal xs =
  isSquare xs ==> diagonal xs == diagonal' xs

{-zp [1,2] [3,4] = [(1,3), (2,4)]-}

matrixAdd :: [[ Integer ]] -> [[ Integer ]] -> [[ Integer ]]
matrixAdd' a b
  |not $ canAdd a b = []
  |otherwise = [zipWith (+) (a!!i)(b!!i)|i<-[0..(fst $ dimnesions a)-1]]

matrixAdd'' a b
if canAdd a b
  then [[x+y]|(x,y)<-(zip aRow bRow)]|(aRow, bRow)<-zip a b]

matrixMult :: [[ Integer ]] -> [[ Integer ]] -> [[ Integer ]]

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [a]=[a]
mergeSort xs =
  mergeLists z u
  where
    z = mergeSort x
    u = mergeSort y
    where (x,y) = splitList xs

{-take 2 [1,2,3,4] = [1,2] ;; drop 2 [1,2,3,4] = [3,4]-}
splitList :: [a]->([a], [a])
splitList xs =
  let x = take (length xs `div` 2) xs
      y = drop (length xs `div` 2) xs
  in (x,y)


mergeLists:: [Integer]->[Integer]->[Integer]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys) =
  if (x < y)
  then x:mergeLists xs (y:ys)
  else y:mergeLists (x:xs) ys

{- zip [1,2,5,6] [3,4] = [(1,3), (2,4)]-}
adjacentPairs :: [a] -> [(a,a)]
adjacentPairs xs = zip xs $ tail xs

prop_mergeSort xs =
  and [ x<=y |(x,y)<-(adjacentPairs $ mergeSort xs)


collatz :: Integer -> [Integer]

prop_Collatz:: Integer -> Property
