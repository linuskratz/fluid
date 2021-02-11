module Exercise_3 where

import Test.QuickCheck
import Data.List
import Data.Char

{-H3.1.1-}
decomposition :: Int -> [Int]
decomposition n = decomp_help n 2

decomp_help :: Int ->Int-> [Int]
decomp_help x y =
  if x >= y
  then if x `mod`y == 0
       then [y]++ decomp_help (x`div`y) y
       else decomp_help x (y+1)
  else []

decomposition2 :: Int -> [(Int, Int)]
decomposition2 x =
  decomp_2_help (decomposition x) 1

decomp_2_help :: [Int] ->Int-> [(Int, Int)]
decomp_2_help (x:y:xs) c =
  if (x == y)
  then decomp_2_help (y:xs) (c+1)
  else [(x,c)]++decomp_2_help (y:xs) 1
decomp_2_help (y:xs) c = [(y,c)]
decomp_2_help [] c = []

{-H3.1.2-}
isPrime :: Int -> Bool
isPrime x =
  decomposition x == [x]

primes :: Int -> [Int]
primes 1 = []
primes x =
  [y |y<-[2..x], isPrime y]

{-H3.1.3-}
takes :: [Int] -> [a] -> [[a]]
takes [] _ = []
takes (n:ns) xs =
  if n > length xs
  then [[x]|x<-xs]
  else [take n xs] ++ takes ns (drop n xs)

{-H3.1.4-}
takePrimes :: [a] -> [[a]]
takePrimes [a] = [[a]]
takePrimes xs = takes (primes (length xs)) xs


{-H3.2.1-}
add :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
add [] ys = ys
add xs [] = xs
add (x:xs) (y:ys)  =
  if snd x < snd y
  then [x] ++ add xs (y:ys)
  else if snd x > snd y
    then [y]++ add (x:xs) ys
  else
    [(fst x + fst y, snd x)| (fst x + fst y) /= 0] ++ add xs ys


{-H3.2.2-}
derivative :: [(Integer,Integer)] -> [(Integer,Integer)]
derivative [] = []
derivative (x:xs)=
  [(fst(x)*snd(x), snd(x)-1) | snd x /= 0] ++ derivative xs

{-H3.2.3-}
flipNegExp :: [(Integer,Integer)] -> [(Integer,Integer)]
flipNegExp [] = []
flipNegExp (x:xs) =
  if snd x >= 0
  then (x:xs)
  else flipNegExp $ add [(fst(x), -snd(x))] xs

{-H3.3.1-}
unspell :: String -> [Int]
unspell [] = []
unspell (x:xs)=
  if (digitToInt x <10)
    then unspell_help (digitToInt x) 3 ++ unspell xs
    else unspell_help ((digitToInt x - digitToInt 'a') +10) 3 ++ unspell xs

unspell_help :: Int -> Int -> [Int]
unspell_help x y
 | y < 0 = []
 | otherwise =
    if x >= (2^y)
      then [1]++unspell_help (x-2^y) (y-1)
    else [0]++unspell_help x (y-1)


{-H3.3.2-}
index :: Int -> Int -> Int -> Int
index x y z = 4*x+2*y+1*z

{-H3.3.3-}
ritual :: [Int] -> [Int] -> [Int]
ritual [] xs = xs
ritual ys [] = []
ritual ys xs =
  ritual_help ys ([0]++xs)



ritual_help:: [Int]->[Int]->[Int]
ritual_help ys (x:y:z:xs)= [ys!!(index x y z)] ++ ritual_help ys (y:z:xs)
ritual_help ys (x:y:xs) = [ys !!(index x y 0)]
ritual_help ys (x:xs) = []

{-H3.3.4-}
simulate :: [Int] -> [Int] -> Int -> [[Int]]
simulate xs ys (-1) = []
simulate xs ys n = [ys] ++ simulate xs (ritual xs ys) (n-1)

-- Tux's open source visualisation software. GNU3 LICENSE
showPenguins [] _ = return ()
showPenguins state pc = do
  putStrLn $ [c | d <- head state, let c = if d == 0 then ' ' else pc]
  showPenguins (tail state) pc
