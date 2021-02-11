module Exercise_14 where

import Polynomial
import Data.List
import Test.QuickCheck

{-H14.1-}

data Tree a = Node a (Tree a) (Tree a)

nats :: Tree Integer
nats = nats_help 1
       where nats_help x = Node x (nats_help (2*x)) (nats_help (2*x+1))

(!!!) :: Tree a -> Integer -> a
(!!!) n d = let x = floor $ logBase 2 (fromIntegral d) in
             lkhelp (2^x) (x-1) n d
             where lkhelp add lvl (Node a b c) d
                     | lvl == -1 = a
                     | add + 2^lvl > d = lkhelp add (lvl-1) b d
                     | otherwise = lkhelp (add+2^lvl) (lvl-1) c d

update:: Tree Integer -> Integer -> Integer -> Tree Integer
update n d new = let x = floor $ logBase 2 (fromIntegral d) in
                 updateNats (2^x) (x-1) n d new
                 where updateNats add lvl (Node a b c) d new
                          |lvl == -1 = Node new b c
                          |add + 2^lvl > d = Node a (updateNats add (lvl-1) b d new) c
                          |otherwise = Node a b (updateNats (add+2^lvl) (lvl-1) c d new)


updateList :: [Integer] -> Tree Integer -> Integer -> Tree Integer
updateList [] n _ = n
updateList (x:xs) n distance = updateList xs n' (distance-1)
                                where n' = update n x distance


collatz:: Integer -> Tree Integer ->  [Integer] -> Integer -> Tree Integer
collatz x n list dist
  | (x == 1 || (n !!! x /= x)) = updateList list n (dist+(n!!!x)-1)
  | otherwise = if (x `mod`2 == 0) then collatz (x `div`2) n (list ++[x]) (dist+1)
                else collatz (3*x+1) n (list ++[x]) (dist+1)

output :: Tree Integer -> IO()
output n = putStrLn $ show[n!!!x| x<-[1..7]]

collatz_steps_list :: [Integer]
collatz_steps_list = 0 : help 2 nats
help x n = (n' !!!  x) : help (x+1) n'
           where n' = collatz x n [] 0


class Size a where
  size :: a -> Float

data Shape = Circle Float | Square Float deriving (Eq, Show)
data Suit = Heart | Spades | Clubs | Diamods
data Number = Ace | Two | Tree
data Card = Card Suit Number
type Deck = [Card]

instance Size Shape where
  size (Square y) = y *y

instance Size a => Size[a] where
  size (x:xs) = size x + size xs
  size [] = 0

type Map a b = a -> Maybe b

updateMap :: Eq a => (a,b) -> Map a b -> Map a b
updateMap (k,v) m = (\x -> if x == k then Just v else m x)

mapList :: Map a b -> [a]->[b]
mapList m (x:xs) = case m x of
                    Nothing -> mapList m xs
                    Just b -> b : mapList m xs

m :: Integer -> Maybe Integer
m x = if x < 10 then Just 0 else Nothing

lups' :: [Int] -> [Int]
lups' [] = []
lups' xs = let y = lups_h xs in
              if (length (xs \\ y)) > (length $ lups' y) then xs \\ y else lups' y

lups_h (x:y:xs)
  | x < y = lups_h (y:xs)
  | otherwise = (y:xs)
lups_h a = []


prop_check ::[Int] -> Property
prop_check xs= length xs < 6 ==> (length $ lups xs) == (length $ lups' xs)

ups :: Ord a => [ a] -> [a] -> [[ a ]]
ups [] ys = [ reverse ys ]
ups ( x: xs ) [] = ups xs [ x]
ups ( x: xs ) ( y : ys )
  | x > y = ups xs ( x :y: ys )
  | otherwise = reverse (y: ys ) : ups xs [ x ]

longest :: [[ a ]] -> [a ]
longest [] = []
longest ( xs : xss ) = if length xs > length ys then xs else ys
                        where ys = longest xss

lups :: Ord a => [a] -> [a ]
lups xs = longest ( ups xs [])

ioSeq :: [IO a] -> IO [a]
ioSeq [] = return ([])
ioSeq (x:xs) = do y<-x
                  z<-ioSeq xs
                  return (y:z)

ioChar :: IO [Char]
ioChar = ioSeq $ [getChar, getChar]

fillForm :: [String] -> IO [String]
fillForm [] = return []
fillForm (x:xs) = do putStrLn x
                     y <- getLine
                     ys <-fillForm xs
                     return (y:ys)
