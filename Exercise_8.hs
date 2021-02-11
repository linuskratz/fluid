module Exercise_8 where

import Data.List
import Test.QuickCheck
import Data.Bits

{-H.8.1-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

showGraphviz :: Show a => Tree a -> String
showGraphviz t =
  let
    show1 x Leaf = []
    show1 x (Node _ y _) = [show x ++ " -- " ++ show y ++ ";"]

    show' Leaf = []
    show' (Node l x r) =
      show1 x l ++ show1 x r ++ show' l ++ show' r
  in
    unlines $ ["graph T {"] ++ show' t ++ ["}"]

{-H.8.1.1-}
symmetric :: Eq a => Tree a -> Bool
symmetric t = switch t == t

switch :: Eq a => Tree a -> Tree a
switch (Leaf) = Leaf
switch (Node x y z) = Node (switch z) y (switch x)

{-H.8.1.2-}
isBST :: Ord a => Tree a -> Bool
isBST t = preorder t == (sort $ preorder t)


foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node a b c) = foldTree f (f b (foldTree f acc c)) a

preorder :: Tree a -> [a]
preorder t = foldTree (:) [] t



val :: Ord a => Tree a -> Maybe a
val (Leaf) = Nothing
val (Node x y z) = Just y

isAlmostComplete :: Tree a -> Bool
isAlmostComplete t = height t min == (height t max) -1 ||  height t min == height t max

height:: Tree a -> (Int -> Int -> Int) -> Int
height (Leaf) f = 0
height (Node a b c) f = 1 + f (height a f) (height c f)


buildBST :: Ord a => [a] -> Tree a
buildBST [] = Leaf
buildBST xs =
  let  (a, b) = splitAt (length xs `div` 2) xs
       (y:ys) = b
  in (Node (buildBST a) (xs !! ((length xs) `div` 2)) (buildBST (ys)))


prop_buildBST :: [Int] -> Bool
prop_buildBST xs = (isAlmostComplete $ buildBST $ sort xs) == True

rangeSubtree :: Ord a => Tree a -> (a, a) -> Tree a
rangeSubtree t (a1, a2)= buildBST $ sort $ extractList t (a1,a2)
                          where extractList Leaf _ = []
                                extractList (Node x y z) (a1,a2)
                                  | y >= a1 && y <= a2 = [y] ++ extractList x (a1,a2) ++ extractList z (a1,a2)
                                  | otherwise = extractList x (a1,a2) ++ extractList z (a1,a2)


{-H8.2-}
shoefa :: (Num a, Ord a) => [a] -> Int
shoefa xs = shoefa_help (filter (/=0) xs)
        where shoefa_help (x:y:xs)
                | signum x /= signum y = 1 + shoefa_help (y:xs)
                | otherwise = shoefa_help (y:xs)
              shoefa_help _ = 0

shoefa' = group . map signum . filter (/=0)

x = (++).reverse

f :: a -> (b -> c)
f x y = (f x) y


add1 :: Int -> (Int -> Int)
add1 x y = x + y

head' x y = undefined

{-H8.3-}
data Poly a = Poly [(a, Integer)]
  deriving (Eq)

-- takes the derivative of a polynomial
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Poly ps) = Poly [(fromInteger e * c, e - 1) | (c, e) <- ps, e /= 0]

-- addition of two polynomials
polyAdd :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
polyAdd (Poly []) q = q
polyAdd p (Poly []) = p
polyAdd p@(Poly ((c1, e1) : ps)) q@(Poly ((c2, e2) : qs))
  | e1 < e2      = let Poly rs = polyAdd (Poly ps) q in
                   Poly ((c1, e1) : rs)
  | e1 > e2      = let Poly rs = polyAdd p (Poly qs) in
                   Poly ((c2, e2) : rs)
  | c1 + c2 == 0 = polyAdd (Poly ps) (Poly qs)
  | otherwise    = let Poly rs = polyAdd (Poly ps) (Poly qs) in
                   Poly ((c1 + c2, e1) : rs)

{-H8.3.1-}
instance Show a => Show (Poly a) where
  show (Poly []) = "0"
  show (Poly p)= show_help (Poly p) True
      where show_help (Poly []) _ = ""
            show_help (Poly ((a,b):px)) True = show a ++ "x^{" ++show b ++"}" ++ show_help(Poly px) False
            show_help (Poly ((a,b):px)) False = " + " ++ show a ++ "x^{" ++show b ++"}" ++ show_help(Poly px) False

{-H8.3.2-}
polyShift :: (Eq a, Num a) => (a, Integer) -> Poly a -> Poly a
polyShift (a,b) (Poly []) = Poly []
polyShift (a,b) (Poly p) = Poly [(x*a, b+y) | (x,y)<-p]


{-H8.3.3-}
instance (Eq a, Num a) => Num (Poly a) where
  (Poly x) + (Poly y) = reduce_zero $ Poly (polyAdd (sort_poly x) (sort_poly y))
          where
            polyAdd xs [] = xs
            polyAdd [] ys = ys
            polyAdd ((a,b):xs) ((x,y):ys)
                | b == y = (a+x, b):polyAdd xs ys
                | b < y = (a,b):polyAdd xs ((x,y):ys)
                | otherwise = (x,y):polyAdd ((a,b):xs) ys
  x - y = reduce_zero $ polyAdd x (negate y)
  x * y = reduce_zero $ polyMul x y
        where polyMul (Poly []) y = Poly []
              polyMul (Poly ((a,b):xs)) y = polyShift (a,b) y + polyMul (Poly xs) y
  fromInteger x
      |x == 0 = Poly []
      |otherwise = Poly [(fromInteger x, 0)]
  negate x = reduce_zero $ polyShift (-1,0) x
  abs x = undefined
  signum x = undefined



f1 = replicate (\x -> x+2) 5

reduce_zero :: (Eq a, Num a) => Poly a -> Poly a
reduce_zero (Poly []) = Poly []
reduce_zero (Poly p) = Poly (filter (\(a,b) -> (a /= 0)) p)


sort_poly :: (Ord b) => [(a,b)] -> [(a,b)]
sort_poly [] = []
sort_poly xs = sortBy (\(_,a) (_,b) -> compare a b) xs

{-H8.3.4-}
leadCoeff :: Num a => Poly a -> a
leadCoeff (Poly []) = 0
leadCoeff (Poly p) =  fst $ last $ sort_poly p


degree :: Poly a -> Integer
degree (Poly []) = -1
degree (Poly p) = snd $ last $ sort_poly p
