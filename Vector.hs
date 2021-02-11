module Vector (Vector, newVector, size, capacity, resize, set, get) where

newtype Vector a = Vector [Maybe a]

newVector :: Int -> Vector a
newVector n = Vector (map (\x -> Nothing) [1..n])

size :: Vector a -> Int
size (Vector xs) = length xs

capacity :: Vector a -> Int
capacity (Vector xs) = foldl (\acc x ->case x of
                                        Just a->acc
                                        Nothing -> acc+1) 0 xs

resize :: Vector a -> Int -> Vector a
resize (Vector xs) x = Vector $ take x xs ++ map (\x -> Nothing) [1..(x-length xs)]

set :: Vector a -> a -> Int-> Maybe (Vector a)
set (Vector xs) x i
  | i < 0 || i >= length xs = Nothing
  | otherwise = let (a, (b:bs)) = splitAt i xs in
                                      case (b:bs) of
                                      [] -> Nothing
                                      xs -> Just $ Vector $ (a ++ Just x:bs)


get :: Vector a -> Int -> Maybe a
get (Vector xs) i
  | i<0 || i >= length xs = Nothing
  | otherwise = xs !! i
