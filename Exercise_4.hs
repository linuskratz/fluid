module Exercise_4 where

import Data.List



{-H4.1.1-}
isMultiSet :: Eq a => [(a,Int)] -> Bool
isMultiSet xs =
  let (c,d) = unzip (xs) in
    nub c == c && all (>0) d


{-H4.1.2-}
toList :: [(a,Int)] -> [a]
toList xs = concat [replicate n x | (x, n)<-xs]

{-H4.1.3-}
toSet :: Eq a => [(a, Int)] -> [a]
toSet xs = nub (toList xs)

{-H4.1.4-}
toMultiSet :: Eq a => [a] -> [(a, Int)]
toMultiSet xs = nub [(a,length $ filter (==a) xs) | a<-xs]

{-H4.1.5-}
multiplicity :: Eq a => a -> [(a, Int)] -> Int
multiplicity _ [] = 0
multiplicity a (x:xs) =
  if a == fst(x)
  then snd (x)
  else multiplicity a xs

{-H4.1.6-}
dotProduct :: Eq a => [(a, Int)] -> [(a, Int)] -> Int
dotProduct _ [] = 0
dotProduct [] _ = 0
dotProduct (x:xs) ys=
  let z = lookup (fst x) ys in
  case z of
    Nothing -> dotProduct xs ys
    Just y -> snd(x) * y + dotProduct xs ys

{-H4.1.7-}
euclidean :: Eq a => [(a, Int)] -> Float
euclidean xs = sqrt (fromIntegral(sum (map (^2) [snd(x)|x<-xs])))

{-H4.1.8-}
cosine :: Eq a => [(a, Int)] -> [(a, Int)] -> Float
cosine xs ys = fromIntegral (dotProduct xs ys)  / (euclidean xs * euclidean ys)

{-H4.1.9-}
vocabSimilarity :: String -> String -> Float
vocabSimilarity a b =
  let s1 = words a
      s2 = words b
  in cosine (sort(multOfString s1 s2)) (sort(multOfString s2 s1))


multOfString :: [String] -> [String]-> [(String,Int)]
multOfString s1 s2 = toMultiSet s1 ++ [(w,0) | w<-s2,  w `notElem` s1]

{-H4.1.10-}
editDistance :: Eq a => [a] -> [a] -> Int
editDistance [] [] = 0
editDistance [] (y:ys) = 1+ editDistance [] ys
editDistance (x:xs) [] = 1+ editDistance xs []
editDistance (x:z:xs) (y:b:ys) =
  minimum([1+editDistance xs ys| (x==b && z==y)]++
          [1+editDistance (z:xs) (y:b:ys)]++
          [1+editDistance (x:z:xs) (b:ys)]++
          [editDistance (z:xs) (b:ys) | x==y] ++
          [1+editDistance (z:xs) (b:ys) | x/=y])
editDistance (x:xs) (y:ys) =
  minimum([1+editDistance xs (y:ys)] ++
          [1+editDistance (x:xs) ys]++
          [editDistance xs ys | x==y] ++
          [1+editDistance xs ys | x/=y])



frequentWords = [
                  "the", "at", "there", "some", "my",
                  "of", "be", "use", "her", "than",
                  "and", "this", "an", "would", "first",
                  "a", "have", "each", "make", "water",
                  "to", "from", "which", "like", "been",
                  "in", "or", "she", "him", "call",
                  "is", "one", "do", "into", "who",
                  "you", "had", "how", "time", "oil",
                  "that", "by", "their", "has", "its",
                  "it", "word", "if", "look", "now",
                  "he", "but", "will", "two", "find",
                  "was", "not", "up", "more", "long",
                  "for", "what", "other", "write", "down",
                  "on", "all", "about", "go", "day",
                  "are", "were", "out", "see", "did",
                  "as", "we", "many", "number", "get",
                  "with", "when", "then", "no", "come",
                  "his", "your", "them", "way", "made",
                  "they", "can", "these", "could", "may",
                  "I", "said", "so", "people", "part",
                  "Alice", "Bob"]
{-H4.1.11-}
spellCorrect :: [String] -> [String] -> [[String]]
spellCorrect (d:ds) xs = [spell_help (d:ds) (editDistance d y)  y []| y <-xs]

spell_help :: [String] -> Int-> String ->[String]->[String]
spell_help [] min' y zs = zs
spell_help (d:ds) min' y zs
  |dist > min' = spell_help ds min' y zs
  |dist == min' =  spell_help ds min' y (d:zs)
  |dist < min' = spell_help ds dist y [d]
  where dist = editDistance' d y min' 0

editDistance' :: Eq a => [a] -> [a]->Int->Int-> Int
editDistance' (x:z:xs) (y:b:ys) min' acc
  | acc > min' = acc
  | otherwise = minimum([editDistance' xs ys min' (acc+1)| (x==b && z==y)] ++
                        [editDistance' (z:xs) (y:b:ys) min' (acc+1)] ++
                        [editDistance' (x:z:xs) (b:ys) min' (acc+1)]++
                        [editDistance' (z:xs) (b:ys) min' acc| x==y] ++
                        [editDistance' (z:xs) (b:ys) min' (acc+1) | x/=y])
                        where check' = editDistance' (z:xs) (b:ys) min' acc
editDistance' (x:xs) (y:ys) min' acc
  | acc > min' = acc
  |otherwise = minimum([editDistance' (xs) (y:ys) min' (acc+1)] ++
                       [editDistance' (x:xs) ys min' (acc+1)] ++
                       [editDistance' xs ys min' acc| x==y] ++
                       [editDistance' xs ys min' (acc+1)| x/=y])
editDistance' [] [] min' acc = acc
editDistance' [] (y:ys) min' acc
      | acc > min' = acc
      | otherwise = editDistance' [] ys min' (acc+1)
editDistance' (x:xs) [] min' acc
      | acc > min' = acc
      | otherwise = editDistance' xs [] min' (acc+1)
