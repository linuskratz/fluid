module Exercise_7 where
import Data.List

{-H7.2-}
matchesPath :: String -> String -> Bool
matchesPath [] [] = True
matchesPath (x:xs) []
  | x== '*' && xs == [] = True
  | otherwise = False
matchesPath [] _ = False
matchesPath (x:xs) (y:ys)
  | x== '?' = matchesPath xs ys
  | x== '/' && compStar ax False = nStar bx by
  | x== y = matchesPath xs ys
  | x== '*' = star xs (y:ys)
  | x== '{' = brackets (x:xs) (y:ys)
  | otherwise = False
  where (ax, bx)= span (\x -> x/='/') xs
        (ay, by)= span (\y -> y/='/') ys

compStar :: String -> Bool -> Bool
compStar [] bool = bool
compStar (x:y:ys) bool
  |"**" == [x]++[y] = compStar ys True
  | otherwise = False
compStar (x:ys) bool
  |'*' == x = bool
  | otherwise = False

nStar :: String -> String -> Bool
nStar [] [] = True
nStar _ [] = False
nStar [] _ = True
nStar xs (y:ys)
  | matchesPath xs (y:ys) = True
  | otherwise = nStar xs by
  where (ay, by)= span (\y -> y/='/') ys


star:: String -> String -> Bool
star [] [] = True
star _ [] = False
star [] (y:ys)
  | y == '/' = False
  | otherwise =  star [] ys
star xs (y:ys)
 | matchesPath xs (y:ys) = True
 | y == '/' = False
 | otherwise = star xs ys


brackets :: String -> String -> Bool
brackets (x:xs) ys
  | x== '}' = False
  | otherwise = matchesPath (ax++remainding) ys || brackets bx ys
  where (ax, bx) = span (\x ->((x/=',') && (x /= '}'))) xs
        remainding = (dropWhile (/='}') xs) \\ ['}']




{-H7.3-}
comp :: Eq b => [(Integer,(a,b))] -> [(Integer,(b,c))] -> [(Integer,(a,c))]
comp [] ys = ys
comp (i,(a,b)):xs) ys
  case of index
    Nothing -> comp xs ys
    Just val -> let (int,(_,_) = ys !! index
                in (int+i,(_,_) ++ comp xs ys
where index = elemIndex b [f,(_,(f,_))<-ys]



symcl :: Eq a => [(Integer,(a,a))] -> [(Integer,(a,a))]
symcl = undefined

{-WETT-}
trancl :: Eq a => [(Integer,(a,a))] -> [(Integer,(a,a))]
trancl = undefined
{-TTEW-}
