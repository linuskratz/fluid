module Exercise_5 where
import Test.QuickCheck
import Data.List


{- H5.3 -}
type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = ([Vertex], [Edge])

longestPath :: Graph -> Vertex -> Int
longestPath g t=
  let (v,e) = g
      start = head $ (v \\ (nub $ snd $ unzip e))
  in traverse' e start t 0


traverse' :: [Edge]->Vertex->Vertex->Int->Int
traverse' es v t acc
 |v == t = acc
 |outgoing == [] = -1
 |otherwise = maximum[(traverse' es x t (acc+1))|x<-outgoing]
  where outgoing = [(snd $ unzip es)!!x | x<-elemIndices v (fst $ unzip $ es)]

-- generates a DAG with u vertices and only one node without incoming edges
-- you can use this function to test your implementation using QuickCheck
genDag :: Int -> Gen Graph
genDag n = let v = [1..n] in
  do b <- mapM (\i -> choose (1,n-i)) [1..n-1]
     t <- mapM (\(c,i) -> vectorOf c (choose (i+1, n))) (zip b [1..n])
     let e = nub $ ([(1, i) | i<-[2..n]] ++ edges t 1 [])
     return $ (v,e)
  where
    edges [] _ acc = acc
    edges (ts:xs) i acc = edges xs (i+1) (acc ++ [(i,t) | t<-ts])
