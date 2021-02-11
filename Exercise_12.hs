module Exercise_12 where

import Data.List (nub, (\\))
import Data.Char

{-H12-}
isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = primeAux (n-1)
  where
    primeAux 1 = True
    primeAux i = n `mod` i /= 0 && primeAux (i-1)

-- returns all primes up to the passed number
primes :: Int -> [Int]
primes n = [i | i<-[2..n], isPrime i]


encrypt :: String -> String
encrypt s = let prime = primes (length s)
                index = [1..(length s)] \\ prime in
  [s !! (x-1) | x<-prime] ++ [s !! (x-1) | x<-index]

decrypt :: String -> String
decrypt s = decrypt_help s (primes $ length s)

decrypt_help :: String -> [Int] -> String
decrypt_help s [] = s
decrypt_help s (p:primes) = let (c:cs, ds) = splitAt (length (p:primes)) s
                                (as, bs) = splitAt (p-1) ds in
                                 decrypt_help (cs ++ as ++ c:bs) primes



main :: IO ()
main = do f <- getLine
          action <- getLine
          x <- readFile f
          if action == "encrypt"
            then writeFile (f++".encrypt") (encrypt x)
            else writeFile (f++".decrypt") (decrypt x)

data Term = App Term Term | Abs String Term | Var String

instance Show Term where
  show (Var x) = x
  show (Abs x t) = "(\\" ++ x ++ " -> " ++ show t ++ ")"
  show (App t1 (App t2 t3)) = show t1 ++ " (" ++ show t2 ++ " " ++ show t3 ++ ")"
  show (App t1 t2) = show t1 ++ " " ++ show t2

freeVars :: Term -> [String]
freeVars = freeVarsB []
  where
    freeVarsB bs (Var x)
      | x `elem` bs = []
      | otherwise = [x]
    freeVarsB bs (App t1 t2) = nub $ freeVarsB bs t1 ++ freeVarsB bs t2
    freeVarsB bs (Abs x t) = freeVarsB (x:bs) t

substVar :: String -> Term -> Term -> Term
substVar v term (Var x)
  | v == x = term
  | otherwise = Var x
substVar v term (Abs x t)
  | v == x = Abs x t
  | otherwise = Abs x (substVar v term t)
substVar v term (App t1 t2) = App (substVar v term t1) (substVar v term t2)


rename :: String -> String -> Term -> Term
rename s1 s2 (App t1 t2) = App (rename s1 s2 t1) (rename s1 s2 t2)
rename s1 s2 (Abs x t)
  | x == s1 = Abs c (rename s1 s2 (rename x c t))
  | otherwise = Abs x (rename s1 s2 t)
    where c = x ++ "a"
rename s1 s2 (Var x)
  | s1 == x = Var s2
  | otherwise = Var x

instance Eq Term where
  App t1 t2 == App t3 t4 = t1 == t3 && t2 == t4
  Abs s1 t1 == Abs s2 t2
    | s1 == s2 = t1 == t2
    | otherwise = t1 == rename s2 s1 t2
  Var x == Var y = x == y
  _ == _ = False

betaRed :: Term -> Term
betaRed (App (t1@(Abs s1 t3)) t2) = substVar s1 t2 (foldl (\t3 f1 -> rename f1 (f1++"a") t3) t1 (freeVars t2))
