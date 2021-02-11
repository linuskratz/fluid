module Exercise_9 where
import Data.List


type Name = String
type Valuation = [(Name,Bool)]
data Atom = T | Var Name
  deriving (Eq, Show)
data Literal = Pos Atom | Neg Atom
  deriving (Eq, Show)
data Form = L Literal | Form :&: Form | Form :|: Form
  deriving (Eq, Show)
type Clause = [Literal]
type ConjForm = [Clause]

{-T9.3.2-}
top :: Literal
top = Pos T

bottom :: Literal
bottom = Neg T

{-T9.3.3-}
clauseToForm :: Clause -> Form
clauseToForm [] = L bottom
clauseToForm ls = foldr ((:|:).L) (L $ last ls) (init ls)

conjToForm :: ConjForm -> Form
conjToForm [] = L top
conjToForm ds = foldr ((:&:) . clauseToForm) (clauseToForm $ last ds) (init ds)

{-T9.3.4-}
substLiteral :: Valuation -> Literal -> Literal
substLiteral v l@(Pos (Var n)) = case lookup n v of
  Just b -> if b then top else bottom
  Nothing -> l
substLiteral v l@(Neg (Var n)) = case lookup n v of
  Just b -> if b then bottom else top
  Nothing -> l
substLiteral v l = l

substClause :: Valuation -> Clause -> Clause
substClause = map . substLiteral

substConj :: Valuation -> ConjForm -> ConjForm
substConj = map . substClause

{-H9.2.1-}
simpConj_clause :: Clause -> Clause
simpConj_clause c = case find (==top) c of
  Just b -> [top]
  Nothing -> filter (/= bottom) c

simpConj :: ConjForm -> ConjForm
simpConj css
    | elem [] l = [[]]
    | otherwise = l
    where l = filter (/= [top]) (map (\cs -> simpConj_clause cs) css)

{-H9.2.2-}
cnf :: Form -> ConjForm
cnf = formToConj . cnf2


cnf2 :: Form -> Form
cnf2 (L a) = L a
cnf2 (a :&: b) = (cnf2 a) :&: (cnf2 b)
cnf2 (a:|:b) = crossproduct (cnf2 a) (cnf2 b)

formToConj :: Form -> ConjForm
formToConj (a :&: b) = formToConj a ++ formToConj b
formToConj (a :|: b) = [subFormToConj a  ++ subFormToConj b]
formToConj (c) = [subFormToConj c]


subFormToConj :: Form -> Clause
subFormToConj (L x) = [x]
subFormToConj (a :|: b) = subFormToConj a ++ subFormToConj b

crossproduct :: Form -> Form -> Form
crossproduct (f1 :&: f2) f3 = (crossproduct f1 f3) :&: (crossproduct f2 f3)
crossproduct f1 (f2 :&: f3) = (crossproduct f1 f2) :&: (crossproduct f1 f3)
crossproduct f1 f2 = f1 :|: f2


{-H9.2.3-} -- DPLL implementierung siehe unten
selectV :: ConjForm -> Maybe (Name, Bool)
selectV [] = Nothing
selectV (c:cs) =
  case selectX c of
    Just a -> Just a
    Nothing -> selectV cs


selectX:: Clause ->  Maybe (Name, Bool)
selectX [] = Nothing
selectX (x:xs)= case isVar x of
                  Just a -> Just a
                  Nothing -> selectX xs

isVar:: Literal -> Maybe (Name, Bool)
isVar (Pos (Var n)) = Just (n, True)
isVar (Neg (Var n)) = Just (n, False)
isVar _ = Nothing


{-H9.2.5-}
satConj :: ConjForm -> Maybe Valuation
satConj m = dpll m []


dpll :: ConjForm -> Valuation -> Maybe Valuation
dpll m val
  | m == [] = Just val
  | m == [[]] = Nothing
  | otherwise = case findUnitClause m of
                   Just x -> dpll (simpConj(substConj [x] m)) (x:val)
                   Nothing -> case selectV m of
                              Just (a,b) -> case dpll (simpConj(substConj [(a,b)] m)) ((a,b):val) of
                                              Just x -> Just x
                                              Nothing -> dpll (simpConj(substConj [(a,not b)] m)) ((a, not b):val)
                              Nothing -> Nothing


findUnitClause :: [Clause] -> Maybe (Name, Bool)
findUnitClause [] = Nothing
findUnitClause (x:xs)
  | length x == 1 = isVar (x!!0)
  | otherwise = findUnitClause xs


{-H9.2.6-}
sat :: Form -> Maybe Valuation
sat = satConj . cnf
