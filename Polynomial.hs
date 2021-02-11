module Polynomial where 

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

instance Show a => Show (Poly a) where
  show (Poly []) = "0"
  show (Poly [(c,e)]) = (show c)++"x"++"^{"++(show e)++"}"
  show (Poly ((c,e):ps)) = (show c)++"x"++"^{"++(show e)++"} + "++(show $ Poly ps)

-- Multiply a polynomial with a monomial c*x^e
polyShift :: (Eq a, Num a) => (a, Integer) -> Poly a -> Poly a
polyShift (0, _) _  = Poly []
polyShift (c, e) (Poly ps) = Poly [(c * c', e + e') | (c', e') <- ps]

instance (Eq a, Num a) => Num (Poly a) where
  (+) = polyAdd
  negate = polyShift (-1, 0)
  (-) = flip $ (+).negate
  (Poly []) * _ = Poly []
  (Poly ((c, e) : ps)) * (Poly qs) = polyShift (c, e) (Poly qs) + (Poly ps) * (Poly qs)
  fromInteger 0 = Poly []
  fromInteger n = Poly [(fromInteger n,0)]
  abs = undefined
  signum = undefined

-- Leading coefficient of a polynomial, i.e. the coefficient of the monomial with the highest exponent
-- Convention: lead_coeff(0) = 0
leadCoeff :: Num a => Poly a -> a
leadCoeff (Poly ps) = if null ps then 0 else fst (last ps)

-- Degree of a polynomial, i.e. the exponent of the monomial with the highest exponent
-- Convention: degree(0) = -1
degree :: Poly a -> Integer
degree (Poly ps) = if null ps then -1 else snd (last ps)

-- Quotient and remainder of p / q. If we call the quotient r and the remainder s,
-- r and s are the unique polynomials with deg(s) < deg(q) and p = qr + s.
-- Convention: p / 0 = 0, p mod 0 = p
polyDivMod :: (Fractional a, Eq a) => Poly a -> Poly a -> (Poly a, Poly a)
polyDivMod p (Poly []) = (Poly [], p)
polyDivMod p@(Poly ps) q@(Poly qs)
  | degree p < degree q = (Poly [], p)
  | otherwise = (quot + Poly [(c, e)], rem)
    where
      c = leadCoeff p / leadCoeff q
      e = degree p - degree q
      (quot, rem) = polyDivMod (p - polyShift (c, e) q) q

polyDiv :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
polyDiv ps qs = fst $ polyDivMod ps qs

polyMod :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
polyMod ps qs = snd $ polyDivMod ps qs

-- evaluates a polynomial at a given position
polyEval :: Num a => Poly a -> a -> a
polyEval (Poly ps) x = sum [c * x ^ e | (c, e) <- ps]

genSturm :: (Fractional a, Eq a) => Poly a -> Poly a -> [Poly a]
genSturm (Poly []) _ = []
genSturm p (Poly []) = [p]
genSturm p q = p : genSturm q (-(polyMod p q))

sturm :: (Fractional a, Eq a) => Poly a -> [Poly a]
sturm p = genSturm p (polyDeriv p)

-- Counts the number of left-to-right sign changes in a sequence of rationals.
-- Zeros are ignored, i.e. [1,0,1] has no sign changes and [1,0,-1] has one.
signChanges :: (Num a, Ord a) => [a] -> Int
signChanges xs = count (filter (/= 0) xs)
  where count (x : y : xs)
          | (x > 0) == (y > 0) = count (y : xs)
          | otherwise          = 1 + count (y : xs)
        count _ = 0

-- Counts the number of real roots of a given polynomial between a and b.
-- Assumption: neither a nor b are roots themselves.
countRootsBetween :: Poly Rational -> Rational -> Rational -> Int
countRootsBetween p a b = signChangesAt a - signChangesAt b
  where signChangesAt x = signChanges [polyEval p x | p <- sturm p]
