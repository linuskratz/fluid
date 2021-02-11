module TUT7 where

  {--- proof by extensionality of filter
  Lemma help1: filter p (filter p xs) .=. filter xs
  Proof by induction on List xs
  Case []
  toShow: filter p (filter p []) .=. filter p []
  Proof
                              filter p (filter p [])
  (by def filter)        .=. filter p []
  QED

  Case (x:xs)
  toShow: filter p (filter p (x:xs)) .=. filter p (x:xs)
  IH1: filter p (filter p xs) .=. filter p xs

  Proof by Case Analysis on Bool p x
  Case True
  Assumption p x = True
  Proof
                                filter p (filter p (x:xs))
  (by def filter)         .=. filter p (if p x then x : filter p xs else filter p xs)
  (by Assumption)         .=. filter p (if True then x : filter p xs else filter p xs)
  (by ifTrue)             .=. filter p (x : filter p xs)
  (by filter)             .=. if p x then x : filter p (filter p xs) else filter p (filter p xs)
  (by Assumption)         .=. if True then x : filter p (filter p xs) else filter p (filter p xs)
  (by ifTrue)             .=. x : filter p (filter p xs)
  (by IH1)                .=. x: filter p xs

                              filter p (x:xs)
  (by def filter)         .=. if f x then x : filter f xs else filter f xs
  (by Assumption)         .=. if True then x : filter f xs else filter f xs
  (by ifTrue)             .=. x : filter f xs
  QED

  Case False
  Assumption p(x) = False
  Proof

                            filter p (filter p (x:xs))
(by def filter)         .=. filter p (if p x then x : filter p xs else filter p xs)
(by Assumption)         .=. filter p (if True then x : filter p xs else filter p xs)
(by ifFalse)            .=. filter p (filter p xs)
(by IH1)                .=. filter p xs

                            filter p (x:xs)
(by def filter)         .=. if f x then x : filter f xs else filter f xs
(by Assumption)         .=. if False then x : filter f xs else filter f xs
(by ifTrue)             .=. filter f xs

QED
QED
QED






  Lemma: filter p . filter p = filter p
  Proof by extensionality with xs
  toShow: filter p. filter p xs = filter p xs
  Proof
                          filter p . filter p xs
  (by def .)          .=. filter p (filter p xs)
  (by def help1)      .=. filter p xs
  QED
  QED




  foldl g b [] = b
foldl g b (x:xs) = foldl g (g b x) xs

foldr g b [] = b
foldr g b (x:xs) = g x (foldr g b xs)

declare_sym f
declare_sym a

axiom f_assoc: f x (f y z) .=. f (f x y) z
axiom f_comm_a: f x a .=. f a x



  Lemma foldl_help: foldl f (f a y) xs .=. f y (foldl f a xs)
  Proof by induction on List xs
  Case []
  To show: foldl f (f a y) [] .=. f y (foldl f a [])
  Proof
                                  foldl f (f a y) []
  (by def foldl)             .=.  f a y

                                  f y (foldl f a [])
  (by def foldl)             .=.  f y a
  (by f_comm_a)              .=.  f a y
  QED
  Case (x:xs)
  To show:  foldl f (f a y) (x:xs) .=. f y (foldl f a (x:xs))
  IH1: foldl f (f a y) xs .=. f y (foldl f a xs)
  Proof
                                  foldl f (f a y) (x:xs)
  (by def foldl)              .=. foldl f (f (f a y) x) xs

                                  f y (foldl f a (x:xs))
  (by def foldl)              .=. f y (foldl f (f a x) xs)
  (by IH1)                    .=. f y (f x (foldl f a xs))
  (by f_assoc)                .=. f (f y x) (foldl f a xs)
  (by IH1)                    .=. foldl f (f a (f y x)) xs
  (by f_assoc)                .=. foldl f (f (f a y) x) xs

  QED
  QED


  Lemma fold: foldl f a xs .=. foldr f a xs
  Proof by induction on List xs
  Case []
  To show: foldl f a [] .=. foldr f a []
  Proof
                        foldl f a []
  (by def foldl)    .=. a
  (by def foldr)    .=. foldr f a []

  QED
  Case (x:xs)
  To show: foldl f a (x:xs) .=. foldr f a (x:xs)
  IH1: foldl f a xs .=. foldr f a xs
  Proof
                              foldl f a (x:xs)
  (by def foldl)           .=.  foldl f (f a x) xs
  (by Lemma foldl_help)    .=.  f x (foldl f a xs)

                          foldr f a (x:xs)
  (by def foldr)     .=. f x (foldr f a xs)
  (by IH1)           .=. f x (foldl f a xs)

  QED
QED





axiom f_assoc: f x (f y z) .=. f (f x y) z
axiom f_comm_a: f x a .=. f a x
foldl g b [] = b
foldl g b (x:xs) = foldl g (g b x) xs


(by def foldr)              .=. f x (foldr f (f a y) xs)
(by IH1)                    .=. f x (f y (foldr f a xs))
(by f_assoc)                .=. f (f x y) (foldr f a xs)
(by IH1)                    .=. foldr f (f a (f x y)) xs
(by f_assoc)                .=. foldr f (f (f a x) y) xs
(by f_comm_a)               .=. foldr f (f (f x a) y) xs
(by IH1)                    .=. f y (foldr f (f x a) xs)
(by f_comm_a)               .=. f y (foldr f (f a x) xs)
(by IH1)                    .=. f y (f x (foldr f a xs))
(by def foldr)              .=. f y (foldr f a (x:xs))

                                  f y (foldr f a (x:xs))
  (by def foldr)              .=. f y (f x (foldr f a xs))
  (by f_assoc)                .=. f (f y x) (foldr f a xs)
  (by IH1)                    .=. foldr f (f a (f y x)) xs
  (by f_assoc)                .=. foldr f (f (f a y) x) xs
  (by f_comm_a)               .=. foldr f (f (f y a) x) xs
  (by f_assoc)                .=. foldr f (f y (f a x)) xs
  (by f_comm_a)               .=. foldr f (f y (f x a)) xs
  (by f_assoc)                .=. foldr f (f (f y x) a) xs
  -}




  iterWhile :: (a -> a -> Bool) -> (a -> a) -> a -> a
  iterWhile test f x
    | test x (f x) == False = x
    | otherwise iterWhile test f (f x)

  fixpoint :: Eq a => (a -> a) -> a -> a
  fixpoint = iterWhile (\xy-> y /= x)

  mySqrt:: Double -> Double
  mySqrt n = fixpoint (\x -> (x + n/x)/2) 1

  findSup :: Ord a => (a -> a) -> a -> a -> a
  findSup f m x = iterWhile (\_y -> y <= m) f x

  mapState :: (s -> a -> (b,s)) -> s -> [a] -> ([b], s)


  f :: String -> Char -> (Char, String)

  --(\curr acc -> curr. acc)
  compose :: [(a -> a)] -> a -> a
  compose (f:fs) = foldr (.) id fs

  fib :: Integer -> Integer
  fib n = fst $ foldr (\_ (a,b)-> (b, a+b)) (0,1) [1..n]

  length':: [a] -> Integer
  length' xs = foldr (+1) 0 xs

  reverse':: [a] -> [a]
  reverse' xs = foldr (\curr acc -> acc ++ [curr]) [] xs

  map' :: (a -> b) -> [a] -> [b]
  map' f = foldr (\curr acc ->[f curr] ++ acc) [] xs

  inits' :: [a] -> [[a]]
  inits' xs = foldr (\curr acc -> take curr (xs:acc)) [] [0..length xs]
