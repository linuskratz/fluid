module Exercise_10 where
import Data.List
import Test.QuickCheck

{-H10.1-}
data Player = V | H -- vertical or horizontal player
  deriving (Eq,Show)
data Field = P Player | E -- a field is occupied by a player or empty
  deriving (Eq,Show)
type Row = [Field]
type Column = [Field]
type Board = [Row] -- we assume that boards are squares and encode a board row by row
data Game = Game Board Player -- stores the currenty board and player
  deriving (Eq,Show)

-- get a given row of a board
row :: Board -> Int -> Row
row = (!!)

-- get a given column of a board
column :: Board -> Int -> Column
column = row . transpose

-- width of a board
width :: Board -> Int
width [] = 0
width (x:xs) = length x

-- height of a board
height :: Board -> Int
height = length

{-H10.1.1-}
prettyShowBoard :: Board -> String
prettyShowBoard [] = ""
prettyShowBoard (x:xs) = (foldr fieldToString "" x) ++ "\n" ++ prettyShowBoard xs

fieldToString :: Field -> String -> String
fieldToString (E) x= '+' : x
fieldToString (P V) x= 'V' : x
fieldToString (P H) x= 'H' : x

{-H10.1.2-}
-- position on a board (row, column)
-- (0,0) corresponds to the top left corner
type Pos = (Int, Int)

isValidMove :: Game -> Pos -> Bool
isValidMove (Game b H) (r, c)
  | (r >= 0) && (c >= 0) && (width b > (c+1)) && (height b > r) = (b !! r !! c) == E && (b !! r !! (c+1)) == E
  | otherwise = False
isValidMove (Game b V) (r, c)
  | (r >= 0) && (c >= 0) && (height b > (r+1)) && (width b > c) = (b !! r !! c) == E && (b !! (r+1) !! c) == E
  | otherwise = False

{-H10.1.3-}
canMove :: Game -> Bool
canMove (Game [] _) = False
canMove (Game (b:bs) H)
  | canMove_h b False = True
  | otherwise = canMove (Game bs H)
canMove (Game b V) = or [canMove_h (column b x) False | x<-[0..(height b-1)]]

canMove_h :: [Field] -> Bool -> Bool
canMove_h [] _ = False
canMove_h (c:cs) True
  | c == E = True
  | otherwise = canMove_h cs False
canMove_h (c:cs) False
    | c == E = canMove_h cs True
    | otherwise = canMove_h cs False


{-H10.1.4-}
updateBoard :: Board -> Pos -> Field -> Board
updateBoard b (r,c) f = let (xs, y:ys) = splitAt r b in
                          xs ++ replace y c f : ys

replace :: Column -> Int -> Field -> Column
replace b c f = let (xs, _:ys) = splitAt c b in
                  xs ++ f : ys

{-H10.1.5-}
playMove :: Game -> Pos -> Game
playMove (Game b V) (r,c) = Game (updateBoard (updateBoard b (r,c) (P V)) (r+1,c) (P V)) H
playMove (Game b H) (r,c) = Game (updateBoard (updateBoard b (r,c) (P H)) (r,c+1) (P H)) V

{-H10.1.6-}
-- the first paramter of a strategy is an infite list of
-- random values between (0,1) (in case you wanna go wild with
-- probabilistic methods)
type Strategy = [Double] -> Game -> Pos

christmasAI :: Strategy -- receives a game and plays a move for the next player
christmasAI _ g@(Game b V) = case findRows g (1,0) of
                              Just x -> x
                              Nothing -> (0,0)
christmasAI _ g@(Game b H) = case findRows g (0,1) of
                              Just x -> x
                              Nothing -> (0,0)

findRows :: Game -> Pos -> Maybe Pos
findRows g@(Game b V) (r,c)
 | isValidMove g (r,c) = Just (r, c)
 | height b <= r = findRows g (0, (c+2))
 | width b <= c = findspot g (0,0)
 | otherwise = findRows g ((r+1), c)
findRows g@(Game b H) (r,c)
 | isValidMove g (r,c) = Just (r, c)
 | width b <= c = findRows g ((r+2), 0)
 | height b <= r= findspot g (0,0)
 | otherwise = findRows g (r, (c+1))

findspot:: Game -> Pos -> Maybe Pos
findspot g@(Game b _) (r,c)
 | isValidMove g (r,c) = Just (r, c)
 | c >= width b = findspot g ((r+1), 0)
 | r >= height b = Nothing
 | otherwise = findspot g (r,(c+1))


{-H10.1.7-}
play :: [[Double]] -> Int -> Strategy -> Strategy -> ([Board],Player)
play rss x sv sh =
  let board = [map (\x -> E) [1..x]| _ <-[1..x]] in
      choosePlayer rss V sv sh [board]

choosePlayer :: [[Double]] -> Player -> Strategy -> Strategy -> [Board] -> ([Board],Player)
choosePlayer (d:ds) H sv sh (b:boards)
  | canMove (Game b H) && isValidMove (Game b H) move = choosePlayer ds V sv sh (newb:b:boards)
  | otherwise = (tail $ reverse (b:boards), V)
      where move = playGame d H sh b
            (Game newb _) = (playMove (Game b H) move)
choosePlayer (d:ds) V sv sh (b:boards)
  | canMove (Game b V) && isValidMove (Game b V) move = choosePlayer ds H sv sh (newb:b:boards)
  | otherwise = (tail $ reverse (b:boards), H)
      where move = playGame d V sv b
            (Game newb _) = (playMove (Game b V) move)


playGame :: [Double] -> Player -> Strategy -> Board -> Pos
playGame rs p s b = s rs (Game b p)



-- generates infinite list of values between (0,1)
genRandomZeroOne :: Gen [Double]
genRandomZeroOne = mapM (const $ choose (0::Double,1)) [1..]

-- plays a game and prints it to the console
playAndPrint :: Int -> Strategy -> Strategy -> IO ()
playAndPrint dim sh sv = do
  rss <- generate $ mapM (const $ genRandomZeroOne) [1..]
  let (bs, w) = play rss dim sh sv
  putStr $ (unlines $ map prettyShowBoard bs) ++ "\nWinner: " ++ show w ++ "\n"
