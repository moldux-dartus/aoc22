data Move = Rock | Paper | Scissors deriving (Show, Eq, Enum)

data Outcome = Win | Draw | Lose deriving (Show, Eq)

day2 :: IO ()
day2 = do
  moveList <- lines <$> readFile "inputs/input2.txt"
  print $ part1 moveList
  print $ part2 moveList
  return ()

part1 :: [String] -> Int
part1 moveList = sum $ map calcScore parsedList
  where parsedList = map (\[x,_,y] -> (parseM x, parseM y)) moveList

part2 :: [String] -> Int
part2 moveList = sum $ map calcScore parsedList
  where parsedList = map parse moveList
        parse [x,_,y] = (parseM x, deduceMove (parseO y) (parseM x))

calcScore :: (Move, Move) -> Int
calcScore (theirs, mine)
  | mine == winAgainst theirs  = 0 + mScore mine
  | mine == loseAgainst theirs = 6 + mScore mine
  | otherwise                  = 3 + mScore mine

winAgainst :: Move -> Move
winAgainst m | m == Rock = Scissors | otherwise = pred m

loseAgainst :: Move -> Move
loseAgainst m | m == Scissors = Rock | otherwise = succ m

mScore m = 1 + fromEnum m

deduceMove :: Outcome -> Move -> Move
deduceMove o = case o of Win  -> winAgainst
                         Draw -> id
                         Lose -> loseAgainst

parseM :: Char -> Move
parseM c | c `elem` "AX" = Rock
         | c `elem` "BY" = Paper
         | otherwise     = Scissors

parseO :: Char -> Outcome
parseO c | c == 'X'  = Win
         | c == 'Y'  = Draw
         | otherwise = Lose
