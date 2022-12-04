import Data.Char
import Data.List
import Data.Bifunctor

day4 = do
  assignments <- lines <$> readFile "inputs/input4.txt"
  print $ part1 assignments
  print $ part2 assignments
  return ()

part1 :: [String] -> Int
part1 as = length $ filter ((==True) . doesContain . pairify) as

part2 :: [String] -> Int
part2 as = length $ filter ((==True) . doesOverlap . pairify) as

pairify :: String -> ([Int], [Int])
pairify a = bimap makeRange makeRange (makePair a)
--HLINT MY BELOVED

makePair :: String -> (String,String)
makePair s = tail <$> break (==',') s

makeRange :: String -> [Int]
makeRange r = [low .. high]
  where low  = read (takeWhile isDigit r) :: Int
        high = read (tail $ dropWhile isDigit r) :: Int

doesContain :: ([Int],[Int]) -> Bool
doesContain (as,bs)
  | length as < length bs = isSubsequenceOf as bs
  | otherwise             = isSubsequenceOf bs as

doesOverlap :: ([Int],[Int]) -> Bool
doesOverlap (as,bs) = (not . null) (as `intersect` bs)
