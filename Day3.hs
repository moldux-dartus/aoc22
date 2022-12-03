import Data.Maybe
import Data.List

day3 = do
  sacks <- lines <$> readFile "inputs/input3.txt"
  print $ part1 sacks
  print $ part2 sacks
  return ()

part1 :: [String] -> Int
part1 = sum . fromJust . mapM (priority . uniq . makeTuple)

part2 :: [String] -> Int
part2 = sum . fromJust . mapM (priority . shared . map nub) . clean . chunksOf3

priority :: Char -> Maybe Int
priority c = lookup c $ zip (['a'..'z'] <> ['A'..'Z']) [1..]

uniq :: Eq a => ([a], [a]) -> a
uniq = head . uncurry intersect

makeTuple :: String -> (String, String)
makeTuple s = splitAt (length s `div` 2) s

shared :: Eq a => [[a]] -> a
shared [xs,ys,zs] = let elemAll c = c `elem` (ys `intersect` zs)
                    in head $ filter elemAll xs

clean :: [[a]] -> [[a]]
clean = filter (not . null)

chunksOf3 :: [String] -> [[String]]
chunksOf3 [] = [[]]
chunksOf3 (x:y:z:rest) = [x,y,z] : chunksOf3 rest
