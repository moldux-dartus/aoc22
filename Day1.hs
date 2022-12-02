import Data.List

day1 :: IO ()
day1 = do
    cals <- lines <$> readFile "inputs/input1.txt"
    let top3 = winners $ map (sum . map read) $ splitOn (/= "") cals
    putStrLn $ "The most cals held by a single elf is: " ++ part1 top3
    putStrLn $ "The top three elves together hold: " ++ part2 top3

winners :: [Int] -> [Int]
winners = take 3 . reverse . sort

part1 :: [Int] -> String
part1 = show . head

part2 :: [Int] -> String
part2 = show . sum

splitOn :: (String -> Bool) -> [String] -> [[String]]
splitOn _ [] = [[]]
splitOn p xs = takeWhile p xs : if null remainder then [[]] else splitOn p (tail remainder)
    where remainder = dropWhile p xs
