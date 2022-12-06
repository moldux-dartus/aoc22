import Data.List
import Control.Monad (liftM2, forM_)

day6' = do
  s <- readFile "inputs/input6.txt"
  let solve :: Int -> String -> String
      solve = (show .) . liftM2 (.) (+) (((length . takeWhile (not . ((==) =<< nub))) .) . (. tails) . map . take)
  forM_ [4,14] (putStrLn . flip solve s)

day6 = do
  s <- readFile "inputs/input6.txt"
  mapM_ (putStrLn . flip solve s) [4,14]

type Marker = Int

solve :: Marker -> String -> String
solve m = show . (+) m . length . takeWhile (not . isMarker . take m) . tails

isMarker :: String -> Bool
isMarker chunk = nub chunk == chunk
