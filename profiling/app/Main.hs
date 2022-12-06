import Data.List
import Control.Monad (liftM2, forM_)
import Data.Bifunctor

main = do
  s <- readFile "bigboy.txt"
 -- mapM_ (putStrLn . flip solve s) [4,14]
  putStrLn $ uncurry (++) $ solve 4 14 s
  return ()
type Marker = Int

--solve :: Marker -> Marker -> String -> String
solve m1 m2 s = bimap (show . length' m2) (show . length' m1) (tails s, tails s)

length' m = (+) m . length . takeWhile (not . isMarker' . take m)

isMarker' :: String -> Bool
isMarker' chunk = go chunk []
  where go [] acc = True
        go (x:xs) acc = x `notElem` acc && go xs (x:acc)

isMarker :: String -> Bool
isMarker chunk = nub chunk == chunk

--(bimap ((+) 14 . length . takeWhile (not . isMarker' . take 14)) ((+) 4 . length . takeWhile (not . isMarker' . take 4)) (tails t, tails t))
