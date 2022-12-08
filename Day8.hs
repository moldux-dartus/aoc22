import Data.Maybe
import Data.Char
import Data.List

day8 = do
  t <- lines <$> readFile "inputs/input8.txt"
  print $ part1 (grid t) -- (map (take 15) (take 15 t))
  return ()

part1 :: Grid -> Int
part1 grid = length
                $ filter (\tree -> visibleFromUp grid tree
                                || visibleFromLeft grid tree
                                || visibleFromRight grid tree
                                || visibleFromDown grid tree) grid

type Table = [String]
type Coords = (Int, Int)
type Grid = [Tree]
type Tree = (Coords, Int)

grid :: Table -> Grid
grid table = zip (coords table) (map digitToInt $ concat table)
  where coords side = [ (y,x) | x <- [1..(length side)], y <- [1..(length side)] ]


visibleFromLeft :: Grid -> Tree -> Bool
visibleFromLeft grid ((x,y), t) = Just t > maximum (queryLeft (x-1,y))
  where queryLeft (x,y) = let lk = lookup (x,y) grid
                          in lk : if (lk > Just t) || isNothing (lookup (x-1,y) grid)
                                  then [Nothing]
                                  else queryLeft (x-1,y)

visibleFromRight :: Grid -> Tree -> Bool
visibleFromRight grid ((x,y), t) = Just t > maximum (queryRight (x+1,y))
  where queryRight (x,y) = let lk = lookup (x,y) grid
                           in lk : if isNothing (lookup (x+1,y) grid)
                                   then [Nothing]
                                   else queryRight (x+1,y)

visibleFromUp :: Grid -> Tree -> Bool
visibleFromUp grid ((x,y), t) = Just t > maximum (queryUp (x,y-1))
  where queryUp (x,y) = let lk = lookup (x,y) grid
                        in lk : if isNothing (lookup (x,y-1) grid)
                                then [Nothing]
                                else queryUp (x,y-1)

visibleFromDown :: Grid -> Tree -> Bool
visibleFromDown grid ((x,y), t) = Just t > maximum (queryUp (x,y+1))
  where queryUp (x,y) = let lk = lookup (x,y) grid
                        in lk : if isNothing (lookup (x,y+1) grid)
                                then [Nothing]
                                else queryUp (x,y+1)
