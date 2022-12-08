import Data.Maybe
import Data.Char
import Data.List

day8 = do
  t <- lines <$> readFile "inputs/input8.txt"
  print $ part1 (grid t)
  print $ part2 (grid t)

type Table = [String]
type Coords = (Int, Int)
type Grid = [Tree]
type Tree = (Coords, Int)

up (x,y) = (x,y-1)
down (x,y) = (x,y+1)
left (x,y) = (x-1,y)
right (x,y) = (x+1,y)

grid :: Table -> Grid
grid table = zip (coords table) (map digitToInt $ concat table)
  where coords side = [ (y,x) | x <- [1..(length side)], y <- [1..(length side)] ]

part1 :: Grid -> Int
part1 grid = length
           $ filter (\tree -> visibleFrom up grid tree
                           || visibleFrom left grid tree
                           || visibleFrom right grid tree
                           || visibleFrom down grid tree) grid

part2 :: Grid -> Int
part2 grid = maximum $
             map (\t -> distanceFrom left grid t
                      * distanceFrom right grid t
                      * distanceFrom up grid t
                      * distanceFrom down grid t) grid

distanceFrom :: (Coords -> Coords) -> Grid -> Tree -> Int
distanceFrom dir grid ((x,y), t)
  | isNothing lk = 0
  | Just t <= lk = 1
  | otherwise = 1 + distanceFrom dir grid (dir (x,y), t)
  where lk = lookup (dir (x,y)) grid

visibleFrom :: (Coords -> Coords) -> Grid -> Tree -> Bool
visibleFrom dir grid ((x,y), t) = Just t > maximum (query $ dir (x,y))
  where query (x,y) = let lk = lookup (x,y) grid
                      in  lk : if isNothing (lookup (dir (x,y)) grid) then [Nothing]
                               else query $ dir (x,y)

t = [ 30373
    , 25512
    , 65332
    , 33549
    , 35390]
