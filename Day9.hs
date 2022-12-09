import Data.List

day9 = do
  iList <- map parseI . lines <$> readFile "inputs/input9.txt"
  print $ part1 iList
  putStrLn "Hello"

part1 :: [Instruction] -> Int
part1 = length . nub . map snd . concat . walk ((0,0),[(0,0)])

parseI :: String -> Instruction
parseI ('U':' ':int) = (U, read int :: Int )
parseI ('D':' ':int) = (D, read int :: Int )
parseI ('L':' ':int) = (L, read int :: Int )
parseI ('R':' ':int) = (R, read int :: Int )

iList :: [Instruction]
iList' = [(R, 4),
         (U, 4),
         (L, 3),
         (D, 1),
         (R, 4),
         (D, 1),
         (L, 5),
         (R, 2)]
iList = [(R, 5),
         (U, 8),
         (L, 8),
         (D, 3),
         (R, 17),
         (D, 10),
         (L, 25),
         (U, 20)]

type Instruction = (Dir, Dist)
data Dir = U | D | L | R deriving (Show, Eq, Read)
type Dist = Int

type H = (Int, Int)
type T = [(Int, Int)]
type S = (H,T)

up (x,y) = (x,y+1)
down (x,y) = (x,y-1)
left (x,y) = (x-1,y)
right (x,y) = (x+1,y)

walk :: S -> [Instruction] -> [[S]]
walk s []     = []
walk s (i:is) = exec i s : walk (last $ exec i s) is

exec :: Instruction -> S -> [S]
exec (dir, 0)    (h,t) = [(h,t)]
exec (dir, dist) (h,t) = (h,t) : exec (dir, dist-1) (move dir (h,t))

move :: Dir -> S -> S
move dir (h,t:ts) = let chase n = if t `needsToChase` n then h else t in
                 case dir of U -> (up h, chase (up h) : [])
                             D -> (down h, chase (down h):[])
                             L -> (left h, chase (left h):[])
                             R -> (right h, chase (right h):[])

--needsToChase :: T -> H -> Bool
needsToChase (x,y) (x1,y1)
  | x1 > x + 1 || x1 < x - 1 = True
  | y1 > y + 1 || y1 < y - 1 = True
  | otherwise  = False
{-
chase :: T -> H -> H -> T
chase (x,y) (x1,y1) oldHead
  | x1 >= x+2 || y1 >= y+2 = oldHead
  | x1 < y && y1 < y-1 = (x-1,y-1)
  | x1 > x+1 = (x+1,y)
  | x1 < x-1 = (x-1,y)
  | y1 > y+1 = (x,y+1)
  | y1 < y-1 = (x,y-1)
  | otherwise= (x,y)
-}
--(2,2) (3,4) -> (2,3)
--(2,2) (4,3) -> (3,3)
--(2,2) (1,0) -> (1,1)
--(2,2) (1,1) -> (1,1)
