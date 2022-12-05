import Data.List
import Data.Maybe
import qualified Data.Map as M

day5 = do
  t <- lines <$> readFile "inputs/input5.txt"
  let ship  = makeShip (take 8 t)
      iList = map (parseInstructions . words) $ drop 10 t
  putStrLn $ part1 ship iList
  putStrLn $ part2 ship iList

part1 :: Foldable t => Ship -> t Instruction -> Answer
part1 ship iList = map head $ M.elems $ foldl' exec ship iList

part2 :: Foldable t => Ship -> t Instruction -> Answer
part2 ship iList = map head $ M.elems $ foldl' exec' ship iList

type Answer      = String
type CrateStack  = String
type Ship        = M.Map Int CrateStack
type Instruction = (Int,Int,Int) --originally record

makeShip :: [String] -> Ship --this function came to me in a dream
makeShip = M.fromList . zip [1..] . map (unwords . words) . transpose
         . map (map snd . filter (\(a,b) -> a `elem` [2,6..35]) . zip [1..])

parseInstructions :: [String] -> Instruction --very safe function
parseInstructions s' = (n, f, t)
  where n  = read ((!!) s' 1) :: Int
        f  = read ((!!) s' 3) :: Int
        t  = read ((!!) s' 5) :: Int

exec :: Ship -> Instruction -> Ship
exec ship (moving, losingStack, gainingStack) = M.adjust (drop moving) losingStack tempShip
  where tempShip = M.adjust (poppedStack ++) gainingStack ship
        poppedStack = reverse $ take moving $ fromJust $ M.lookup losingStack ship

exec' :: Ship -> Instruction -> Ship
exec' ship (moving, losingStack, gainingStack) = M.adjust (drop moving) losingStack tempShip
  where tempShip = M.adjust (poppedStack ++) gainingStack ship
        poppedStack = {-reverse $-} take moving $ fromJust $ M.lookup losingStack ship
