--import Data.Tree

day7 = do
  l <- lines <$> readFile "inputs/input7.txt"
  let z = take 20 l
  mapM_ print $ parseList l

  --  print $ makeFileTree ((filter ((/=) ParseError)) $ parseList z)
--filter LS first

type Name = String
type Size = Int

data Line =  List | CD Name | Dir Name | F Size Name | ParseError  deriving (Show, Eq)

data DirectoryContents = File | Directory deriving Show

type Directory = [DirectoryContents]

parseList :: [String] -> [Line]
parseList = map (p . words)
  where p ["$","cd", dir] = CD dir
        p ["$","ls"] = ParseError --List
        p ["dir", n] = ParseError --Dir n
        p [size,name] = F (read size :: Int) name
        p a = ParseError
