module Main where

data MapObject = Tree | Blank

type TobogganMap = [[MapObject]]

type TobogganPosition = (Int, Int)

type TobogganPath = [TobogganPosition]

characterToMapObject :: Char -> MapObject
characterToMapObject character =
  case character of
    '#' -> Tree
    _ -> Blank

inputLineToMapRow :: String -> [MapObject]
inputLineToMapRow line = map characterToMapObject line

loadMap :: String -> TobogganMap
loadMap input =
  let inputLines = lines input
   in map inputLineToMapRow inputLines

positionToObject :: TobogganMap -> TobogganPosition -> MapObject
positionToObject tMap tPos =
  let (x, y) = tPos
      row = cycle (tMap !! y)
   in row !! x

onlyTrees :: MapObject -> Bool
onlyTrees object =
  case object of
    Tree -> True
    Blank -> False

treeCount :: TobogganPath -> TobogganMap -> Int
treeCount tobogganPath tobogganMap =
  let objects = map (positionToObject tobogganMap) tobogganPath
      trees = filter onlyTrees objects
   in length trees

getNextPosition :: TobogganMap -> TobogganPosition -> Maybe TobogganPosition
getNextPosition tMap currentPosition =
  let (x, y) = currentPosition
      maxY = (length tMap) - 1
      newX = x + 3
      newY = y + 1
   in if newY > maxY
        then Nothing
        else Just (x + 3, y + 1)

getTobogganPath :: TobogganMap -> TobogganPath -> TobogganPath
getTobogganPath tMap tPath =
  let currentPosition = last tPath
      maybeNextPosition = getNextPosition tMap currentPosition
   in case maybeNextPosition of
        Just nextPosition ->
          getTobogganPath tMap (tPath ++ [nextPosition])
        Nothing ->
          tPath

solve :: String -> Int
solve input =
  let tobogganMap = loadMap input
      tobogganPath = getTobogganPath tobogganMap [(0, 0)]
   in treeCount tobogganPath tobogganMap

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStr (show (solve input))
