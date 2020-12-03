module Main where

data MapObject = Tree | Blank

type TobogganMap = [[MapObject]]

type TobogganPosition = (Int, Int)

type RoutingStrategy = (Int, Int)

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

getNextPosition :: TobogganMap -> TobogganPosition -> RoutingStrategy -> Maybe TobogganPosition
getNextPosition tMap currentPosition routingStrategy =
  let (x, y) = currentPosition
      (xDelta, yDelta) = routingStrategy
      maxY = (length tMap) - 1
      newX = x + xDelta
      newY = y + yDelta
   in if newY > maxY
        then Nothing
        else Just (newX, newY)

getTobogganPath :: TobogganMap -> RoutingStrategy -> TobogganPath -> TobogganPath
getTobogganPath tMap routingStrategy tPath =
  let currentPosition = last tPath
      maybeNextPosition = getNextPosition tMap currentPosition routingStrategy
   in case maybeNextPosition of
        Just nextPosition ->
          getTobogganPath tMap routingStrategy (tPath ++ [nextPosition])
        Nothing ->
          tPath

countTreesForRoutingStrategy :: TobogganMap -> RoutingStrategy -> Int
countTreesForRoutingStrategy tMap routingStrategy =
  let tobogganPath = getTobogganPath tMap routingStrategy [(0, 0)]
   in treeCount tobogganPath tMap

solve :: String -> Int
solve input =
  let tobogganMap = loadMap input
   in (countTreesForRoutingStrategy tobogganMap (1, 1)) * (countTreesForRoutingStrategy tobogganMap (3, 1)) * (countTreesForRoutingStrategy tobogganMap (5, 1)) * (countTreesForRoutingStrategy tobogganMap (7, 1)) * (countTreesForRoutingStrategy tobogganMap (1, 2))

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStr (show (solve input))
