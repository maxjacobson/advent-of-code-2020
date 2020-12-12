module Lib
  ( solve,
  )
where

import Data.Maybe (listToMaybe)

data Cell = Floor | EmptySeat | OccupiedSeat deriving (Show, Eq)

type Room = [[Cell]]

nextCellInDirectionFrom :: Int -> Int -> Room -> (Int, Int) -> Maybe (Int, Int, Cell)
nextCellInDirectionFrom x y room direction =
  let (xDirection, yDirection) = direction
      newX = x + xDirection
      newY = y + yDirection
   in if newY >= 0 && newY < length room
        then
          let row = room !! newY
           in if newX >= 0 && newX < length row
                then Just (newX, newY, row !! newX)
                else Nothing
        else Nothing

cellsInDirection :: Int -> Int -> Room -> (Int, Int) -> [Cell] -> [Cell]
cellsInDirection x y room direction list =
  case nextCellInDirectionFrom x y room direction of
    Just (newX, newY, cell) -> cellsInDirection newX newY room direction (cell : list)
    Nothing -> list

firstSeatInDirectionFrom :: Int -> Int -> Room -> (Int, Int) -> Maybe Cell
firstSeatInDirectionFrom x y room direction =
  let cells = cellsInDirection x y room direction []
      seatsInDirection = filter isSeat cells
   in listToMaybe (reverse seatsInDirection)

visibleSeats :: Int -> Int -> Room -> [Cell]
visibleSeats x y room =
  foldl
    ( \acc direction ->
        case firstSeatInDirectionFrom x y room direction of
          Just cell -> cell : acc
          Nothing -> acc
    )
    []
    [ (-1, -1), -- up and to left
      (0, -1), -- up
      (1, -1), -- up and to right
      (-1, 0), -- left
      (1, 0), -- right
      (-1, 1), -- down and to left
      (0, 1), -- down
      (1, 1) -- down and to right
    ]

isSeat :: Cell -> Bool
isSeat cell =
  case cell of
    OccupiedSeat -> True
    EmptySeat -> True
    Floor -> False

isOccupied :: Cell -> Bool
isOccupied cell =
  case cell of
    OccupiedSeat -> True
    _ -> False

stepCell :: Int -> Int -> Room -> Cell -> Cell
stepCell x y room cell =
  case cell of
    Floor -> Floor
    EmptySeat ->
      if not (any isOccupied (visibleSeats x y room))
        then OccupiedSeat
        else EmptySeat
    OccupiedSeat ->
      if length (filter isOccupied (visibleSeats x y room)) >= 5
        then EmptySeat
        else OccupiedSeat

step :: Room -> Room
step room =
  map
    ( \y ->
        let row = room !! y
         in map
              ( \x ->
                  let cell = row !! x
                   in stepCell x y room cell
              )
              [0 .. (length row - 1)]
    )
    [0 .. (length room - 1)]

stabilizeRoom :: Room -> Int -> Room
stabilizeRoom room counter =
  let newRoom = step room
   in if newRoom == room
        then newRoom
        else stabilizeRoom newRoom (counter + 1)

parseCell :: Char -> Cell
parseCell char =
  case char of
    'L' -> EmptySeat
    '#' -> OccupiedSeat
    '.' -> Floor
    _ -> error ("unknown thingy: " ++ [char])

parseRoom :: String -> Room
parseRoom input =
  map (map parseCell) (lines input)

countOccupied :: Room -> Int
countOccupied room =
  sum (map (length . filter isOccupied) room)

solve :: String -> Maybe Int
solve input =
  let room = parseRoom input
      stabilizedRoom = stabilizeRoom room 0
      count = countOccupied stabilizedRoom
   in Just count
