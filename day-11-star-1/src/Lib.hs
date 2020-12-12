module Lib
  ( solve,
  )
where

data Cell = Floor | EmptySeat | OccupiedSeat deriving (Show, Eq)

type Room = [[Cell]]

adjacentSeats :: Int -> Int -> Room -> [Cell]
adjacentSeats x y room =
  foldl
    ( \acc (cx, cy) ->
        if cy >= 0 && cy < length room
          then
            let row = room !! cy
             in if cx >= 0 && cx < length row
                  then
                    let adjacentSeat = row !! cx
                     in acc ++ [adjacentSeat]
                  else acc
          else acc
    )
    []
    [(x -1, y -1), (x, y -1), (x + 1, y -1), (x -1, y), (x + 1, y), (x -1, y + 1), (x, y + 1), (x + 1, y + 1)]

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
      if not (any isOccupied (adjacentSeats x y room))
        then OccupiedSeat
        else EmptySeat
    OccupiedSeat ->
      if length (filter isOccupied (adjacentSeats x y room)) >= 4
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

stabilizeRoom :: Room -> Room
stabilizeRoom room =
  let newRoom = step room
   in if newRoom == room
        then newRoom
        else stabilizeRoom newRoom

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
      stabilizedRoom = stabilizeRoom room
      count = countOccupied stabilizedRoom
   in Just count
