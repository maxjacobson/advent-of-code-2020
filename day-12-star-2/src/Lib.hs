module Lib
  ( solve,
  )
where

data Position = Position {x :: Int, y :: Int, waypointXDistance :: Int, waypointYDistance :: Int} deriving (Show)

data Instruction
  = RotateWaypointLeft Int
  | RotateWaypointRight Int
  | MoveForward Int
  | MoveWaypointNorth Int
  | MoveWaypointEast Int
  | MoveWaypointSouth Int
  | MoveWaypointWest Int

parseInstructions :: String -> [Instruction]
parseInstructions input =
  map
    ( \line ->
        let instructionCode = head line
            amount = read (tail line) :: Int
         in case instructionCode of
              'N' -> MoveWaypointNorth amount
              'E' -> MoveWaypointEast amount
              'S' -> MoveWaypointSouth amount
              'W' -> MoveWaypointWest amount
              'F' -> MoveForward amount
              'R' -> RotateWaypointRight amount
              'L' -> RotateWaypointLeft amount
              _ -> error ("Unknown instruction: " ++ line)
    )
    (lines input)

moveTowardWaypoint :: Position -> Int -> Position
moveTowardWaypoint position times =
  if times == 0
    then position
    else
      let newPosition =
            position
              { x = (x position) + (waypointXDistance position),
                y = (y position) + (waypointYDistance position)
              }
       in moveTowardWaypoint newPosition (times - 1)

countTurns :: Int -> Int -> Int
countTurns degrees turnsCount =
  if degrees > 90
    then countTurns (degrees - 90) (turnsCount + 1)
    else
      if degrees == 90
        then turnsCount + 1
        else error "bad degrees value"

rotateLeftThisManyTimes :: Position -> Int -> Position
rotateLeftThisManyTimes position times =
  if times == 0
    then position
    else
      let newPosition =
            position
              { waypointXDistance = - (waypointYDistance position),
                waypointYDistance = waypointXDistance position
              }
       in rotateLeftThisManyTimes newPosition (times - 1)

rotateRightThisManyTimes :: Position -> Int -> Position
rotateRightThisManyTimes position times =
  if times == 0
    then position
    else
      let newPosition =
            position
              { waypointXDistance = waypointYDistance position,
                waypointYDistance = - (waypointXDistance position)
              }
       in rotateRightThisManyTimes newPosition (times - 1)

travel :: Position -> [Instruction] -> Position
travel startingPosition instructions =
  foldl
    ( \pos instruction ->
        case instruction of
          RotateWaypointLeft degrees ->
            rotateLeftThisManyTimes pos (countTurns degrees 0)
          RotateWaypointRight degrees ->
            rotateRightThisManyTimes pos (countTurns degrees 0)
          MoveForward amount ->
            moveTowardWaypoint pos amount
          MoveWaypointNorth amount -> pos {waypointYDistance = (waypointYDistance pos) + amount}
          MoveWaypointEast amount -> pos {waypointXDistance = (waypointXDistance pos) + amount}
          MoveWaypointSouth amount -> pos {waypointYDistance = (waypointYDistance pos) - amount}
          MoveWaypointWest amount -> pos {waypointXDistance = (waypointXDistance pos) - amount}
    )
    startingPosition
    instructions

manhattanDistance :: Position -> Int
manhattanDistance position =
  (abs (x position)) + (abs (y position))

solve :: String -> Int
solve input =
  let startingPosition =
        Position
          { x = 0,
            y = 0,
            waypointXDistance = 10,
            waypointYDistance = 1
          }
      instructions = parseInstructions input
      finalPosition = travel startingPosition instructions
   in manhattanDistance finalPosition
