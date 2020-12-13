module Lib
  ( solve,
  )
where

data Direction = North | East | South | West

data Position = Position {x :: Int, y :: Int, direction :: Direction}

data Instruction
  = TurnLeft Int
  | TurnRight Int
  | MoveForward Int
  | MoveNorth Int
  | MoveEast Int
  | MoveSouth Int
  | MoveWest Int

parseInstructions :: String -> [Instruction]
parseInstructions input =
  map
    ( \line ->
        let instructionCode = head line
            amount = read (tail line) :: Int
         in case instructionCode of
              'N' -> MoveNorth amount
              'E' -> MoveEast amount
              'S' -> MoveSouth amount
              'W' -> MoveWest amount
              'F' -> MoveForward amount
              'R' -> TurnRight amount
              'L' -> TurnLeft amount
              _ -> error ("Unknown instruction: " ++ line)
    )
    (lines input)

turnLeftThisManyTimes :: Int -> Direction -> Direction
turnLeftThisManyTimes times startingDirection =
  foldl
    ( \dir _ ->
        case dir of
          North -> West
          West -> South
          South -> East
          East -> North
    )
    startingDirection
    [1 .. times]

turnRightThisManyTimes :: Int -> Direction -> Direction
turnRightThisManyTimes times startingDirection =
  foldl
    ( \dir _ ->
        case dir of
          North -> East
          East -> South
          South -> West
          West -> North
    )
    startingDirection
    [1 .. times]

countTurns :: Int -> Int -> Int
countTurns degrees turnsCount =
  if degrees > 90
    then countTurns (degrees - 90) (turnsCount + 1)
    else
      if degrees == 90
        then turnsCount + 1
        else error "bad degrees value"

travel :: Position -> [Instruction] -> Position
travel startingPosition instructions =
  foldl
    ( \pos instruction ->
        case instruction of
          TurnLeft degrees ->
            let numTurns = countTurns degrees 0
                newDirection = turnLeftThisManyTimes numTurns (direction pos)
             in pos {direction = newDirection}
          TurnRight degrees ->
            let numTurns = countTurns degrees 0
                newDirection = turnRightThisManyTimes numTurns (direction pos)
             in pos {direction = newDirection}
          MoveForward amount ->
            case (direction pos) of
              North -> pos {y = (y pos) + amount}
              East -> pos {x = (x pos) + amount}
              South -> pos {y = (y pos) - amount}
              West -> pos {x = (x pos) - amount}
          MoveNorth amount -> pos {y = (y pos) + amount}
          MoveEast amount -> pos {x = (x pos) + amount}
          MoveSouth amount -> pos {y = (y pos) - amount}
          MoveWest amount -> pos {x = (x pos) - amount}
    )
    startingPosition
    instructions

manhattanDistance :: Position -> Int
manhattanDistance position =
  (abs (x position)) + (abs (y position))

solve :: String -> Int
solve input =
  let startingPosition = Position {x = 0, y = 0, direction = East}
      instructions = parseInstructions input
      finalPosition = travel startingPosition instructions
   in manhattanDistance finalPosition
