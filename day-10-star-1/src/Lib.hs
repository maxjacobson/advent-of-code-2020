module Lib
  ( solve,
  )
where

import Data.List (sort)

orderedVoltages :: [Int] -> Maybe [Int]
orderedVoltages adapterVoltages =
    let orderedAdapterVoltages = sort adapterVoltages
        deviceAdapterVoltage = last orderedAdapterVoltages + 3
    in Just ([0] ++ orderedAdapterVoltages ++ [deviceAdapterVoltage])

voltageDifferences :: [Int] -> (Int, Int, Int)
voltageDifferences voltages =
  let range = [1 .. (length voltages - 1)]
   in foldl
        ( \acc index ->
            let from = voltages !! (index - 1)
                to = voltages !! index
                (ones, twos, threes) = acc
             in case to - from of
                  1 -> (ones + 1, twos, threes)
                  2 -> (ones, twos + 1, threes)
                  3 -> (ones, twos, threes + 1)
                  _ -> error "unexpected difference"
        )
        (0, 0, 0)
        range

solve :: String -> Maybe Int
solve input =
  case orderedVoltages
    ( map
        ( \line ->
            read line :: Int
        )
        (lines input)
    ) of
    Just voltages ->
      let (ones, _twos, threes) = voltageDifferences voltages
       in Just (ones * threes)
    Nothing -> Nothing
