module Lib
  ( solve,
  )
where

import Data.List (nub, union)

type Group = [String]

groupsFrom :: [String] -> [Group] -> [Group]
groupsFrom inputLines groupsList =
  let (group, remainingInput) = break (== "") (dropWhile (== "") inputLines)
   in case group of
        [] -> groupsList
        _ -> groupsFrom remainingInput (groupsList ++ [group])

uniqueYeses :: Group -> Int
uniqueYeses group =
  let unioned = foldl union "" group
   in length (nub unioned)

solve :: String -> Int
solve input =
  let groups = groupsFrom (lines input) []
      groupCounts = map uniqueYeses groups
   in sum groupCounts
