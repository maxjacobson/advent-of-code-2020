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

unanimousYeses :: Group -> Int
unanimousYeses group =
  let firstLine = head group
      yeses =
        filter
          ( \letter ->
              all (\form -> letter `elem` form) group
          )
          firstLine
   in length yeses

solve :: String -> Int
solve input =
  let groups = groupsFrom (lines input) []
      groupCounts = map unanimousYeses groups
   in sum groupCounts
