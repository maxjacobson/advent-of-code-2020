module Lib
  ( solve,
  )
where

import Data.List (find, inits)
import Data.Maybe (fromMaybe, isJust)

findBlockAt :: [Int] -> Int -> Int -> Maybe [Int]
findBlockAt numbers goal startingIndex =
  find
    ( \block ->
        length block >= 2 && sum block == goal
    )
    (inits (drop startingIndex numbers))

findBlock :: [Int] -> Int -> Maybe [Int]
findBlock numbers sum =
  let range = [0 .. length numbers - 1]
      findBlockAtIndex = findBlockAt numbers sum
      maybeBlocks = map findBlockAtIndex range
   in fromMaybe Nothing (find isJust maybeBlocks)

solve :: String -> Maybe Int
solve input =
  let numbers = map (\line -> read line :: Int) (lines input)
      maybeBlock = findBlock numbers 21806024
   in case maybeBlock of
        Just block ->
          Just (minimum block + maximum block)
        Nothing -> Nothing
