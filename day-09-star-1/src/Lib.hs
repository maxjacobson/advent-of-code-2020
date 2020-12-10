module Lib
  ( solve,
  )
where

import Data.List (elemIndices, find)

anyTwoAddUpTo :: [Int] -> Int -> Bool
anyTwoAddUpTo set sumNumber =
  any
    ( \setNumber ->
        let otherNumber = sumNumber - setNumber
            indices = elemIndices otherNumber set
         in if setNumber == otherNumber
              then length indices == 2
              else length indices == 1
    )
    set

-- inspired by https://stackoverflow.com/a/4597898
inclusiveWindow :: Int -> Int -> [Int] -> [Int]
inclusiveWindow fromIndex toIndex numbers =
  take (toIndex - fromIndex + 1) (drop fromIndex numbers)

isInvalid :: Int -> [Int] -> Int -> Bool
isInvalid preamble numbers index =
  let set = inclusiveWindow (index - preamble) (index - 1) numbers
      number = numbers !! index
   in not (anyTwoAddUpTo set number)

firstInvalidNumber :: Int -> [Int] -> Maybe Int
firstInvalidNumber preamble numbers =
  let range = [preamble .. (length numbers - 1)]
      maybeIndex = find (isInvalid preamble numbers) range
   in case maybeIndex of
        Just index -> Just (numbers !! index)
        Nothing -> Nothing

toInt :: String -> Int
toInt str = read str :: Int

solve :: String -> Maybe Int
solve input =
  let numbers = map toInt (lines input)
      preamble = 25
   in firstInvalidNumber preamble numbers
