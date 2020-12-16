module Lib
  ( solve,
  )
where

import Data.List (elemIndices)

withNextNumber :: [Int] -> [Int]
withNextNumber nums =
  let value = last nums
      indices = elemIndices value nums
      indicesLength = length indices
   in if indicesLength == 1
        then nums ++ [0]
        else nums ++ [(indices !! (indicesLength - 1)) - (indices !! (indicesLength - 2))]

solve :: [Int] -> Int
solve startingNumbers =
  last
    ( foldl
        ( \acc _ ->
            withNextNumber acc
        )
        startingNumbers
        [1 .. (2020 - length startingNumbers)]
    )
