module Lib
    ( printSolution
    ) where


import Data.List

existsIn :: [Int] -> (Int, Int) -> Bool
existsIn numbers hypotheticalPair =
  let
    (actualNumber, desiredNumber) = hypotheticalPair
  in
  elem desiredNumber numbers

withOtherNumber :: Int -> (Int, Int)
withOtherNumber num = (num, 2020 - num)

findPair :: [Int] -> Maybe (Int, Int)
findPair numbers =
  let
    allPossibleHappyAnswers = map withOtherNumber numbers
  in
    find (existsIn numbers) allPossibleHappyAnswers

convertStringToInt str = read str :: Int

solve :: String -> Maybe Int
solve input =
  let
    numbers = (map convertStringToInt (lines input))

    maybePair = findPair numbers

  in
    case maybePair of
      Just (num, other_num) -> Just (num * other_num)
      Nothing               -> Nothing

printSolution :: IO ()
printSolution = do {
  input <- readFile "input.txt";

  putStr (show (solve input))
}
