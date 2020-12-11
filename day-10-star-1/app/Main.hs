module Main where

import Lib

main :: IO ()
main = do
  input <- readFile "input.txt"

  case solve input of
    Just solution ->
      putStr (show solution)
    Nothing ->
      putStr "Could not find answer"
