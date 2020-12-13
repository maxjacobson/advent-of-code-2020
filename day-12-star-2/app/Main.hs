module Main where

import Lib (solve)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr (show (solve input))
