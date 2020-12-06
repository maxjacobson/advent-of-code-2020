module Main where

import Lib

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStr (show (solve input))
