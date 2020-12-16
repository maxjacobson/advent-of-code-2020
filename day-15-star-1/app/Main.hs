module Main where

import Lib (solve)

main :: IO ()
main = do
  putStr (show (solve [1,0,16,5,17,4]))
