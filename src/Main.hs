module Main where

import Bowling

main :: IO ()
main = do
  putStrLn $ scoreCard [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]
  putStrLn $ scoreCard [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 7]
  putStrLn $ scoreCard [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]

  putStrLn ""
