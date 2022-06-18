module Main where

import Bowling (scoreCard)
import Utils (parseList)

main :: IO ()
main = do
  line <- getLine
  putStrLn $ scoreCard $ parseList line
