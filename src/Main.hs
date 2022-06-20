module Main where

import Bowling
import Utils (parseList)

main :: IO ()
main = do
  line <- getLine
  let pins = parseList line
  putStrLn $ Bowling.scoreCard pins ++ " | " ++ show (Bowling.finalScore pins)
