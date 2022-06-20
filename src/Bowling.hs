module Bowling (scoreCard, finalScore) where

import Data.List (intercalate)

-- | Get a player's final score in a bowling match.
finalScore :: [Integer] -> Integer
finalScore plays = sum $ take 10 $ framesScore plays

-- | Convert the list of pins knocked over to the list of points scored in
-- each frame.
framesScore :: [Integer] -> [Integer]
framesScore (10 : plays@(next1 : next2 : _)) = (10 + next1 + next2) : framesScore plays
framesScore (play1 : play2 : plays@(next : _))
  | currSum == 10 = (10 + next) : framesScore plays
  | otherwise = currSum : framesScore plays
  where
    currSum = play1 + play2
framesScore (play : plays) = play : framesScore plays
framesScore [] = []

-- | Get a bowling match's score card representations.
-- A bowling match consists in a list for number of pins that were knocked over
-- in each of it's plays.
scoreCard :: [Integer] -> String
scoreCard plays = intercalate " | " $ parseScoreCard $ stuffStrikes 1 plays

-- | Parse a bowling match into a list of it's 10 frames' score card
-- representations.
-- A bowling match consists in a list for number of pins that were knocked over
-- in each of it's plays.
parseScoreCard :: [Integer] -> [String]
parseScoreCard [] = []
parseScoreCard [p] = []
parseScoreCard plays | length plays <= 3 = [parseFrame plays]
parseScoreCard (play1 : play2 : plays) = parseFrame [play1, play2] : parseScoreCard plays

-- | Parse a frame into it's score card representation.
-- A frame consists in a list for number of pins that were knocked over in each
-- of it's 2-3 plays.
parseFrame :: [Integer] -> String
parseFrame [10, 0] = "X _"
parseFrame [play1, play2] | play1 + play2 == 10 = show play1 ++ " /"
parseFrame [play1, play2, play3] | play1 + play2 == 10 = show play1 ++ " / " ++ swap10X play3
parseFrame plays = unwords $ map swap10X plays

-- | Insert a stuffing play (second play consisting of 0 pins) to the right of
-- each strike (10) in a game, as long as it's not in the last frame.
stuffStrikes :: Integer -> [Integer] -> [Integer]
stuffStrikes 10 plays = plays
stuffStrikes frame (10 : plays) = 10 : 0 : stuffStrikes (frame + 1) plays
stuffStrikes frame (play1 : play2 : plays) = play1 : play2 : stuffStrikes (frame + 1) plays
stuffStrikes _ _ = []

-- | Convert Integer to String and swap it for "X" if it's a 10.
swap10X :: Integer -> String
swap10X 10 = "X"
swap10X n = show n
