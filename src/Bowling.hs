module Bowling (scoreCard) where

import Data.List (intercalate)

-- | Get a bowling match's score card representations.
-- A bowling match consists in a list for number of pins that were knocked down
-- in each of it's plays.
scoreCard :: [Integer] -> String
scoreCard plays = intercalate " | " $ parseScoreCard $ stuffStrikes 1 plays

-- | Parse a bowling match into a list of it's 10 frames' score card
-- representations.
-- A bowling match consists in a list for number of pins that were knocked down
-- in each of it's plays.
parseScoreCard :: [Integer] -> [String]
parseScoreCard [] = []
parseScoreCard [p] = []
parseScoreCard plays | length plays <= 3 = [parseFrame plays]
parseScoreCard (play1 : play2 : plays) = parseFrame [play1, play2] : parseScoreCard plays

-- | Parse a frame into it's score card representation.
-- A frame consists in a list for number of pins that were knocked down in each
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
