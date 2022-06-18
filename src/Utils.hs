module Utils (parseList) where

import Data.List (intercalate)

-- | Parse a string with space-separated integers into a list of integers.
parseList :: String -> [Integer]
parseList str = read $ "[" ++ intercalate ", " (words str) ++ "]" :: [Integer]
