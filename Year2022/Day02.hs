module Day02 (part1, part2) where

import Data.Char (ord)

part1, part2 :: [String] -> Int
part1 = sum . map (score1 . nums)
part2 = sum . map (score2 . nums)

nums :: String -> (Int, Int)
nums [x, _, y] = (ord x - 65, ord y - 88)

score1 :: (Int, Int) -> Int
score1 (c1, c2) = ((c2 - c1 + 1) `mod` 3) * 3 + c2 + 1

score2 :: (Int, Int) -> Int
score2 (c1, c2) = (c1 + c2 - 1) `mod` 3 + 1 + c2 * 3
