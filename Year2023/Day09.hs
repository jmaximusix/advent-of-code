module Day09 (part1, part2) where

import Data.Function (fix)

part1, part2 :: [String] -> Int
part1 = sum . map (fix (\p l@(h : t) -> let d = zipWith (-) t l in if all (== 0) d then h else h - p d) . reverse . map read . words)
part2 = sum . map (fix (\p l@(h : t) -> let d = zipWith (-) t l in if all (== 0) d then h else h - p d) . map read . words)

-- Pre one-liner version (same as above but better readability)
-- For this one the input of p2 is reversed instead of p1

-- part1 = sum . map (predict . parseInput)
-- part2 = sum . map (predict . reverse . parseInput)

-- parseInput :: String -> [Int]
-- parseInput = map read . words

-- predict :: [Int] -> Int
-- predict xs
--   | all (== 0) diffs = last xs
--   | otherwise = last xs + predict diffs
--   where
--     diffs = zipWith (-) (tail xs) xs
