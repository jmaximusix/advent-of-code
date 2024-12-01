module Day01 (part1, part2) where

import Data.List (sort)
import Data.Tuple.Extra (both)

part1, part2 :: [String] -> Int
part1 lines = sum $ map (\(a, b) -> abs (a - b)) $ uncurry zip $ both sort $ unzip $ map parseInput lines
part2 lines = sum $ uncurry soos $ unzip $ map parseInput lines

parseInput :: String -> (Int, Int)
parseInput line = (a, b)
  where
    [a, b] = map read $ words line

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

soos :: [Int] -> [Int] -> [Int]
soos [] _ = []
soos (a : as) bs = (count a bs * a) : soos as bs