module Day25 (part1, part2) where

import Data.List (partition, transpose)
import Data.List.Extra (chunksOf)
import Data.Tuple.Extra (both)
import MyLib.Utils (count, countEq)

part1 :: [String] -> Int
part1 input = count (all (< 8)) [zipWith (+) l k | l <- locks, k <- keys]
  where
    (locks, keys) = parseInput input

part2 :: [String] -> String
part2 _ = "Yay, this is the first AoC I completely finished :D"

parseInput :: [String] -> ([[Int]], [[Int]])
parseInput =
  both
    (map (map (countEq '#') . transpose))
    . partition (all (== '.') . filter (`elem` ".#") . last)
    . map (filter (not . null))
    . chunksOf 8