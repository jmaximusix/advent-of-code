module Day04 (part1, part2) where

import Data.List.Extra (splitOn)

part1, part2 :: [String] -> Int
part1 = length . filter isContained . map ranges
part2 = length . filter overlaps . map ranges

ranges :: String -> [[Int]]
ranges str = map (map read . splitOn "-") (splitOn "," str)

isContained :: [[Int]] -> Bool
isContained [[a, b], [c, d]]
  | a == c = True
  | a < c = b >= d
  | a > c = b <= d

overlaps :: [[Int]] -> Bool
overlaps [[a, b], [c, d]]
  | a == c = True
  | a < c = b >= c
  | a > c = d >= a