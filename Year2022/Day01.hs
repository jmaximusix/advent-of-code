module Day01 (part1, part2) where

import Data.List.Extra (sortBy, splitOn)
import Data.Ord (Down (Down), comparing)

part1, part2 :: [String] -> Int
part1 = maximum . map (sum . map read) . splitOn [[]]
part2 = sum . take 3 . sortBy (comparing Down) . map (sum . map read) . splitOn [[]]