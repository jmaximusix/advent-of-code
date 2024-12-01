module Day01 (part1, part2) where

import Data.List (sort)
import Data.Tuple.Extra (both)

part1, part2 :: [String] -> Int
part1 = sum . map (abs . uncurry (-)) . uncurry zip . both sort . parseInput
part2 = sum . (\(l, r) -> map (\l' -> length (filter (== l') r) * l') l) . parseInput

parseInput :: [String] -> ([Int], [Int])
parseInput = unzip . map ((\[a, b] -> (a, b)) . map read . words)