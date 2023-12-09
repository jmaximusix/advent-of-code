module Day09 (part1, part2) where

import Advent (Part (Part1, Part2))

part1, part2 :: [String] -> Int
part1 = sum . map (predict Part1 . parseInput)
part2 = sum . map (predict Part2 . parseInput)

parseInput :: String -> [Int]
parseInput = map read . words

predict :: Part -> [Int] -> Int
predict part xs
  | all (== 0) diffs = headlast xs
  | otherwise = headlast xs `plusminus` predict part diffs
  where
    diffs = zipWith (-) (tail xs) xs
    headlast = if part == Part1 then last else head
    plusminus = if part == Part1 then (+) else (-)