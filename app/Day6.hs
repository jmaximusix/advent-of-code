module Day6 (part1, part2) where

import Data.Tuple.Extra (both)

part1, part2 :: [String] -> Int
part1 = product . map calcRace . parseInput
part2 = calcRace . both (read . concatMap show) . unzip . parseInput

parseInput :: [String] -> [(Int, Int)]
parseInput input = zip time dist
  where
    [time, dist] = map (map read . tail . words) input

calcRace :: (Int, Int) -> Int
calcRace (time, dist) = length $ filter (> dist) $ map (\t -> t * (time - t)) [0 .. time]
