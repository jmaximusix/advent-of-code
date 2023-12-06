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
calcRace (time, dist) = (\(a, b) -> ceiling b - floor a - 1) $ pqFormula (-fromIntegral time :: Double) (fromIntegral dist)

pqFormula :: (Floating a) => a -> a -> (a, a)
pqFormula p q = (p' - sqrt q', p' + sqrt q')
  where
    p' = -p / 2
    q' = p' * p' - q
