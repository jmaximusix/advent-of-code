module Day09 (part1, part2) where

part1, part2 :: [String] -> Int
part1 = sum . map (predict . parseInput)
part2 = sum . map (predict . reverse . parseInput)

parseInput :: String -> [Int]
parseInput = map read . words

predict :: [Int] -> Int
predict xs
  | all (== 0) diffs = last xs
  | otherwise = last xs + predict diffs
  where
    diffs = zipWith (-) (tail xs) xs