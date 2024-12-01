module Day01 (part1, part2) where

import Data.List (partition, scanl)

part1, part2 :: [String] -> Int
part1 input = length up - length down
  where
    (up, down) = partition (== '(') (head input)
part2 input = length (takeWhile (/= (-1)) $ scanl1 (+) $ map (\x -> if x == '(' then 1 else -1) (head input)) + 1