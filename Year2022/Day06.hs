module Day06 (part1, part2) where

import Data.List (group, sort)

part1, part2 :: [String] -> Int
part1 = parse 4 . head
part2 = parse 14 . head

parse :: Int -> String -> Int
parse n str
  | length ((group . sort) (take n str)) == n = n
  | otherwise = 1 + parse n (tail str)