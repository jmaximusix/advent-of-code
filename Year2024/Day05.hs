module Day05 (part1, part2) where

import Combinatorics (tuples)
import Data.List (partition)
import Data.List.Extra (replace, splitOn)
import MyLib (tup2)

type Rule = (Int, Int)

type Update = [Int]

part1, part2 :: [String] -> Int
part1 = sum . map middleItem . snd . uncurry partitionUpdates . parseInput
part2 input = sum . map (middleItem . reorder rules) . fst $ partitionUpdates rules updates
  where
    (rules, updates) = parseInput input

parseInput :: [String] -> ([Rule], [Update])
parseInput input = (rules, map (map read . splitOn ",") b)
  where
    rules = map (tup2 . map read . splitOn "|") a
    [a, b] = splitOn [""] input

partitionUpdates :: [Rule] -> [Update] -> ([Update], [Update])
partitionUpdates rules = partition (any (violatesRules rules . tup2) . tuples 2)

middleItem :: [a] -> a
middleItem xs = xs !! (length xs `div` 2)

violatesRules :: [Rule] -> (Int, Int) -> Bool
violatesRules rules (a, b) = (b, a) `elem` rules

reorder :: [Rule] -> Update -> Update
reorder _ [] = []
reorder rules (a : bs)
  | null l = a : reorder rules bs
  | otherwise = reorder rules (y : replace [y] [x] bs)
  where
    l = filter (violatesRules rules) [(a, n) | n <- bs]
    (x, y) = head l
