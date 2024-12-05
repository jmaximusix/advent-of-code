module Day05 (part1, part2) where

import Data.List (sortBy)
import Data.List.Extra (splitOn)
import GHC.Utils.Misc (isSortedBy)
import MyLib (tup2)

type Rule = (Int, Int)

part1, part2 :: [String] -> Int
part1 = sum . map middleItem . uncurry (filter . isSortedBy . ruleOrder) . parseInput
part2 input = sum . map (middleItem . sortBy f) $ filter (not . isSortedBy f) updates
  where
    (rules, updates) = parseInput input
    f = ruleOrder rules

parseInput :: [String] -> ([Rule], [[Int]])
parseInput input = (rules, map (map read . splitOn ",") b)
  where
    rules = map (tup2 . map read . splitOn "|") a
    [a, b] = splitOn [""] input

middleItem :: [a] -> a
middleItem xs = xs !! (length xs `div` 2)

violatesRules :: [Rule] -> (Int, Int) -> Bool
violatesRules rules (a, b) = (b, a) `elem` rules

ruleOrder :: [Rule] -> Int -> Int -> Ordering
ruleOrder rules a b
  | violatesRules rules (a, b) = GT
  | violatesRules rules (b, a) = LT
  | otherwise = EQ