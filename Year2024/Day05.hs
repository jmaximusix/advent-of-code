module Day05 (part1, part2) where

import Combinatorics (tuples)
import Data.List.Extra (replace, splitOn)
import Debug.Trace (traceShow, traceShowId)

type Rule = (Int, Int)

part1, part2 :: [String] -> Int
part1 input = sum . map fst . traceShowId . filter (\(_, b) -> not (any (violatesRules rules) b)) $ traceShowId $ map (\x -> (middleItem x, map (\[a, b] -> (a, b)) $ tuples 2 x)) updates
  where
    (rules, updates) = parseInput input
part2 input = sum . map (middleItem . reorder rules . fst) . traceShowId . filter (\(_, b) -> any (violatesRules rules) b) $ traceShowId $ map (\x -> (x, map (\[a, b] -> (a, b)) $ tuples 2 x)) updates
  where
    (rules, updates) = parseInput input

parseInput :: [String] -> ([Rule], [[Int]])
parseInput input = (rules, map (map read . splitOn ",") b)
  where
    rules = map ((\[p, q] -> (p, q)) . map read . splitOn "|") a
    [a, b] = splitOn [""] input

middleItem :: [a] -> a
middleItem xs = xs !! (length xs `div` 2)

violatesRules :: [Rule] -> (Int, Int) -> Bool
violatesRules rules (a, b) = (b, a) `elem` rules

reorder :: [Rule] -> [Int] -> [Int]
reorder _ [] = []
reorder rules (a : bs)
  | null l = a : reorder rules bs
  | otherwise = reorder rules (y : replace [y] [x] bs)
  where
    l = filter (violatesRules rules) [(a, n) | n <- bs]
    (x, y) = head l
