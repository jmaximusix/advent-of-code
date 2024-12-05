module Day05 (part1, part2) where

import Combinatorics (tuples)
import Data.List.Extra (replace, splitOn)
import Debug.Trace (traceShow, traceShowId)

part1, part2 :: [String] -> Int
part1 input = sum . map fst . traceShowId . filter (\(_, b) -> not (any (violates rules) b)) $ traceShowId $ map ((\x -> (read (middleItem x), tuples 2 x)) . splitOn ",") orders
  where
    (rules, orders) = parseInput input
-- part1 input = traceShow (map (tuples 2 . splitOn ",") orders) 1
--   where
--     (rules, orders) = parseInput input
part2 input = sum . map (read . middleItem . reorder rules . fst) . traceShowId . filter (\(_, b) -> any (violates rules) b) $ traceShowId $ map ((\x -> (x, tuples 2 x)) . splitOn ",") orders
  where
    (rules, orders) = parseInput input

parseInput :: [String] -> ([String], [String])
parseInput input = (a, b)
  where
    [a, b] = splitOn [""] input

middleItem :: [a] -> a
middleItem xs = xs !! (length xs `div` 2)

violates :: [String] -> [String] -> Bool
violates rules [a, b] = (b ++ "|" ++ a) `elem` rules

reorder :: [String] -> [String] -> [String]
reorder _ [] = []
reorder rules (a : bs)
  | null l = a : reorder rules bs
  | otherwise = reorder rules (y : replace [y] [x] bs)
  where
    l = filter (violates rules) [[a, n] | n <- bs]
    [x, y] = head l
