module Day5 (part1, part2) where

import Data.List.Extra (chunksOf)
import Data.List.Split (splitOn)

part1, part2 :: [String] -> Int
part1 input = minimum $ foldl stuffMap (seeds1 input) $ parseInput input
part2 input = minimum $ map fst $ foldl stuffMap2 (seeds2 input) $ parseInput input

seeds1 :: [String] -> [Int]
seeds1 = map read . tail . words . head

seeds2 :: [String] -> [(Int, Int)]
seeds2 input = map (\[a, b] -> (a, b)) $ chunksOf 2 $ seeds1 input

stuffMap :: [Int] -> [[Int]] -> [Int]
stuffMap seeds lookupgedöns = map (lookupShit lookupgedöns) seeds

lookupShit :: [[Int]] -> Int -> Int
lookupShit [] seed = seed
lookupShit ([dest, source, len] : xs) seed
  | seed >= source && seed < source + len = dest + seed - source
  | otherwise = lookupShit xs seed

parseInput :: [String] -> [[[Int]]]
parseInput input = map (map (map (\x -> read x :: Int) . words) . tail . lines) $ splitOn "\n\n" (unlines $ tail input)

stuffMap2 :: [(Int, Int)] -> [[Int]] -> [(Int, Int)]
stuffMap2 seeds lookupgedöns = concatMap (lookupShit2 lookupgedöns) seeds

lookupShit2 :: [[Int]] -> (Int, Int) -> [(Int, Int)]
lookupShit2 [] range = [range]
lookupShit2 ([dest, source, len] : xs) range = inside' ++ concatMap (lookupShit2 xs) outside
  where
    (inside, outside) = range `intersect` (source, len)
    inside' = map (\(a, b) -> (dest + a - source, b)) inside

type Range = (Int, Int)

-- a is the range that gets split on b, seperated in inside and outside
intersect :: Range -> Range -> ([Range], [Range])
intersect (a1, l1) (b1, l2)
  | a1 >= b1 && a2 <= b2 = ([(a1, l1)], [])
  | a1 < b1 && a2 > b2 = ([(b1, l2)], [(a1, b1 - a1), (b2, a2 - b2)])
  | a1 < b2 && a2 > b2 = ([(a1, b2 - a1)], [(b2, a2 - b2)])
  | a1 < b1 && a2 > b1 = ([(b1, a2 - b1)], [(a1, b1 - a1)])
  | otherwise = ([], [(a1, l1)])
  where
    a2 = a1 + l1
    b2 = b1 + l2
