module Day5 (part1, part2) where

import Data.List.Extra (chunksOf)
import Data.List.Split (splitOn)

type Range = (Int, Int)

part1, part2 :: [String] -> Int
part1 input = minimum $ foldl applyOneMap (seeds1 input) $ parseInput input
part2 input = minimum $ map fst $ foldl applyOneMap2 (seeds2 input) $ parseInput input

parseInput :: [String] -> [[[Int]]]
parseInput input = map (map (map (\x -> read x :: Int) . words) . tail . lines) $ splitOn "\n\n" (unlines $ tail input)

seeds1 :: [String] -> [Int]
seeds1 = map read . tail . words . head

seeds2 :: [String] -> [Range]
seeds2 input = map (\[a, b] -> (a, b)) $ chunksOf 2 $ seeds1 input

applyOneMap :: [Int] -> [[Int]] -> [Int]
applyOneMap seeds rangeMap = map (applyToSeed rangeMap) seeds

applyToSeed :: [[Int]] -> Int -> Int
applyToSeed [] seed = seed
applyToSeed ([dest, source, len] : xs) seed
  | seed >= source && seed < source + len = dest + seed - source
  | otherwise = applyToSeed xs seed

applyOneMap2 :: [Range] -> [[Int]] -> [Range]
applyOneMap2 seeds rangeMap = concatMap (applyToSeed2 rangeMap) seeds

applyToSeed2 :: [[Int]] -> Range -> [Range]
applyToSeed2 [] range = [range]
applyToSeed2 ([dest, source, len] : xs) range = inside' ++ concatMap (applyToSeed2 xs) outside
  where
    (inside, outside) = range `intersect` (source, len)
    inside' = map (\(a, b) -> (dest + a - source, b)) inside

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
