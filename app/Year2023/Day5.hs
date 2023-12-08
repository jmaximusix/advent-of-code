module Year2023.Day5 (part1, part2) where

import Data.Bifunctor (bimap, first)
import Data.List.Extra (chunksOf, uncons)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type Range = (Int, Int)

part1, part2 :: [String] -> Int
part1 = minimum . uncurry (foldl applyOneMap) . parseInput
part2 =
  minimum
    . map fst
    . uncurry (foldl applyOneMap2)
    . first (map (\[a, b] -> (a, b)) . chunksOf 2)
    . parseInput

parseInput :: [String] -> ([Int], [[[Int]]])
parseInput =
  bimap
    (map read . tail . words . head)
    (map (map (map (\x -> read x :: Int) . words) . tail))
    . fromJust
    . uncons
    . splitOn [[]]

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

-- a is the range that gets split on b, seperated in (inside, outside)
intersect :: Range -> Range -> ([Range], [Range])
intersect a@(a1, l1) b@(b1, l2)
  | a1 >= b1 && a2 <= b2 = ([a], [])
  | a1 < b1 && a2 > b2 = ([b], [(a1, b1 - a1), (b2, a2 - b2)])
  | a1 < b2 && a2 > b2 = ([(a1, b2 - a1)], [(b2, a2 - b2)])
  | a1 < b1 && a2 > b1 = ([(b1, a2 - b1)], [(a1, b1 - a1)])
  | otherwise = ([], [a])
  where
    a2 = a1 + l1
    b2 = b1 + l2
