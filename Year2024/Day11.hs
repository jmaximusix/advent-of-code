{-# LANGUAGE TupleSections #-}

module Day11 (part1, part2) where

import qualified Data.Foldable as Set
import Data.IntMap (keysSet)
import qualified Data.Map as Map (Map, empty, fold, foldWithKey, foldlWithKey, foldrWithKey, fromList, insertWith, keys, keysSet, map, mapKeys, mapWithKey, toList, (!))
import Debug.Trace (traceShow, traceShowId)

type Stone = Integer

-- stone, count
type StoneMap = Map.Map Integer Integer

part1, part2 :: [String] -> Integer
-- part1 = length . repeatNTimes 25 . map read . words . head
part1 = sum . map snd . Map.toList . applyNTimes 25 . Map.fromList . map ((,1) . read) . words . head
part2 = sum . map snd . Map.toList . applyNTimes 75 . Map.fromList . map ((,1 :: Integer) . read) . words . head

repeatNTimes :: Int -> [Stone] -> [Stone]
repeatNTimes 0 stones = stones
repeatNTimes n stones = concatMap applyRule $ repeatNTimes (n - 1) stones

applyNTimes :: Int -> StoneMap -> StoneMap
applyNTimes 0 stones = stones
applyNTimes n stones = soos
  where
    soos = Map.foldlWithKey nextGen Map.empty $ applyNTimes (n - 1) stones

nextGen :: StoneMap -> Stone -> Integer -> StoneMap
nextGen stones stone count = Map.foldlWithKey insert stones $ Map.fromList $ combine $ map (,count) $ applyRule stone

insert :: StoneMap -> Stone -> Integer -> StoneMap
insert stones stone count = Map.insertWith (+) stone count stones

combine :: [(Stone, Integer)] -> [(Stone, Integer)]
combine [] = []
combine [x] = [x]
combine [(x, cx), (y, cy)]
  | x == y = [(x, cx + cy)]
  | otherwise = [(x, cx), (y, cy)]

applyRule :: Stone -> [Stone]
applyRule 0 = [1]
applyRule n
  | even (length asstr) = [read before, read after]
  | otherwise = [n * 2024]
  where
    asstr = show n
    (before, after) = splitAt (length asstr `div` 2) asstr

-- (9456,2),(9494,1),(9888,1),(16192,1),(24579456,1),(32772608,3),(36869184,1)

-- (88,1),(94,1),(98,1),(2457,1),
-- (2608,3),(3277,3),(3686,1),(9184,1),(9456,1),(32772608,1)