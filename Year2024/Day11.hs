{-# LANGUAGE TupleSections #-}

module Day11 (part1, part2) where

import qualified Data.Map.Strict as Map (empty, foldlWithKey, fromList, insertWith)
import GHC.Num (integerLogBase)

part1, part2 :: [String] -> Int
part1 = solve 25
part2 = solve 75

solve :: Int -> [String] -> Int
solve n = sum . blink n . Map.fromList . map ((,1) . read) . words . head
  where
    blink 0 stones = stones
    blink n' stones = Map.foldlWithKey update Map.empty . blink (n' - 1) $ stones
    update stonemap s c = foldl ((flip . uncurry . Map.insertWith) (+)) stonemap . map (,c) . applyRule $ s

applyRule :: Int -> [Int]
applyRule 0 = [1]
applyRule n
  | even len = [n `div` l', n `rem` l']
  | otherwise = [n * 2024]
  where
    len = integerLogBase 10 (toInteger n) + 1
    l' = 10 ^ (len `div` 2)