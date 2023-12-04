{-# LANGUAGE TupleSections #-}

module Day4 (part1, part2) where

import Data.List.Extra (splitOn)
import qualified Data.Set as Set (fromList, intersection)
import Data.Tuple.Extra (both, first)

part1, part2 :: String -> Int
part1 = sum . map ((\x -> if x == 0 then 0 else 2 ^ (x - 1)) . evaluateCard) . lines
part2 = duplicateStack . map ((1,) . evaluateCard) . lines

evaluateCard :: String -> Int
evaluateCard line = (length . uncurry Set.intersection . both Set.fromList) (drop 2 winning, yours)
  where
    [winning, yours] = map words $ splitOn " | " line

duplicateStack :: [(Int, Int)] -> Int
duplicateStack [] = 0
duplicateStack ((count, next) : xs) = count + duplicateStack (copies ++ rest)
  where
    copies = map (first (+ count)) $ take next xs
    rest = drop next xs
