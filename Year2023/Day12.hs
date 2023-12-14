{-# LANGUAGE TupleSections #-}

module Day12 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.List (elemIndex, intercalate, tails)
import Data.List.Extra (groupOn, splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Status = Unknown | Broken | Good deriving (Eq, Ord)

type Sequence = [Status]

part1, part2 :: [String] -> Int
part1 = sum . map (solveLine . parseInput)
part2 = sum . map (solveLine . resize . parseInput)
  where
    resize =
      bimap
        (intercalate [Unknown] . replicate 5)
        (concat . replicate 5)

solveLine :: (Sequence, [Int]) -> Int
solveLine (is, r) = alignAll (Map.fromList [(is, 1)]) r

isBrokenAt :: Int -> Sequence -> Bool
isBrokenAt i is
  | i >= length is || i < 0 = False
  | otherwise = is !! i == Broken

alignAll :: Map.Map Sequence Int -> [Int] -> Int
alignAll m [] = sum $ Map.filterWithKey (\k _ -> Broken `notElem` k) m
alignAll m (a : rest) = alignAll (Map.unionsWith (+) next) rest
  where
    next = map (\(is, count) -> Map.map (* count) $ alignFirst is a) $ Map.toList m

alignFirst :: Sequence -> Int -> Map.Map Sequence Int
alignFirst is size = Map.mapKeys (++ rest) possiblePlacements
  where
    firstBroken = fromMaybe (length is) (elemIndex Broken is) + size + 1
    (relevant, rest) = splitAt firstBroken is
    grouped = groupOn (== Good) relevant
    possiblePlacements = foldl (place size) Map.empty $ init $ tails grouped

place :: Int -> Map.Map Sequence Int -> [Sequence] -> Map.Map Sequence Int
place size existing (xs : rest)
  | head xs == Good || l < size = existing
  | otherwise = Map.unionWith (+) existing newPlacements
  where
    l = length xs
    upperBound = if isBrokenAt (l - size - 1) xs then l - 1 else l
    newPlacements =
      Map.fromListWith (+) $
        map ((,1) . concat . (: rest) . (`drop` xs) . (+ 1)) $
          filter (\n' -> not $ isBrokenAt n' xs) [size .. upperBound]

parseInput :: String -> (Sequence, [Int])
parseInput str = (map charToInfo info, map read $ splitOn "," rules)
  where
    [info, rules] = words str
    charToInfo c = case c of
      '#' -> Broken
      '.' -> Good
      '?' -> Unknown