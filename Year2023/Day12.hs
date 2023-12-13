{-# LANGUAGE TupleSections #-}

module Day12 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.List (elemIndex, intercalate, tails)
import Data.List.Extra (groupOn, splitOn)
import qualified Data.Map as Map (Map, empty, filterWithKey, fromList, fromListWith, map, mapKeys, toList, unionWith, unionsWith)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)

part1, part2 :: [String] -> Int
part1 input = sum $ map (solveLine . parseInput) input
part2 input = sum $ map (solveLine . bimap (intercalate [Unknown] . replicate 5) (concat . replicate 5) . parseInput) input

solveLine :: ([Info], [Int]) -> Int
solveLine (is, r) = alignall (Map.fromList [(is, 1)]) (r ++ [0])

isBrokenAt :: Int -> [Info] -> Bool
isBrokenAt i is
  | i >= length is || i < 0 = False
  | otherwise = is !! i == Broken

alignall :: Map.Map [Info] Int -> [Int] -> Int
alignall m [0] = sum $ Map.filterWithKey (\k _ -> Broken `notElem` k) m
alignall m (a : b : rest) = alignall yeet (b : rest)
  where
    map' = Map.toList m
    soos = map (\(is, count) -> Map.map (* count) $ alignFirst is a b) map'
    yeet = Map.unionsWith (+) soos

alignFirst :: [Info] -> Int -> Int -> Map.Map [Info] Int
alignFirst is this next = Map.mapKeys (++ rest) soos
  where
    firstBroken = fromMaybe (length is) (elemIndex Broken is) + this + 1
    (relevant, rest) = splitAt firstBroken is
    grouped = groupOn (== Good) relevant
    soos = snd $ foldl (foldlGrouped is this next) (0, Map.empty) $ init $ tails grouped

foldlGrouped :: [Info] -> Int -> Int -> (Int, Map.Map [Info] Int) -> [[Info]] -> (Int, Map.Map [Info] Int)
foldlGrouped total this next (n, remains) (xs : rest)
  | head xs == Good || l < this = (n + l, remains)
  | otherwise = (n + l, Map.unionWith (+) remains genOptions)
  where
    l = length xs
    mayNotUseLast = if (l > this) && isBrokenAt (l - this - 1) xs then 1 else 0
    genOptions =
      Map.fromListWith (+) $
        map ((,1 :: Int) . concat . trytrunc next . (: rest) . (`drop` xs) . (+ 1)) $
          filter (\n' -> not $ isBrokenAt (n + n') total) [this .. l - mayNotUseLast]

trytrunc :: Int -> [[Info]] -> [[Info]]
trytrunc n stuff = dropWhile (\xs -> notElem Broken xs && length xs < n) (init stuff) ++ [last stuff]

data Info = Unknown | Broken | Good deriving (Eq, Ord)

instance Show Info where
  show Unknown = "?"
  show Broken = "#"
  show Good = "."

charToInfo :: Char -> Info
charToInfo '#' = Broken
charToInfo '.' = Good
charToInfo '?' = Unknown

parseInput :: String -> ([Info], [Int])
parseInput str = (map charToInfo info, map read $ splitOn "," rules)
  where
    [info, rules] = words str