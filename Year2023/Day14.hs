{-# LANGUAGE NumericUnderscores #-}

module Day14 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (groupBy, sort, sortOn, tails)
import Data.List.Extra (groupSortOn, sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Ord
import qualified Data.Set as Set
import Data.Tuple.Extra (both, swap)
import Debug.Trace (traceShow, traceShowId)
import Geometry

part1, part2 :: Grid Char -> Int
part1 input = let (b, r) = parseInput input in evaluateLoad b (rollDirection b U r)
part2 input = let (b, r) = parseInput input in evaluateLoad b (rollNCycles 999_999_999 b r)

rollNCycles :: Int -> Grid Char -> Set.Set Pos -> Set.Set Pos
rollNCycles n board rounded = traceShow (a, b) record Map.! n'
  where
    n' = ((n - a) `mod` period) + a
    period = b - a
    (record, (a, b)) = rollUntilRepeating 0 board Map.empty rounded

rollUntilRepeating :: Int -> Grid Char -> Map.Map (Set.Set Pos) Int -> Set.Set Pos -> (Map.Map Int (Set.Set Pos), (Int, Int))
rollUntilRepeating n board record rounded
  | next `Map.member` record = (Map.fromList $ map swap $ Map.toList record, (record Map.! next, n))
  | otherwise = rollUntilRepeating (n + 1) board (Map.insert next n record) next
  where
    next = rollOneCycle board rounded

rollOneCycle :: Grid Char -> Set.Set Pos -> Set.Set Pos
rollOneCycle b rounded = foldl (flip (rollDirection b)) rounded [U, L, D, R]

rollDirection :: Grid Char -> Direction -> Set.Set Pos -> Set.Set Pos
rollDirection g d
  | d == L || d == U = Set.foldl (moveToEdge g d) Set.empty
  | otherwise = Set.foldr (flip (moveToEdge g d)) Set.empty

evaluateLoad :: Grid Char -> Set.Set Pos -> Int
evaluateLoad g = sum . map (((snd . dimensions) g -) . snd) . Set.toList

moveToEdge :: Grid Char -> Direction -> Set.Set Pos -> Pos -> Set.Set Pos
moveToEdge g d blocked p
  | c == '#' || p' `Set.member` blocked = Set.insert p blocked
  | otherwise = moveToEdge g d blocked p'
  where
    p' = neighborTo d p
    c = getGridElementWithDefault '#' g p'

parseInput :: Grid Char -> (Grid Char, Set.Set Pos)
parseInput input =
  ( input,
    Set.fromList
      . map fst
      . filter
        ((== 'O') . snd)
      . zipPoints
      $ input
  )