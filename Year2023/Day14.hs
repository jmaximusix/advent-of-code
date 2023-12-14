{-# LANGUAGE NumericUnderscores #-}

module Day14 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.List (partition, sort)
import Data.List.Extra (sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Ord
import qualified Data.Set as Set
import Data.Tuple.Extra (both, dupe, swap)
import Debug.Trace (traceShow, traceShowId)
import Geometry

type BoardInfo = (Set.Set Pos, (Int, Int))

part1, part2 :: Grid Char -> Int
part1 input = load
  where
    ((cubes, rounded), dim) = parseInput input
    rounded' = rollDirection (U, dim) cubes rounded
    load = evaluateLoad rounded' (snd dim)
part2 input = evaluateLoad aftern (snd dim)
  where
    aftern = rollNCycles 999_999_999 dim cubes rounded
    ((cubes, rounded), dim) = parseInput input

rollNCycles :: Int -> (Int, Int) -> Set.Set Pos -> Set.Set Pos -> Set.Set Pos
rollNCycles n dim cubes rounded = record Map.! n'
  where
    n' = ((n - a) `mod` period) + a
    period = b - a
    (record, (a, b)) = rollUntilRepeating dim 0 Map.empty cubes rounded

rollUntilRepeating :: (Int, Int) -> Int -> Map.Map (Set.Set Pos) Int -> Set.Set Pos -> Set.Set Pos -> (Map.Map Int (Set.Set Pos), (Int, Int))
rollUntilRepeating _ n _ _ _ | traceShow n False = undefined
rollUntilRepeating dim n record cubes rounded
  | next `Map.member` record = (Map.fromList $ map swap $ Map.toList record, (record Map.! next, n))
  | otherwise = rollUntilRepeating dim (n + 1) (Map.insert next n record) cubes next
  where
    next = rollOneCycle dim cubes rounded

rollOneCycle :: (Int, Int) -> Set.Set Pos -> Set.Set Pos -> Set.Set Pos
rollOneCycle dim cubes rounded = foldl (\r d -> rollDirection (d, dim) cubes r) rounded [U, L, D, R]

evaluateLoad :: Set.Set Pos -> Int -> Int
evaluateLoad rounded ydim = sum $ map (\(_, y) -> ydim - y) $ Set.toList rounded

rollDirection :: (Direction, (Int, Int)) -> Set.Set Pos -> Set.Set Pos -> Set.Set Pos
rollDirection ds cubes r = foldl (yeet ds cubes) Set.empty $ sortInDirection (fst ds) $ Set.toList r

sortInDirection :: Direction -> [Pos] -> [Pos]
sortInDirection d
  | d == U || d == L = sort
  | otherwise = sortBy (comparing Data.Ord.Down)

yeet :: (Direction, (Int, Int)) -> Set.Set Pos -> Set.Set Pos -> Pos -> Set.Set Pos
yeet ds cubes rounded pos = Set.insert (newY ds (Set.union cubes rounded) pos) rounded

newY :: (Direction, (Int, Int)) -> Set.Set Pos -> Pos -> Pos
newY (d, dim) blocked p
  | p' `Set.member` blocked || not (isInside dim p') = p
  | otherwise = newY (d, dim) blocked p'
  where
    p' = neighborTo d p

parseInput :: Grid Char -> (BoardInfo, Set.Set Pos)
parseInput input = ((cubes, dimensions input), rounded)
  where
    zipped = zipPoints input
    (cubes, rounded) =
      both
        (Set.fromList . map fst)
        (filter ((== '#') . snd) zipped, filter ((== 'O') . snd) zipped)