{-# LANGUAGE NumericUnderscores #-}

module Day14 (part1, part2) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple.Extra (swap)
import MyLib.Geometry
  ( Direction (..),
    Grid,
    Pos,
    dimensions,
    getGridElementWithDefault,
    neighborTo,
    zipPoints,
  )

part1, part2 :: Grid Char -> Int
part1 grid = evaluateLoad grid (rollDirection grid U (getRounded grid))
part2 grid = evaluateLoad grid (rollNCycles 1_000_000_000 grid (getRounded grid))

evaluateLoad :: Grid Char -> Set.Set Pos -> Int
evaluateLoad g = sum . map (((snd . dimensions) g -) . snd) . Set.toList

rollNCycles :: Int -> Grid Char -> Set.Set Pos -> Set.Set Pos
rollNCycles n board rounded = record Map.! n'
  where
    n' = (n - a) `mod` (b - a) + a
    (record, (a, b)) = rollUntilRepeating 1 board Map.empty rounded

rollUntilRepeating :: Int -> Grid Char -> Map.Map (Set.Set Pos) Int -> Set.Set Pos -> (Map.Map Int (Set.Set Pos), (Int, Int))
rollUntilRepeating n board record rounded
  | next `Map.member` record = (Map.fromList . map swap . Map.toList $ record, (record Map.! next, n))
  | otherwise = rollUntilRepeating (n + 1) board (Map.insert next n record) next
  where
    next = rollOneCycle board rounded

rollOneCycle :: Grid Char -> Set.Set Pos -> Set.Set Pos
rollOneCycle b rounded = foldl (flip (rollDirection b)) rounded [U, L, D, R]

rollDirection :: Grid Char -> Direction -> Set.Set Pos -> Set.Set Pos
rollDirection g d
  | d == L || d == U = Set.foldl (moveToEdge g d) Set.empty
  | otherwise = Set.foldr (flip (moveToEdge g d)) Set.empty

moveToEdge :: Grid Char -> Direction -> Set.Set Pos -> Pos -> Set.Set Pos
moveToEdge g d blocked p
  | c == '#' || p' `Set.member` blocked = Set.insert p blocked
  | otherwise = moveToEdge g d blocked p'
  where
    p' = neighborTo d p
    c = getGridElementWithDefault '#' g p'

getRounded :: Grid Char -> Set.Set Pos
getRounded = Set.fromList . map fst . filter ((== 'O') . snd) . zipPoints
