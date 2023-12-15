{-# LANGUAGE LambdaCase #-}

module Day16 (part1, part2) where

import Algorithm.Search (bfs, dijkstraAssoc)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, (!))
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set (delete, fromList, map, toList)
import Debug.Trace (traceShow)

type Valve = (Int, Int)

type Cost = Int

type DistanceMap = Map (Set Int) Int

data State
  = State
      { pos :: Int,
        remaining :: Set Valve,
        pressure :: Int,
        time :: Int
      }
  | Done Int
  deriving (Show, Eq, Ord)

part1, part2 :: [String] -> Int
part1 input = (\(Done x) -> x) $ last path
  where
    Just (_, path) = dijkstraAssoc (next dmap) (\case Done _ -> True; _ -> False) (State start valves 0 30)
    (dmap, valves, start) = parseValves input
part2 = undefined

next :: DistanceMap -> State -> [(State, Cost)]
next adj state
  | null (remaining state) = [(Done (pressure state), 0)]
  | otherwise = [move adj state v | v <- Set.toList $ remaining state]

move :: DistanceMap -> State -> (Int, Int) -> (State, Cost)
move adj (State l r p t) (l', f)
  | t' > 0 = (State l' (Set.delete (l', f) r) (p + f * t') t', cost)
  | otherwise = (Done p, cost)
  where
    dt = dist adj l l' + 1
    cost = min dt t * sum (Set.map snd r)
    t' = t - dt

dist :: DistanceMap -> Int -> Int -> Int
dist adj l l' = adj Map.! Set.fromList [l, l']

parseValves :: [String] -> (DistanceMap, Set Valve, Int)
parseValves lines' = (Map.fromList distmap, Set.fromList valves, start)
  where
    words' = map words lines'
    namemap = Map.fromList $ zip (map (!! 1) words') [0 ..]
    start = namemap Map.! "AA"
    adjacent = Map.fromList . zip [0 ..] . map (map ((namemap Map.!) . take 2) . drop 9) $ words'
    valves = filter ((> 0) . snd) . zip [0 ..] . map (read . init . last . splitOn "=" . (!! 4)) $ words'
    dist' a b = length $ fromJust (bfs (adjacent Map.!) (== b) a)
    relevant = start : map fst valves
    distmap = [(Set.fromList [a, b], dist' a b) | a <- relevant, b <- relevant, a /= b]