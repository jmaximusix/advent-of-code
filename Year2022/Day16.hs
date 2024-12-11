{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Day16 (part1, part2) where

import Algorithm.Search (bfs, dijkstra)
import Data.List (delete, sort)
import Data.List.Extra (maximumOn, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, (!))
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (delete, empty, fromList, map, toList)

type Valve = (Int, Int)

type Cost = Int

type DistanceMap = Map (Set Int) Int

-- (pos, available time the entity has left)
type Entity = (Int, Int)

data State = State
  { entities :: [Entity],
    remaining :: Set Valve,
    pressure :: Int
  }
  deriving (Show, Eq, Ord)

part1, part2 :: [String] -> Int
part1 input = pressure $ last path
  where
    Just (_, path) = dijkstra (next dmap) (cost dmap) (null . entities) (State [(start, 30)] valves 0)
    (dmap, valves, start) = parseValves input
part2 input = pressure $ last path
  where
    Just (_, path) = dijkstra (next dmap) (cost dmap) (null . entities) (State [(start, 26), (start, 26)] valves 0)
    (dmap, valves, start) = parseValves input

next :: DistanceMap -> State -> [State]
next adj state
  | null soos = [State [] Set.empty (pressure state)]
  | otherwise = soos
  where
    soos = map (maximumOn pressure . moveTo adj state) . Set.toList . remaining $ state

moveTo :: DistanceMap -> State -> Valve -> [State]
moveTo adj (State es r p) v@(vl, f)
  | null entityandpressure = [State [] r p]
  | otherwise = [State (sort entities') r' (p + f') | (entities', f') <- entityandpressure]
  where
    r' = v `Set.delete` r
    entityandpressure = mapMaybe (\e -> (\dt -> ((vl, dt) : delete e es, f * dt)) <$> timeAfterReaching adj vl e) es

maxAchievablePressure :: DistanceMap -> State -> Int
maxAchievablePressure adj (State es r p) = p + sum (Set.map (\(v, f) -> safemax (mapMaybe (fmap (* f) . timeAfterReaching adj v) es)) r)
  where
    safemax xs
      | null xs = 0
      | otherwise = maximum xs

cost :: DistanceMap -> State -> State -> Cost
cost adj s1 s2 = maxAchievablePressure adj s1 - maxAchievablePressure adj s2

timeAfterReaching :: DistanceMap -> Int -> Entity -> Maybe Int
timeAfterReaching adj l' (l, t)
  | dt > 0 = Just dt
  | otherwise = Nothing
  where
    dt = t - dist adj l l' - 1

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