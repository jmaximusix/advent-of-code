{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Day16 (part1, part2) where

import Algorithm.Search (bfs, dijkstra)
import Control.Applicative (liftA2)
import Control.Monad.Extra (guarded)
import Data.List (delete, sort, sortOn)
import Data.List.Extra (maximumOn, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, (!))
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (delete, empty, fromList, map, toList)
import Debug.Trace (traceShow)

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
part1 = solve [30] . parseValves
part2 = solve [26, 26] . parseValves

solve :: [Int] -> (DistanceMap, Set Valve, Int) -> Int
solve ts' (ds, vs, start) =
  pressure
    . last
    . snd
    . fromJust
    $ dijkstra (next ds) (cost ds) (null . entities) (State (map (start,) ts') vs 0)

next :: DistanceMap -> State -> [State]
next adj state =
  fromMaybe [State [] Set.empty (pressure state)]
    . guarded (not . null)
    . mapMaybe (goAndOpenValve adj state)
    . Set.toList
    . remaining
    $ state

goAndOpenValve :: DistanceMap -> State -> Valve -> Maybe State
goAndOpenValve adj (State es r p) (vl, f) =
  fmap (\(e', t) -> State (sortOn (negate . snd) ((vl, t) : delete e' es)) r (p + t * f))
    . guarded (not . null)
    . maximumOn snd
    . mapMaybe (liftA2 fmap (,) (maybeTimeAfterReaching adj vl))
    $ es

maxAchievablePressure :: DistanceMap -> State -> Int
maxAchievablePressure adj (State es r p) = p + sum (Set.map (\(v, f) -> safemax (mapMaybe (fmap (* f) . maybeTimeAfterReaching adj v) es)) r)
  where
    safemax xs
      | null xs = 0
      | otherwise = maximum xs

cost :: DistanceMap -> State -> State -> Cost
cost adj s1 s2 = maxAchievablePressure adj s1 - maxAchievablePressure adj s2

maybeTimeAfterReaching :: DistanceMap -> Int -> Entity -> Maybe Int
maybeTimeAfterReaching adj l' (l, t)
  | t' > 0 = Just t'
  | otherwise = Nothing
  where
    t' = t - (adj Map.! Set.fromList [l, l']) - 1

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