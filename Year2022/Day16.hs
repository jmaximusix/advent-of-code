{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Day16 (part1, part2) where

import Algorithm.Search (aStarAssoc, bfs, dijkstraAssoc)
import Combinatorics (variate)
import Data.List (partition)
import Data.List.Extra (minimumOn, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, (!))
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set (delete, fromList, map, toList)
import Debug.Trace (traceShow)

type Valve = (Int, Int)

type Cost = Int

type DistanceMap = Map (Set Int) Int

data Position = Ready Int | InTransit Valve Int deriving (Show, Eq, Ord)

data State
  = State
      { pos :: [Position],
        remaining :: Set Valve,
        pressure :: Int,
        time :: Int
      }
  | Done Int
  deriving (Show, Eq, Ord)

part1, part2 :: [String] -> Int
part1 input = (\(Done x) -> x) $ last path
  where
    Just (_, path) = dijkstraAssoc (next dmap) (\case Done _ -> True; _ -> False) (State [Ready start] valves 0 30)
    -- Just (_, path) = aStarAssoc (next dmap) (heuristic dmap) (\case Done _ -> True; _ -> False) (State start valves 0 30)
    (dmap, valves, start) = parseValves input
part2 = undefined

next :: DistanceMap -> State -> [(State, Cost)]
next adj state
  | null (remaining state) = [(Done (pressure state), 0)]
  | otherwise = undefined
  where
    (moving, ready) = partition inTransit $ skipWaiting $ pos state
    newgoals = [zipWith (\(Ready l) v -> InTransit v l) vs ready | vs <- variate (length ready) $ Set.toList (remaining state)]

inTransit :: Position -> Bool
inTransit (InTransit _ _) = True
inTransit _ = False

skipWaiting :: [Position] -> [Position]
skipWaiting ps
  | all inTransit ps = map skip ps
  | otherwise = ps
  where
    min = minimum $ map (\(InTransit _ t) -> t) ps
    skip (InTransit l t) = let t' = t - min in if t' > 0 then InTransit l t' else Ready l

move :: DistanceMap -> State -> [Valve] -> (State, Cost)
move adj (State l r p t) vs
  | t' > 0 = (State l' (Set.delete (l', f) r) (p + f * t') t', cost)
  | otherwise = (Done p, cost)
  where
    dt = dist adj l l' + 1
    cost = min dt t * sum (Set.map snd r)
    soos = zipWith (\(Ready l'', (l', f)) -> let dt = dist adj l'' l' + 1 in InTransit ()) p vs
    t' = t - dt

heuristic :: DistanceMap -> State -> Cost
heuristic _ (Done _) = 0
heuristic adj (State l r _ t) = sum $ Set.map (\(l', f) -> (t - (dist adj l l' + 1)) * f) r

dist :: DistanceMap -> Int -> Int -> Int
dist adj (Ready l) (Ready l') = adj Map.! Set.fromList [l, l']

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