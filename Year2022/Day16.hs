{-# LANGUAGE TupleSections #-}

module Day16 (part1, part2) where

import Algorithm.Search (bfs, dijkstra)
import Control.Applicative (liftA2)
import Control.Monad.Extra (guarded)
import Data.Function (on)
import Data.List (delete, sortOn)
import Data.List.Extra (maximumOn, splitOn)
import qualified Data.Map as Map (Map, fromList, (!))
import Data.Maybe (fromJust, fromMaybe, mapMaybe)

type Entity = (Int, Int)

type Valve = (Int, Int)

type DistanceMap = Map.Map (Int, Int) Int

data State = State
  { entities :: [Entity],
    remaining :: [Valve],
    pressure :: Int
  }
  deriving (Show, Eq, Ord)

part1, part2 :: [String] -> Int
part1 = solve [30] . parseValves
part2 = solve [26, 26] . parseValves

solve :: [Int] -> (DistanceMap, [Valve], Int) -> Int
solve minutes (adj, vs, start) =
  pressure
    . last
    . snd
    . fromJust
    $ dijkstra next cost (null . entities) (State (map (start,) minutes) vs 0)
  where
    cost = (-) `on` (\(State es r p) -> p + sum (mapMaybe (\(vl, f) -> (* f) . snd <$> fastestToOpen adj vl es) r))
    next state =
      fromMaybe [State [] [] (pressure state)]
        . guarded (not . null)
        . mapMaybe (goAndOpenValve adj state)
        . remaining
        $ state

goAndOpenValve :: DistanceMap -> State -> Valve -> Maybe State
goAndOpenValve adj (State es r p) v@(vl, f) = (\(e, t) -> State (sortOn (negate . snd) ((vl, t) : delete e es)) (delete v r) (p + t * f)) <$> fastestToOpen adj vl es

fastestToOpen :: DistanceMap -> Int -> [Entity] -> Maybe (Entity, Int)
fastestToOpen adj l' =
  fmap (maximumOn snd)
    . guarded (not . null)
    . mapMaybe (liftA2 fmap (,) (\(l, t) -> guarded (> 0) $ t - (adj Map.! (min l l', max l l')) - 1))

parseValves :: [String] -> (DistanceMap, [Valve], Int)
parseValves lines' = (Map.fromList distmap, valves, start)
  where
    words' = map words lines'
    namemap = Map.fromList $ zip (map (!! 1) words') [0 ..]
    start = namemap Map.! "AA"
    adjacent = Map.fromList . zip [0 ..] . map (map ((namemap Map.!) . take 2) . drop 9) $ words'
    valves = filter ((> 0) . snd) . zip [0 ..] . map (read . init . last . splitOn "=" . (!! 4)) $ words'
    dist' a b = length $ fromJust (bfs (adjacent Map.!) (== b) a)
    relevant = start : map fst valves
    distmap = [((a, b), dist' a b) | a <- relevant, b <- relevant, a < b]