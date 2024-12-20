{-# LANGUAGE TupleSections #-}

module Day20 (part1, part2) where

import Algorithm.Search (bfs, pruning)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import Linear (V2 (V2))
import MyLib.GridV

part1, part2 :: [String] -> Int
part1 = solve 2
part2 = solve 20

solve :: Int -> [String] -> Int
solve cheattime input = length cheats
  where
    (grid, [start, end]) = pullPois "SE" '.' $ asGridMap input
    nocheats = Map.fromList $ zip (end : fromJust (bfs (neighbors `pruning` (\v -> Map.lookup v grid == Just '#')) (== start) end)) [0 ..]
    cheats = foldl (countCheats cheattime nocheats) Set.empty $ Map.keys nocheats

countCheats :: Int -> Map.Map VecPos Int -> Set.Set (VecPos, VecPos) -> VecPos -> Set.Set (VecPos, VecPos)
countCheats ct nocheats cheats p = foldl (\c ((v, _), _) -> (p, v) `Set.insert` c) cheats successful
  where
    successful =
      filter (\((v, d), i) -> (nocheats Map.! p) - i - d >= 100 && (p, v) `Set.notMember` cheats) $
        mapMaybe (\a@(v, _) -> (a,) <$> Map.lookup v nocheats) candidates
    candidates =
      [ (p + V2 dx dy, abs dx + abs dy)
        | dx <- [-ct .. ct],
          dy <- [-ct .. ct],
          abs dx + abs dy <= ct
      ]