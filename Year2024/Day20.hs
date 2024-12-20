module Day20 (part1, part2) where

import Algorithm.Search (bfs, pruning)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Linear (V2 (V2))
import MyLib.GridV (VecPos, asGridMap, neighbors, pullPois)
import MyLib.Utils (count)

part1, part2 :: [String] -> Int
part1 = solve 2
part2 = solve 20

solve :: Int -> [String] -> Int
solve cheattime input = sum . map (countCheats cheattime nocheats) $ Map.keys nocheats
  where
    (grid, [start, end]) = pullPois "SE" '.' $ asGridMap input
    -- technically we should also map locations that aren't on the shortest path from start to end but apparently it doesn't matter with the inputs
    nocheats = Map.fromList $ zip (end : fromJust (bfs (neighbors `pruning` (\v -> Map.lookup v grid == Just '#')) (== start) end)) [0 ..]

countCheats :: Int -> Map.Map VecPos Int -> VecPos -> Int
countCheats ct nocheats p =
  count
    (\t' -> (nocheats Map.! p) - t' >= 100)
    (mapMaybe (\(v, d) -> (+ d) <$> Map.lookup v nocheats) candidates)
  where
    candidates =
      [ (p + V2 dx dy, abs dx + abs dy)
        | dx <- [-ct .. ct],
          dy <- [-ct .. ct],
          abs dx + abs dy <= ct
      ]