module Day20 (part1, part2) where

import Algorithm.Search (bfs, pruning)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Debug.Trace (traceShow, traceShowId)
import MyLib.GridV

part1, part2 :: [String] -> Int
part1 input = fastestwithCheats
  where
    (grid, [start, end]) = pullPois "SE" '.' $ asGridMap input
    nocheats = length $ fromJust $ bfs (neighbors `pruning` (\v -> Map.lookup v grid == Just '#')) (== end) start
    fastestwithCheats = repeatWhileshorterThanN (nocheats - 100) grid Set.empty start end
part2 = undefined

repeatWhileshorterThanN :: Int -> GridMap Char -> Set.Set [VecPos] -> VecPos -> VecPos -> Int
repeatWhileshorterThanN n grid previous start end
  | length res <= n = repeatWhileshorterThanN n grid allcheats start end
  | otherwise = length $ traceShowId previous
  where
    allcheats = traceShow (length res, n) $ cheats `Set.insert` previous
    res = fromJust $ bfs (nextwithcheats grid `pruning` (\(_, (_, c)) -> c `Set.member` previous)) ((== end) . fst) (start, (2, []))
    (_, (_, cheats)) = last $ res

nextwithcheats :: GridMap Char -> (VecPos, (Int, [VecPos])) -> [(VecPos, (Int, [VecPos]))]
nextwithcheats grid (pos, (cheats, cheatloc)) = map (\v -> (v, remainingcheats v)) $ filter (\v -> cheats > 1 || (Map.lookup v grid == Just '.')) $ neighbors pos
  where
    remainingcheats v
      | cheats == 0 = (0, cheatloc)
      | cheats == 1 = (0, v : cheatloc)
      | Map.lookup v grid == Just '#' = (cheats - 1, v : cheatloc)
      | otherwise = (cheats, cheatloc)