module Day13 (part1, part2) where

import Algorithm.Search (dijkstraAssoc)
import Data.List.Extra (chunksOf, splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow)
import Linear.V2 (V2 (V2))

part1, part2 :: [String] -> Int
part1 input = sum $ map fst $ mapMaybe (\(a, b, target) -> dijkstraAssoc (move (a, b)) (\(p, _) -> p == target) (V2 0 0, (0, 0))) $ parseInput input
--   where
--     (a, b, target) = head $ parseInput input
part2 = undefined

move :: (V2 Int, V2 Int) -> (V2 Int, (Int, Int)) -> [((V2 Int, (Int, Int)), Int)]
move (a, b) (pos, (pa, pb))
  | pa >= 100 || pb > 100 = []
  | otherwise = [((pos + a, (pa + 1, pb)), 3), ((pos + b, (pa, pb + 1)), 1)]

parseInput :: [String] -> [(V2 Int, V2 Int, V2 Int)]
parseInput lines = map parseMachine machines
  where
    parseMachine [a, b, c] = (parseButton a 2 '+', parseButton b 2 '+', parseButton c 1 '=')
    parseButton s n delim = V2 (read $ init x') (read y')
      where
        [x', y'] = map ((!! 1) . splitOn [delim]) $ drop n $ words s
    machines = map (take 3) $ chunksOf 4 lines