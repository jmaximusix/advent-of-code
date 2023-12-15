module Day16 (part1, part2) where

import Algorithm.Search (aStar, bfs, dijkstra)
import Data.Char (isDigit)
import Data.List (find, nub, partition, sort, (\\))
import Data.List.Extra (maximumOn, splitOn)
import Data.Map (Map, empty, findWithDefault)
import qualified Data.Map as Map (empty, filter, filterWithKey, fromList, insert, insertWith, keys, lookup, toList, (!))
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set, member, notMember)
import qualified Data.Set as Set (delete, empty, foldl, fromList, insert, map, toList)
import Data.Tuple.Extra (snd3, thd3)
import Debug.Trace (trace, traceShow, traceShowId)
import MyLib (replace)

type Valve = (Int, Int)

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

part1, part2 :: [String] -> String
part1 input = show ast
  where
    ast = dijkstra (next dmap) transitionCost isDone initial
    initial = State laa vs 0 30
    (dmap, vs, laa) = parseValves input
part2 = undefined

next :: DistanceMap -> State -> [State]
next adj state
  | null (remaining state) = [Done (pressure state)]
  | time state == 0 = [Done (pressure state)]
  | otherwise = [move adj state v | v <- Set.toList $ remaining state]

move :: DistanceMap -> State -> (Int, Int) -> State
move adj (State l r p t) (l', f)
  | t' > 0 = State l' (Set.delete (l', f) r) (p + f * t') t'
  | otherwise = State l r p 0
  where
    t' = t - (dist adj l l' + 1)

dist :: DistanceMap -> Int -> Int -> Int
dist adj l l' = adj Map.! Set.fromList [l, l']

-- all the pressure you missed
transitionCost :: State -> State -> Int
transitionCost _ (Done _) = 0
transitionCost (State _ r p t) (State _ r' p' t') = p + Set.foldl (\acc (_, f) -> acc + t * f) 0 r - p' - Set.foldl (\acc (_, f) -> acc + t' * f) 0 r'

-- ghostMode :: DistanceMap -> State -> Int

-- ghostMode

-- ghostMode adj (State l r p t) = Set.foldl (\acc (_, f) -> acc + t * f) 0 r - Set.foldl (\acc (l', f) -> acc + t' l' * f) 0 r
--   where
--     t' x = t - (dist adj l x + 1)

isDone :: State -> Bool
isDone (Done _) = True
isDone _ = False

parseValves :: [String] -> (DistanceMap, Set Valve, Int)
parseValves s = (Map.fromList adjmatrix, Set.fromList valves, lAA)
  where
    indices = [0 .. length s - 1]
    lAA = namemap Map.! "AA"
    namemap = Map.fromList $ zip names indices
    names = map (\x -> words x !! 1) s
    flow = map (\x -> read $ takeWhile isDigit $ splitOn "=" (words x !! 4) !! 1) s
    adjacent = Map.fromList $ zip indices $ map (\x -> map ((namemap Map.!) . take 2) (drop 9 $ words x)) s
    valves = filter ((> 0) . snd) (zip indices flow)
    relevant = lAA : map fst valves
    adjmatrix = [(Set.fromList [a, b], length $ fromJust (bfs (adjacent Map.!) (== b) a)) | a <- relevant, b <- relevant, a /= b]