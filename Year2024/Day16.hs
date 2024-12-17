module Day16 (part1, part2) where

import Algorithm.Search (dijkstraAssoc)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Linear (negated)
import MyLib.GridV (Direction, DirectionOct (E), GridMap, VecPos, asGridMap, dir, pullPois, turn90L, turn90R)

part1, part2 :: [String] -> Int
part1 grid = fst . fromJust . dijkstraAssoc (next map') ((== endpos) . fst) $ (startpos, dir E)
  where
    (map', [startpos, endpos]) = pullPois "SE" '.' $ asGridMap grid
part2 input = length $ Set.map fst $ Map.keysSet ontrack
  where
    (map', [startpos, endpos]) = pullPois "SE" '.' $ asGridMap input
    mincost = part1 input
    mcmap = minRemCostMap map' mincost (Map.singleton (startpos, dir E) 0, [((startpos, dir E), 0)])
    (_, enddir) = fst . head . Map.toList . Map.filter (== mincost) $ mcmap
    endstate = (endpos, negated enddir)
    mcmap' = minRemCostMap map' mincost (Map.singleton endstate 0, [(endstate, 0)])
    ontrack = Map.filterWithKey (\(p, d) c -> Map.lookup (p, negated d) mcmap == Just (mincost - c)) mcmap'

next :: GridMap Char -> (VecPos, Direction) -> [((VecPos, Direction), Int)]
next map' (p, d)
  | Map.lookup (p + d) map' == Just '.' = ((p + d, d), 1) : turns
  | otherwise = turns
  where
    turns = [((p, turn90L d), 1000), ((p, turn90R d), 1000)]

minRemCostMap :: GridMap Char -> Int -> (Map.Map (VecPos, Direction) Int, [((VecPos, Direction), Int)]) -> Map.Map (VecPos, Direction) Int
minRemCostMap map' mincost (acc, edge)
  | null edge' = acc'
  | otherwise = minRemCostMap map' mincost (acc', edge')
  where
    edge' = filter (\(k, v) -> Map.lookup k acc' == Just v) next'
    acc' = foldl updateMap acc next'
    next' = concatMap (filter ((<= mincost) . snd) . (\(e, c) -> map (\(e', c') -> (e', c' + c)) $ next map' e)) edge

updateMap :: Map.Map (VecPos, Direction) Int -> ((VecPos, Direction), Int) -> Map.Map (VecPos, Direction) Int
updateMap map' ((p, d), c) = Map.insertWith min (p, d) c map'