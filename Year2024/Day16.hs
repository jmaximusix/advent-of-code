module Day16 (part1, part2) where

import Algorithm.Search (dijkstraAssoc)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Linear (V2 (..), negated)
import MyLib.GridV (Direction, DirectionOct (E), GridMap, VecPos, asGridMap, dir, turn90L, turn90R)

part1, part2 :: [String] -> Int
part1 grid = fst . fromJust . dijkstraAssoc (next map') (\(p, _) -> Map.lookup p map' == Just 'E') $ state0
  where
    (map', state0) = pullStart $ asGridMap grid
part2 input = length $ Set.map fst $ Map.keysSet ontrack
  where
    (map', state0) = pullStart $ asGridMap input
    mincost = part1 input
    mcmap = minRemCostMap map' mincost (Map.singleton state0 0, [(state0, 0)])
    (endpos, enddir) = fst . head . Map.toList . Map.filter (== mincost) $ mcmap
    endstate = (endpos, negated enddir)
    mcmap' = minRemCostMap map' mincost (Map.singleton endstate 0, [(endstate, 0)])
    ontrack = Map.filterWithKey (\(p, d) c -> Map.lookup (p, negated d) mcmap == Just (mincost - c)) mcmap'

next :: GridMap Char -> (VecPos, Direction) -> [((VecPos, Direction), Int)]
next map' (p, d) = filter (\((p', _), _) -> Map.lookup p' map' == Just '.' || Map.lookup p' map' == Just 'E') [((p + d, d), 1), ((p, turn90L d), 1000), ((p, turn90R d), 1000)]

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

pullStart :: GridMap Char -> (GridMap Char, (VecPos, VecPos))
pullStart grid = (grid', (start, dir E))
  where
    start = head . Map.keys . Map.filter (== 'S') $ grid
    grid' = Map.adjust (const '.') start grid