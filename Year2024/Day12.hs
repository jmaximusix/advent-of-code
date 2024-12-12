module Day12 (part1, part2) where

import Advent (Part (..))
import qualified Data.Map.Strict as Map (Map, fromList, lookup, lookupMin, withoutKeys, (!))
import Data.Maybe (fromJust)
import qualified Data.Set as Set (difference, foldl, fromList, singleton, union)
import Geometry (Direction (D, L, R, U), Grid, Pos, neighborTo, neighbors, turn, zipPoints)

part1, part2 :: Grid Char -> Int
part1 = processRegions Part1 0 . Map.fromList . zipPoints
part2 = processRegions Part2 0 . Map.fromList . zipPoints

processRegions :: Part -> Int -> Map.Map Pos Char -> Int
processRegions part acc map'
  | null map' = acc
  | otherwise = processRegions part acc' (map' `Map.withoutKeys` region)
  where
    acc' = case part of
      Part1 -> acc + (area * perim)
      Part2 -> acc + (area * newedges)
    area = length region
    perim = Set.foldl (\acc'' p -> acc'' + perimeter p) 0 region
    perimeter p = 4 - length (filter ((== Just (map' Map.! p)) . (`Map.lookup` map')) $ neighbors p)
    newedges = Set.foldl (\acc'' p -> acc'' + countNewEdges map' (p, map' Map.! p)) 0 region
    region = findregion $ Set.singleton (fst $ fromJust $ Map.lookupMin map')
    findregion ps
      | (not . null) (next `Set.difference` ps) = findregion $ ps `Set.union` next
      | otherwise = ps
      where
        next = Set.fromList $ concatMap (\p -> filter ((== Just (map' Map.! p)) . (`Map.lookup` map')) . neighbors $ p) ps

hasNewEdge :: Direction -> Map.Map Pos Char -> (Pos, Char) -> Bool
hasNewEdge dir grid (p, c) = not direct && (not before || diag)
  where
    d' = turn R dir
    [direct, before, diag] =
      map
        ((== Just c) . (`Map.lookup` grid) . (\f -> f p))
        [neighborTo dir, neighborTo d', neighborTo dir . neighborTo d']

countNewEdges :: Map.Map Pos Char -> (Pos, Char) -> Int
countNewEdges grid s = length $ filter (\d -> hasNewEdge d grid s) [L, R, U, D]