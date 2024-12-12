module Day12 (part1, part2) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as Map (Map, empty, foldl, foldlWithKey, insert, insertWith, lookup)
import qualified Data.Set as Set (Set, difference, filter, findMin, fromList, singleton, toAscList, toList, union)
import Debug.Trace (traceShow, traceShowId)
import Geometry (Direction (D, L, R, U), Grid, Pos, getGridElementSafe, neighborTo, neighbors, turn, zipPoints)

part1, part2 :: Grid Char -> Int
part1 input = Map.foldl (\acc (a, perim) -> acc + a * perim) 0 $ areasAndPerimeters input
part2 input = Map.foldl (\acc (a, edgecount) -> acc + a * edgecount) 0 $ areasAndPerimeters input

areasAndPerimeters :: Grid Char -> Map.Map RegionKey (Int, Int)
areasAndPerimeters input = snd $ foldl (update input) (Map.empty, Map.empty) $ zipPoints input

type RegionKey = Pos

type KeyMap = Map.Map Pos RegionKey

getAndUpdateRegionKey :: Grid Char -> KeyMap -> (Pos, Char) -> (RegionKey, KeyMap)
getAndUpdateRegionKey grid regionmap (p1, c) = case maybekey of
  Just key -> (key, regionmap)
  Nothing -> (newregionkey, foldl (\m v -> Map.insert v newregionkey m) regionmap $ Set.toList newregionset)
  where
    maybekey = Map.lookup p1 regionmap
    newregionset = go $ Set.singleton p1
    newregionkey = Set.findMin newregionset
    go :: Set.Set Pos -> Set.Set Pos
    go ps
      | (not . null) (next `Set.difference` ps) = go $ ps `Set.union` next
      | otherwise = ps
      where
        next = Set.fromList $ concatMap (filter ((== Just c) . getGridElementSafe grid) . neighbors) ps

hasNewEdge :: Direction -> Grid Char -> (Pos, Char) -> Bool
hasNewEdge dir grid (p, c) = not direct && (not before || diag)
  where
    d' = case dir of
      L -> U
      U -> L
      R -> U
      D -> L
    [direct, before, diag] =
      map
        ((== Just c) . getGridElementSafe grid . (\f -> f p))
        [neighborTo dir, neighborTo d', neighborTo dir . neighborTo d']

countNewEdges :: Grid Char -> (Pos, Char) -> Int
countNewEdges grid s = length $ filter (\d -> hasNewEdge d grid s) [L, R, U, D]

update :: Grid Char -> (KeyMap, Map.Map RegionKey (Int, Int)) -> (Pos, Char) -> (KeyMap, Map.Map RegionKey (Int, Int))
update grid (regionkeys, acc) (p, c) = (regionkeys', Map.insertWith combine key (1, dperim) acc)
  where
    (key, regionkeys') = getAndUpdateRegionKey grid regionkeys (p, c)
    dperim = 4 - length (filter ((== Just c) . getGridElementSafe grid) $ neighbors p)
    -- dperim = countNewEdges grid (p, c)
    combine (a1, p1) (a2, p2) = (a1 + a2, p1 + p2)