module Day12 (part1, part2) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as Map (Map, empty, foldl, foldlWithKey, insertWith)
import qualified Data.Set as Set (Set, difference, filter, fromList, singleton, toAscList, union)
import Debug.Trace (traceShow, traceShowId)
import Geometry (Direction (D, L, R, U), Grid, Pos, getGridElementSafe, neighborTo, neighbors, zipPoints)

part1, part2 :: Grid Char -> Int
part1 input = Map.foldl (\acc (a, perim) -> acc + a * perim) 0 $ traceShowId $ areasAndPerimeters input
part2 input = Map.foldl (\acc (a, perim) -> acc + a * perim) 0 $ traceShowId $ areasAndPerimeters input

areasAndPerimeters :: Grid Char -> Map.Map RegionKey (Int, Int)
areasAndPerimeters input = foldl (update input) Map.empty $ zipPoints input

type RegionKey = Pos

regionTopLeftCorner :: Grid Char -> (Pos, Char) -> Pos
regionTopLeftCorner grid (p1, c) = head (Set.toAscList $ go $ Set.singleton p1)
  where
    go :: Set.Set Pos -> Set.Set Pos
    go ps
      | hasNew = go $ ps `Set.union` next
      | otherwise = ps
      where
        hasNew = not $ null $ next `Set.difference` ps
        next = Set.fromList $ concatMap (filter ((== Just c) . getGridElementSafe grid) . neighbors) ps

hasnewEdgeLeft :: Grid Char -> (Pos, Char) -> Bool
hasnewEdgeLeft grid (p, c) = not ln' && (not un' || lun')
  where
    [ln', un', lun'] = map ((== Just c) . getGridElementSafe grid) [ln, un, lun]
    ln = neighborTo L p
    un = neighborTo U p
    lun = neighborTo U ln

hasnewEdgeUp :: Grid Char -> (Pos, Char) -> Bool
hasnewEdgeUp grid (p, c) = not direct && (not before || diag)
  where
    [direct, before, diag] = map ((== Just c) . getGridElementSafe grid) [neighborTo U p, neighborTo L p, (neighborTo U . neighborTo L) p]

hasnewEdgeRight :: Grid Char -> (Pos, Char) -> Bool
hasnewEdgeRight grid (p, c) = not direct && (not before || diag)
  where
    [direct, before, diag] = map ((== Just c) . getGridElementSafe grid) [neighborTo R p, neighborTo U p, (neighborTo R . neighborTo U) p]

hasnewEdgeDown :: Grid Char -> (Pos, Char) -> Bool
hasnewEdgeDown grid (p, c) = not direct && (not before || diag)
  where
    [direct, before, diag] = map ((== Just c) . getGridElementSafe grid) [neighborTo D p, neighborTo L p, (neighborTo L . neighborTo D) p]

countNewEdges :: Grid Char -> (Pos, Char) -> Int
countNewEdges grid s = length $ filter id [hasnewEdgeLeft grid s, hasnewEdgeUp grid s, hasnewEdgeRight grid s, hasnewEdgeDown grid s]

update :: Grid Char -> Map.Map RegionKey (Int, Int) -> ((Int, Int), Char) -> Map.Map RegionKey (Int, Int)
update grid acc (p, c) = Map.insertWith combine key (1, dperim) acc
  where
    key = regionTopLeftCorner grid (p, c)
    -- dperim = 4 - length (filter ((== Just c) . getGridElementSafe grid) $ neighbors p)
    dperim = countNewEdges grid (p, c)
    combine (a1, p1) (a2, p2) = (a1 + a2, p1 + p2)