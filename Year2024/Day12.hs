module Day12 (part1, part2) where

import Advent (Part (..))
import Control.Applicative (liftA2)
import Data.Foldable.Extra (sumOn')
import qualified Data.Map.Strict as Map (lookupMin, withoutKeys)
import Data.Maybe (fromJust)
import qualified Data.Set as Set (Set, difference, fromList, singleton, union)
import Geometry (DirectionV, Grid, GridMap, VecPos, asGridMap, directDirs, gridElementsSame, neighborsV, turn90R)
import MyLib (count)

part1, part2 :: Grid Char -> Int
part1 = processRegions Part1 0 . asGridMap
part2 = processRegions Part2 0 . asGridMap

processRegions :: Part -> Int -> GridMap Char -> Int
processRegions part acc grid
  | null grid = acc
  | otherwise = processRegions part acc' (grid `Map.withoutKeys` region)
  where
    acc' = acc + (length region * sumOn' f region)
    f = case part of
      Part1 -> (4 -) . liftA2 count (gridElementsSame grid) neighborsV
      Part2 -> (\p -> count (hasNewEdge grid p) directDirs)
    region = findRegion grid $ Set.singleton . fst . fromJust . Map.lookupMin $ grid

findRegion :: GridMap Char -> Set.Set VecPos -> Set.Set VecPos
findRegion grid r
  | null (next `Set.difference` r) = r
  | otherwise = findRegion grid $ r `Set.union` next
  where
    next = Set.fromList $ concatMap (liftA2 filter (gridElementsSame grid) neighborsV) r

hasNewEdge :: GridMap Char -> VecPos -> DirectionV -> Bool
hasNewEdge grid p dir = not (f dir) && (not (f dir') || f (dir + dir'))
  where
    f = gridElementsSame grid p
    dir' = turn90R dir