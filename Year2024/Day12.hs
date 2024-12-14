module Day12 (part1, part2) where

import Control.Applicative (liftA2)
import Data.Foldable.Extra (sumOn')
import qualified Data.Map.Strict as Map (lookupMin, withoutKeys)
import Data.Maybe (fromJust)
import qualified Data.Set as Set (Set, difference, fromList, member, singleton, unions)
import Data.Tuple.Extra (dupe)
import MyLib (Grid, Part (..), count)
import MyLib.GridV (Direction, GridMap, VecPos, asGridMap, directDirs, gridElementsSame, neighbors, turn90L)

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
      Part1 -> (4 -) . count (`Set.member` region) . neighbors
      Part2 -> (\p -> count (hasNewEdge region p) directDirs)
    region = findRegion grid . dupe . Set.singleton . fst . fromJust . Map.lookupMin $ grid

findRegion :: GridMap Char -> (Set.Set VecPos, Set.Set VecPos) -> Set.Set VecPos
findRegion grid (edge, acc)
  | null new = acc
  | otherwise = findRegion grid (new, Set.unions [acc, new])
  where
    new = next `Set.difference` acc
    next = Set.fromList (concatMap (liftA2 filter (gridElementsSame grid) neighbors) edge)

hasNewEdge :: Set.Set VecPos -> VecPos -> Direction -> Bool
hasNewEdge region p dir = not (f dir) && (not (f dir') || f (dir + dir'))
  where
    dir' = turn90L dir
    f = (`Set.member` region) . (p +)