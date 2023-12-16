module Day16 (part1, part2) where

import Data.List (partition)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Geometry

part1, part2 :: Grid Char -> Int
part1 = energized ((0, 0), R)
part2 grid = maximum $ map (`energized` grid) $ allEdges grid

allEdges :: Grid Char -> [(Pos, Direction)]
allEdges grid = top ++ bottom ++ left ++ right
  where
    (x, y) = dimensions grid
    top = [((x', 0), D) | x' <- [0 .. x - 1]]
    bottom = [((x', y - 1), U) | x' <- [0 .. x - 1]]
    left = [((0, y'), R) | y' <- [0 .. y - 1]]
    right = [((x - 1, y'), L) | y' <- [0 .. y - 1]]

energized :: (Pos, Direction) -> Grid Char -> Int
energized loc grid = Set.size $ Set.map fst $ fillGrid grid [loc] $ Set.singleton loc

fillGrid :: Grid Char -> [(Pos, Direction)] -> Set.Set (Pos, Direction) -> Set.Set (Pos, Direction)
fillGrid grid locations visited
  | null new = visited
  | otherwise = fillGrid grid new visited'
  where
    new = concatMap (filter (`Set.notMember` visited) . moveOneStep grid) locations
    visited' = Set.union visited $ Set.fromList new

moveOneStep :: Grid Char -> (Pos, Direction) -> [(Pos, Direction)]
moveOneStep grid (pos, dir)
  | isNothing nextPos = []
  | nextPos == Just '.' = [(neigh, dir)]
  | nextPos == Just '/' = case dir of
      U -> [(neigh, R)]
      D -> [(neigh, L)]
      L -> [(neigh, D)]
      R -> [(neigh, U)]
  | nextPos == Just '\\' = case dir of
      U -> [(neigh, L)]
      D -> [(neigh, R)]
      L -> [(neigh, U)]
      R -> [(neigh, D)]
  | nextPos == Just '|' = case dir of
      U -> [(neigh, U)]
      D -> [(neigh, D)]
      L -> [(neigh, U), (neigh, D)]
      R -> [(neigh, U), (neigh, D)]
  | nextPos == Just '-' = case dir of
      U -> [(neigh, L), (neigh, R)]
      D -> [(neigh, L), (neigh, R)]
      L -> [(neigh, L)]
      R -> [(neigh, R)]
  where
    neigh = neighborTo dir pos
    nextPos = getGridElementSafe grid neigh
