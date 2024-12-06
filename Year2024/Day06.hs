module Day06 (part1, part2) where

import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set (Set, delete, filter, fromList, insert, map, member)
import Geometry (Direction (R, U), Grid, Pos, dimensions, getGridElementSafe, goNSteps, index2d, isInside, replace2d, turn)

part1, part2 :: Grid Char -> Int
part1 grid = length $ fromJust $ exitPath (start, Set.fromList [start]) grid
  where
    start = (index2d '^' grid, U)
part2 grid = length $ Set.filter (isNothing . exitPath (start, Set.fromList [start]) . placeObstacleAt) possibleNewObstacles
  where
    placeObstacleAt p = replace2d p '#' grid
    possibleNewObstacles = Set.delete startpos $ fromJust $ exitPath (start, Set.fromList [start]) grid
    start@(startpos, _) = (index2d '^' grid, U)

-- Just (Set of visited positions) | Nothing (Guard is stuck in loop)
exitPath :: ((Pos, Direction), Set.Set (Pos, Direction)) -> Grid Char -> Maybe (Set.Set Pos)
exitPath (current, visited) g
  | (not . isInside (dimensions g)) loc' = Just (Set.map fst visited)
  | new `Set.member` visited = Nothing
  | otherwise = exitPath (new, new `Set.insert` visited) g
  where
    new@(loc', _) = moveGuard current g

moveGuard :: (Pos, Direction) -> Grid Char -> (Pos, Direction)
moveGuard (loc, dir) g
  | getGridElementSafe g loc' == Just '#' = moveGuard (loc, turn R dir) g
  | otherwise = (loc', dir)
  where
    loc' = goNSteps 1 dir loc
