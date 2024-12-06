module Day06 (part1, part2) where

import Data.Maybe (isNothing)
import qualified Data.Set as Set (Set, fromList, insert, member)
import Geometry (Direction (R, U), Grid, Pos, getGridElementSafe, goNSteps, index2d, replace2d, turn, zipPoints)

part1, part2 :: Grid Char -> Int
part1 input = moveUntilEnd (start, U, Set.fromList [start]) input
  where
    start = index2d '^' input
part2 grid = length $ filter (moveUntilLoop (start, U, Set.fromList [(start, U)]) . placeObstacleAt) possibleNewObstacles
  where
    placeObstacleAt p = replace2d p '#' grid
    possibleNewObstacles = map fst . filter (\(_, b) -> b == '.') . zipPoints $ grid
    start = index2d '^' grid

moveUntilEnd :: (Pos, Direction, Set.Set Pos) -> Grid Char -> Int
moveUntilEnd (loc, dir, visited) g
  | isNothing (getGridElementSafe g loc') = length visited
  | otherwise = moveUntilEnd (loc', dir', loc' `Set.insert` visited) g
  where
    (loc', dir') = moveGuard (loc, dir) g

moveUntilLoop :: (Pos, Direction, Set.Set (Pos, Direction)) -> Grid Char -> Bool
moveUntilLoop (loc, dir, visited) g
  | isNothing (getGridElementSafe g loc') = False
  | new `Set.member` visited = True
  | otherwise = moveUntilLoop (loc', dir', new `Set.insert` visited) g
  where
    new@(loc', dir') = moveGuard (loc, dir) g

moveGuard :: (Pos, Direction) -> Grid Char -> (Pos, Direction)
moveGuard (loc, dir) g
  | getGridElementSafe g loc' == Just '#' = moveGuard (loc, turn R dir) g
  | otherwise = (loc', dir)
  where
    loc' = goNSteps 1 dir loc
