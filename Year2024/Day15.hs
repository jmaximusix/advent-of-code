{-# LANGUAGE TupleSections #-}

module Day15 (part1, part2) where

import Data.Bifunctor (first)
import Data.List.Extra (splitOn)
import qualified Data.Map.Strict as Map (adjust, filter, keys, lookup)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow, traceShowId)
import Linear (V2 (..))
import MyLib.GridV (Direction, DirectionOct (..), GridMap, VecPos, asGridMap, dir)

part1, part2 :: [String] -> Int
part1 = sum . map gpsLoc . Map.keys . Map.filter (== 'O') . fst . uncurry (foldl move) . traceShowId . first pullStart . parseInput
part2 = undefined

gpsLoc :: VecPos -> Int
gpsLoc (V2 x y) = x + 100 * y

parseInput :: [String] -> (GridMap Char, [Direction])
parseInput ls = (asGridMap grid, map asDir $ filter (/= '\n') $ unlines rest)
  where
    [grid, rest] = splitOn [""] ls
    asDir c = case c of
      '^' -> dir N
      'v' -> dir S
      '<' -> dir W
      '>' -> dir E

pullStart :: GridMap Char -> (GridMap Char, VecPos)
pullStart grid = (grid', pos)
  where
    pos = head . Map.keys . Map.filter (== '@') $ grid
    grid' = Map.adjust (const '.') pos grid

move :: (GridMap Char, VecPos) -> Direction -> (GridMap Char, VecPos)
move (grid, robot) d'
  | c' == Just '.' = (grid, p')
  | c' == Just '#' = (grid, robot)
  | c' == Just 'O' = fromMaybe (grid, robot) soos
  where
    soos = (,p') . Map.adjust (const '.') p' <$> tryMove (grid, p') d'
    c' = Map.lookup p' grid
    p' = robot + d'

tryMove :: (GridMap Char, VecPos) -> Direction -> Maybe (GridMap Char)
tryMove (grid, p) dir'
  | c' == Just '.' = Just (Map.adjust (const 'O') p' grid)
  | c' == Just '#' = Nothing
  | c' == Just 'O' = tryMove (grid, p') dir'
  where
    c' = Map.lookup p' grid
    p' = p + dir'