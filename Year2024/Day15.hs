{-# LANGUAGE TupleSections #-}

module Day15 (part1, part2) where

import Control.Lens (over)
import Data.Bifunctor (first)
import Data.List.Extra (chunksOf, splitOn)
import qualified Data.Map.Strict as Map (adjust, filter, keys, lookup, map, mapKeys, union)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set
import Data.Tuple.Extra (dupe)
import Debug.Trace (trace, traceShow, traceShowId)
import Linear (R1 (_x), V2 (..))
import MyLib (replace2d)
import MyLib.GridV (Direction, DirectionOct (..), GridMap, VecPos, asGridMap, dir)

part1, part2 :: [String] -> Int
part1 = sum . map gpsLoc . Map.keys . Map.filter (== 'O') . fst . uncurry (foldl move) . first pullStart . parseInput
-- part2 = sum . map gpsLoc . Map.keys . Map.filter (== '[') . (\m -> trace (printgrid m (V2 0 0)) m) . fst . uncurry (foldl move2) . first (pullStart . resizeGrid) . parseInput
part2 = sum . map gpsLoc . Map.keys . Map.filter (== '[') . fst . uncurry (foldl move2) . first (pullStart . resizeGrid) . parseInput

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

resizeGrid :: GridMap Char -> GridMap Char
resizeGrid grid = Map.union twox twoxp1
  where
    twox = Map.map (\x -> if x == 'O' then '[' else x) $ Map.mapKeys (over _x (* 2)) grid
    twoxp1 = Map.map replace2 $ Map.mapKeys (over _x (\x -> x * 2 + 1)) grid
    replace2 c = case c of
      '@' -> '.'
      'O' -> ']'
      _ -> c

move2 :: (GridMap Char, VecPos) -> Direction -> (GridMap Char, VecPos)
-- move2 (grid, robot) d' | trace (printgrid grid robot) trace (asstring d' ++ "\n") False = undefined
move2 (grid, robot) d'
  | c' == Just '.' = (grid, p')
  | c' == Just '#' = (grid, robot)
  | (d' == dir E || d' == dir W) && (c' == Just '[' || c' == Just ']') = fromMaybe (grid, robot) horiz
  | (d' == dir N || d' == dir S) && (c' == Just '[' || c' == Just ']') = fromMaybe (grid, robot) vert
  where
    horiz = (,p') . Map.adjust (const '.') p' <$> tryMove2Horizontal (grid, p') d'
    vert = (,p') <$> tryMove2Vertical (grid, p') d'
    c' = Map.lookup p' grid
    p' = robot + d'

tryMove2Horizontal :: (GridMap Char, VecPos) -> Direction -> Maybe (GridMap Char)
tryMove2Horizontal (grid, p) dir'
  | c' == Just '.' = Just (Map.adjust (const c) p' grid)
  | c' == Just '#' = Nothing
  | c' == Just '[' = Map.adjust (const ']') p' <$> tryMove2Horizontal (grid, p') dir'
  | c' == Just ']' = Map.adjust (const '[') p' <$> tryMove2Horizontal (grid, p') dir'
  where
    c = fromJust $ Map.lookup p grid
    c' = Map.lookup p' grid
    p' = p + dir'

tryMove2Vertical :: (GridMap Char, VecPos) -> Direction -> Maybe (GridMap Char)
tryMove2Vertical (grid, p) dir'
  | '#' `Set.notMember` c' = Just $ Set.foldl (flip (Map.adjust (const '.'))) grid' nowempty
  | otherwise = Nothing
  where
    affected = allAffectedVertical grid dir' . dupe . Set.singleton $ p
    moved = Set.map (+ dir') affected
    mustbefree = moved `Set.difference` affected
    c' = Set.map (fromJust . (`Map.lookup` grid)) mustbefree
    nowempty = affected `Set.difference` moved
    grid' = Set.foldl (\g p' -> Map.adjust (const (fromJust $ Map.lookup p' grid)) (p' + dir') g) grid affected

-- allAffectedVertical :: GridMap Char -> Set.Set VecPos -> Direction -> Set.Set VecPos
-- allAffectedVertical grid dir'

allAffectedVertical :: GridMap Char -> Direction -> (Set.Set VecPos, Set.Set VecPos) -> Set.Set VecPos
allAffectedVertical grid d' (edge, acc) | traceShow (edge, acc) False = undefined
allAffectedVertical grid d' (edge, acc)
  | null new = acc
  | otherwise = allAffectedVertical grid d' (new, Set.union acc new)
  where
    new = next `Set.difference` acc
    next = Set.fromList (concatMap (filter (\x -> Map.lookup x grid `elem` [Just '[', Just ']']) . adj) edge)
    adj e =
      let ce = Map.lookup e grid
       in case ce of
            Just '[' -> [e + d', e + dir E]
            Just ']' -> [e + d', e + dir W]

-- printgrid :: GridMap Char -> VecPos -> String
-- printgrid grid p@(V2 rx ry) = unlines $ replace2d (rx, ry) '@' $ chunksOf 20 $ [(\a -> fromMaybe '?' $ Map.lookup a grid) $ V2 x y | y <- [0 .. 9], x <- [0 .. 19]]

asstring :: Direction -> String
asstring (V2 0 (-1)) = "N"
asstring (V2 1 0) = "E"
asstring (V2 0 1) = "S"
asstring (V2 (-1) 0) = "W"