{-# LANGUAGE TupleSections #-}

module Day16 (part1, part2) where

import Data.List (tails)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Debug.Trace (traceShow, traceShowId)
import MyLib.Geometry

type Loc = (Pos, Direction)

-- parseGrid :: Grid Char -> Map.Map Loc Loc
-- parseGrid grid = yeet
--   where
--     soos = Map.fromList $ filter ((/= '.') . snd) $ zipPoints grid ++ allEdges2 grid
--     yeet = Map.mapWithKey (buildThing grid) soos

pathlengths :: Grid Char -> [Loc] -> [Int]
pathlengths grid starts = map Set.size paths
  where
    lmap = traceShowId $ buildThing grid Map.empty starts
    paths = map (beamyeah lmap Set.empty . pure) starts

beamyeah :: Map.Map Loc [Loc] -> Set.Set Pos -> [Loc] -> Set.Set Pos
beamyeah lmap pos locs
  | pos' == pos = pos
  | otherwise = beamyeah lmap pos' next
  where
    pos' = Set.union pos $ Set.fromList $ traceShowId $ concatMap (\((x, y), ls') -> concatMap (\((x', y'), _) -> [(a, b) | a <- [x .. x'], b <- [y .. y']]) ls') next'
    next' = traceShowId $ map (\loc@(p, _) -> (p, lmap Map.! loc)) locs
    next = concatMap snd next'

buildThing :: Grid Char -> Map.Map Loc [Loc] -> [Loc] -> Map.Map Loc [Loc]
buildThing g = foldl insertnew
  where
    insertnew lmap loc@(pos, dir)
      | (pos, dir) `Map.member` lmap = lmap
      | otherwise = buildThing g (Map.insert loc locs' lmap) locs'
      where
        locs' = maybe [] (\(p, c) -> [(p, d') | d' <- dirlol c dir]) (findnext g loc)

dirlol :: Char -> Direction -> [Direction]
dirlol c d = case c of
  '/' -> [mirrorXY d]
  '\\' -> [invertDir (mirrorXY d)]
  '|' -> [invertDir a | a <- [U, D], a /= d]
  '-' -> [invertDir a | a <- [L, R], a /= d]

findnext :: Grid Char -> Loc -> Maybe (Pos, Char)
findnext soos (pos, dir)
  | next == Just '.' = findnext soos (neigh, dir)
  | otherwise = (neigh,) <$> next
  where
    neigh = neighborTo dir pos
    next = getGridElementSafe soos neigh

-- soos :: Thing -> Direction -> [DT]
-- soos (Splitter H a b) dir = case dir of
--   L -> [a]
--   R -> [b]
--   U -> [a, b]
--   D -> [a, b]
-- soos (Splitter V a b) dir = case dir of
--   L -> [a, b]
--   R -> [a, b]
--   U -> [a]
--   D -> [b]
-- soos (Mirror l u r d) dir = case dir of
--   L -> [l]
--   U -> [u]
--   R -> [r]
--   D -> [d]

part1, part2 :: Grid Char -> Int
part1 = energized ((0, 0), R)
-- part2 grid = maximum $ map (`energized` grid) $ allEdges grid
part2 grid = traceShow (pathlengths grid [((0, 0), R)]) 1

allEdges2 :: Grid Char -> [(Pos, Char)]
allEdges2 grid = top ++ bottom ++ left ++ right
  where
    (x, y) = dimensions grid
    top = [((x', 0), 'D') | x' <- [0 .. x - 1]]
    bottom = [((x', y - 1), 'U') | x' <- [0 .. x - 1]]
    left = [((0, y'), 'R') | y' <- [0 .. y - 1]]
    right = [((x - 1, y'), 'L') | y' <- [0 .. y - 1]]

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
  | nextPos == Just '/' = [(neigh, mirrorXY dir)]
  | nextPos == Just '\\' = [(neigh, invertDir (mirrorXY dir))]
  | nextPos == Just '|' = [(neigh, invertDir a) | a <- [U, D], a /= dir]
  | nextPos == Just '-' = [(neigh, invertDir a) | a <- [L, R], a /= dir]
  where
    neigh = neighborTo dir pos
    nextPos = getGridElementSafe grid neigh

mirrorXY :: Direction -> Direction
mirrorXY d = case d of
  L -> D
  U -> R
  R -> U
  D -> L