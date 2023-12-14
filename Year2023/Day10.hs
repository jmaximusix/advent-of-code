module Day10 (part1, part2) where

import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set (Set, insert, member, singleton)
import Data.Tuple.Extra (both)
import Geometry
  ( Direction (..),
    Grid,
    Pos,
    getGridElement,
    index2d,
    invertDir,
    neighborTo,
    replace2d,
    zipPoints,
  )

-- always sorted in clockwise order
type Pipe = (Direction, Direction)

type PipeGrid = Grid (Maybe Pipe)

part1, part2 :: [String] -> Int
part1 = (`div` 2) . length . fst . getLoop
part2 = uncurry countEnclosed . getLoop

getLoop :: [String] -> (Set.Set Pos, PipeGrid)
getLoop charGrid = (loop, grid)
  where
    loop = traverseLoop grid (s, dir) s
    (grid, (s, dir)) = start charGrid

countEnclosed :: Set.Set Pos -> PipeGrid -> Int
countEnclosed loop =
  fst
    . foldl count (0, 0 :: Int)
    . map (uncurry evalIntersect)
    . zipPoints
  where
    count (c, n) n'
      | n' == 0 && odd (n `div` 2) = (c + 1, n)
      | otherwise = (c, n + n')
    evalIntersect p g
      | p `Set.member` loop = (pipeToNum . fromJust) g
      | otherwise = 0
    pipeToNum p
      | p == (L, R) = 4
      | p == (U, D) = 2
      | p == (L, U) || p == (R, D) = 1
      | p == (U, R) || p == (D, L) = -1

traverseLoop :: PipeGrid -> (Pos, Direction) -> Pos -> Set.Set Pos
traverseLoop grid (p, d) startp
  | p' == startp = Set.singleton p'
  | otherwise = Set.insert p' $ traverseLoop grid (p', d') startp
  where
    p' = neighborTo d p
    (a, b) = fromJust $ getGridElement grid p'
    d' = if invertDir d == a then b else a

-- returns (grid, (startPos, direction you're *going to*))
start :: [String] -> (PipeGrid, (Pos, Direction))
start charGrid = (replace2d p (Just (a, b)) grid, (p, a))
  where
    [a, b] = mapMaybe (\d -> adjacentPipes d >>= connects d) [L, U, R, D]
    adjacentPipes = getGridElement grid . flip neighborTo p
    connects d ds = if uncurry (||) $ both ((d ==) . invertDir) ds then Just d else Nothing
    p = index2d 'S' charGrid
    grid = map (map parsePipe) charGrid

parsePipe :: Char -> Maybe Pipe
parsePipe c
  | c == 'J' = Just (L, U)
  | c == 'L' = Just (U, R)
  | c == '7' = Just (D, L)
  | c == 'F' = Just (R, D)
  | c == '|' = Just (U, D)
  | c == '-' = Just (L, R)
  | otherwise = Nothing
