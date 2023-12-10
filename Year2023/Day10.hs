{-# LANGUAGE TypeApplications #-}

module Day10 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.Maybe (fromJust, mapMaybe)
import Data.Tuple.Extra (both, dupe, second)
import Geometry
  ( Direction (D, L, R, U),
    Grid,
    Pos,
    getGridElement,
    index2d,
    invertDir,
    neighborInDirection,
    pointList,
    replace2d,
  )

-- always sorted in clockwise order
type Pipe = (Direction, Direction)

type PipeGrid = Grid (Maybe Pipe)

part1, part2 :: [String] -> Int
part1 = (`div` 2) . length . fst . getLoop
part2 = uncurry countEnclosed . getLoop

getLoop :: [String] -> ([Pos], PipeGrid)
getLoop charGrid = (loop, grid)
  where
    loop = traverseLoop grid (s, dir) s
    (grid, (s, dir)) = start charGrid

countEnclosed :: [Pos] -> PipeGrid -> Int
countEnclosed loop =
  length
    . filter id
    . uncurry (zipWith (\x -> (&&) x . odd @Int . (`div` 2)))
    . second (scanl1 (+))
    . unzip
    . uncurry (zipWith (\p g -> if p `elem` loop then (False, (pipeToNum . fromJust) g) else (True, 0)))
    . bimap pointList concat
    . dupe
  where
    pipeToNum p
      | p == (L, R) = 0
      | p == (U, D) = 2
      | p == (L, U) || p == (R, D) = 1
      | p == (U, R) || p == (D, L) = -1

traverseLoop :: PipeGrid -> (Pos, Direction) -> Pos -> [Pos]
traverseLoop grid (p, d) startp
  | p' == startp = [p']
  | otherwise = p' : traverseLoop grid (p', d') startp
  where
    p' = neighborInDirection d p
    (a, b) = fromJust $ getGridElement grid p'
    d' = if invertDir d == a then b else a

-- returns (grid, (startPos, direction you're *going to*))
start :: [String] -> (PipeGrid, (Pos, Direction))
start charGrid = (replace2d p (Just (a, b)) grid, (p, a))
  where
    [a, b] = mapMaybe (\d -> getGridElement grid (neighborInDirection d p) >>= connects d) [L, U, R, D]
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
