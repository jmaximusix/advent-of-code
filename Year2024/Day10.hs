module Day10 (part1, part2) where

import Data.List (nub)
import Geometry (Grid, Pos, getGridElementSafe, neighbors, zipPoints)

part1, part2 :: Grid Char -> Int
part1 g = sum . map (length . nub . paths g) . trailHeads $ g
part2 g = sum . map (length . paths g) . trailHeads $ g

trailHeads :: Grid Char -> [Pos]
trailHeads = map fst . filter ((== '0') . snd) . zipPoints

paths :: Grid Char -> Pos -> [Pos]
paths g start = go (9 :: Int)
  where
    go 0 = [start]
    go n = concatMap (neighborsWithValue g ((head . show) n)) (go (n - 1))

neighborsWithValue :: Grid Char -> Char -> Pos -> [Pos]
neighborsWithValue g c p = filter ((== Just c) . getGridElementSafe g) $ neighbors p