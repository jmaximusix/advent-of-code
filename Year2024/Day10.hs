module Day10 (part1, part2) where

import Data.List (nub)
import Geometry (Grid, Pos, getGridElementSafe, neighbors, zipPoints)

part1, part2 :: Grid Char -> Int
part1 g = sum . map (length . nub . trails g) . trailHeads $ g
part2 g = sum . map (length . trails g) . trailHeads $ g

trailHeads :: Grid Char -> [Pos]
trailHeads = map fst . filter ((== '0') . snd) . zipPoints

trails :: Grid Char -> Pos -> [Pos]
trails g start = go (9 :: Int)
  where
    go 0 = [start]
    go n = concatMap (filter ((== Just ((head . show) n)) . getGridElementSafe g) . neighbors) (go (n - 1))
