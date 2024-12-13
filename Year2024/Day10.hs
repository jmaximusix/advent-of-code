module Day10 (part1, part2) where

import Data.List (nub)
import MyLib.Geometry (Grid, Pos, getGridElementSafe, neighbors, zipPoints)

part1, part2 :: Grid Char -> Int
part1 g = sum . map (trailScore g nub) . trailHeads $ g
part2 g = sum . map (trailScore g id) . trailHeads $ g

trailHeads :: Grid Char -> [Pos]
trailHeads = map fst . filter ((== '0') . snd) . zipPoints

trailScore :: Grid Char -> ([Pos] -> [Pos]) -> Pos -> Int
trailScore g nubfunc start = length $ go (9 :: Int)
  where
    go 0 = [start]
    go n = nubfunc $ concatMap (filter ((== Just ((head . show) n)) . getGridElementSafe g) . neighbors) (go (n - 1))
