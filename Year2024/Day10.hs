module Day10 (part1, part2) where

import Control.Applicative (liftA2)
import Data.List (nub)
import MyLib.Geometry (Grid, Pos, getGridElementSafe, neighbors, zipPoints)

part1, part2 :: Grid Char -> Int
part1 = sum . liftA2 map (trailScore nub) trailHeads
part2 = sum . liftA2 map (trailScore id) trailHeads

trailHeads :: Grid Char -> [Pos]
trailHeads = map fst . filter ((== '0') . snd) . zipPoints

trailScore :: ([Pos] -> [Pos]) -> Grid Char -> Pos -> Int
trailScore nubfunc g start = length (go '9')
  where
    go '0' = [start]
    go n = nubfunc $ concatMap (filter ((== Just n) . getGridElementSafe g) . neighbors) (go (pred n))
