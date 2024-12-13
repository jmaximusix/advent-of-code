module Day08 (part1, part2) where

import Combinatorics (variate)
import Data.List (sortOn)
import Data.List.Extra (groupOn)
import qualified Data.Set as Set (empty, fromList, union)
import Geometry (Grid, isInside, zipPoints)
import Linear (V2 (V2))
import MyLib (tup2)

part1, part2 :: Grid Char -> Int
part1 = solve (\(a, b) -> [2 * a - b])
part2 = solve (\(a, b) -> iterate ((\v@(V2 x y) -> fmap (`div` gcd x y) v) (b - a) +) b)

solve :: ((V2 Int, V2 Int) -> [V2 Int]) -> Grid Char -> Int
solve antennas g =
  length
    . foldl
      ( \s ->
          Set.union s
            . Set.fromList
            . concatMap (takeWhile (isInside g . (\(V2 x y) -> (x, y))) . antennas . tup2)
            . variate 2
      )
      Set.empty
    . ( map (map (uncurry V2 . fst))
          . groupOn snd
          . sortOn snd
          . filter ((/= '.') . snd)
          . zipPoints
      )
    $ g