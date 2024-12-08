module Day08 (part1, part2) where

import Combinatorics (variate)
import Data.List (sortOn)
import Data.List.Extra (groupOn)
import qualified Data.Set as Set (empty, fromList, union)
import Data.Tuple.Extra (both)
import Geometry (Grid, isInside, toPos, toV2, zipPoints)
import Linear (V2 (V2))
import MyLib (tup2)

part1, part2 :: Grid Char -> Int
part1 = solve (\g a b -> filter (isInside g . toPos) [2 * b - a, 2 * a - b])
part2 =
  solve
    ( \g a b ->
        let d = toPos (b - a)
            filldir f = takeWhile (isInside g . toPos) (iterate (f ((uncurry V2 . both (`div` uncurry gcd d)) d)) b)
         in filldir (+) ++ filldir (flip (-))
    )

solve :: (Grid Char -> V2 Int -> V2 Int -> [V2 Int]) -> Grid Char -> Int
solve antennas g =
  length
    . foldl
      ( \s ->
          Set.union s
            . Set.fromList
            . concatMap (uncurry (antennas g) . tup2)
            . variate 2
      )
      Set.empty
    . ( map (map (toV2 . fst))
          . groupOn snd
          . sortOn snd
          . filter ((/= '.') . snd)
          . zipPoints
      )
    $ g