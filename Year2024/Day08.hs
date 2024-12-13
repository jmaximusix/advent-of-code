module Day08 (part1, part2) where

import Combinatorics (variate)
import Control.Monad.Extra (guarded)
import Data.List (sortOn)
import Data.List.Extra (groupOn)
import qualified Data.Set as Set (empty, fromList, union)
import Geometry (Grid, isInside, toPos, zipPoints)
import Linear (V2 (V2))
import MyLib (tup2)

part1, part2 :: Grid Char -> Int
part1 = solve (\g (a, b) -> guarded (isInside g . toPos) (2 * a - b))
part2 = solve (\g (a, b) -> takeWhile (isInside g . toPos) (iterate (fmap (`div` uncurry gcd (toPos (b - a))) (b - a) +) b))

solve :: (Grid Char -> (V2 Int, V2 Int) -> [V2 Int]) -> Grid Char -> Int
solve antennas g =
  length
    . foldl
      ( \s ->
          Set.union s
            . Set.fromList
            . concatMap (antennas g . tup2)
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