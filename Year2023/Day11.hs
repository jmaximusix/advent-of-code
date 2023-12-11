module Day11 (part1, part2) where

import Combinatorics (tuples)
import Data.Bifunctor (bimap)
import Data.List (transpose)
import qualified Data.Set as Set (Set, fromList, map, toList)
import Data.Tuple.Extra (both, dupe, second, swap)
import Geometry (Grid, Pos, pointList, tcabDist)

part1, part2 :: Grid Char -> Int
part1 = solve 1
part2 = solve 999999

solve :: Int -> Grid Char -> Int
solve expFactor =
  sum
    . map (\[a, b] -> tcabDist a b)
    . tuples 2
    . Set.toList
    . expandUniverse expFactor
    . parseInput

isStar :: Char -> Bool
isStar = (== '#')

parseInput :: Grid Char -> (Set.Set Pos, ([Int], [Int]))
parseInput =
  bimap
    ( Set.fromList
        . map snd
        . filter (isStar . fst)
        . uncurry zip
        . bimap concat pointList
    )
    ( both
        (map fst . filter (not . any isStar . snd) . zip [0 ..])
        . second transpose
    )
    . both dupe
    . dupe

expandUniverse :: Int -> (Set.Set Pos, ([Int], [Int])) -> Set.Set Pos
expandUniverse expFactor (set, empty) =
  foldl
    (expandDimension expFactor swap)
    (foldl (expandDimension expFactor id) set emptyColumns)
    emptyRows
  where
    (emptyRows, emptyColumns) = both (zipWith (+) [0, expFactor ..]) empty

expandDimension :: Int -> (Pos -> Pos) -> Set.Set Pos -> Int -> Set.Set Pos
expandDimension expFactor f set rowOrCol = Set.map expand set
  where
    expand p =
      let (a, b) = f p
       in if a > rowOrCol then f (a + expFactor, b) else p