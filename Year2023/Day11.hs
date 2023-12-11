module Day11 (part1, part2) where

import Combinatorics (tuples)
import Data.Bifunctor (Bifunctor (second), bimap)
import Data.List (transpose)
import qualified Data.Set as Set (Set, fromList, map, toList)
import Data.Tuple.Extra (both, dupe, swap)
import Geometry (Grid, Pos, pointList, tcabDist)

part1, part2 :: Grid Char -> Int
part1 = sum . map (\[a, b] -> tcabDist a b) . tuples 2 . Set.toList . expandUniverse 1 . parseInput
part2 = sum . map (\[a, b] -> tcabDist a b) . tuples 2 . Set.toList . expandUniverse 999999 . parseInput

parseInput :: Grid Char -> (Set.Set Pos, ([Int], [Int]))
parseInput =
  bimap
    ( Set.fromList
        . map snd
        . filter (\(x, _) -> x == '#')
        . uncurry zip
        . bimap concat pointList
    )
    (both getEmptyRows . second transpose)
    . both dupe
    . dupe

getEmptyRows :: Grid Char -> [Int]
getEmptyRows = map fst . filter (\(_, row) -> all (== '.') row) . zip [0 ..]

expandUniverse :: Int -> (Set.Set Pos, ([Int], [Int])) -> Set.Set Pos
expandUniverse expFactor (s, empty) =
  foldl
    (expandDimension expFactor swap)
    (foldl (expandDimension expFactor id) s emptyColumns)
    emptyRows
  where
    (emptyRows, emptyColumns) = both (zipWith (+) [0, expFactor ..]) empty

expandDimension :: Int -> (Pos -> Pos) -> Set.Set Pos -> Int -> Set.Set Pos
expandDimension factor f set rowOrCol = Set.map expand set
  where
    expand p
      | a > rowOrCol = f (a + factor, b)
      | otherwise = p
      where
        (a, b) = f p
