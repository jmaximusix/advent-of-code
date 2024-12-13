module Day04 (part1, part2) where

import Control.Applicative (liftA2)
import Data.List (transpose)
import qualified Data.Set as Set (fromList)
import Data.Tuple.Extra (both)
import MyLib.Geometry (Grid, Pos, dimensions, getGridElement, getGridElementSafe, pointList)
import MyLib.Utils (count)
import Text.Regex.TDFA ((=~))

part1, part2 :: [[Char]] -> Int
part1 input = countXmas input + countXmas (transpose input) + countXmas (diagonals input)
part2 input = count (isMasCenterA input) (pointList input)

countXmas :: [String] -> Int
countXmas = sum . map (\l -> (l =~ "XMAS" :: Int) + (reverse l =~ "XMAS" :: Int))

diagonals :: [[a]] -> [[a]]
diagonals g = mainDiags g ++ mainDiags (reverse g)
  where
    (w, h) = both (subtract 1) (dimensions g)
    mainDiags g' = [[g' !! a !! (n - a) | a <- [0 .. min n w], (n - a) < h + 1] | n <- [0 .. w + h]]

isMasCenterA :: Grid Char -> Pos -> Bool
isMasCenterA g pos@(x, y)
  | getGridElement g pos /= 'A' = False
  | otherwise = diag ((x + 1, y + 1), (x - 1, y - 1)) && diag ((x + 1, y - 1), (x - 1, y + 1))
  where
    diag =
      elem (Set.fromList ['S', 'M'])
        . uncurry (liftA2 (\a b -> Set.fromList [a, b]))
        . both (getGridElementSafe g)
