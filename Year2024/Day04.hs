module Day04 (part1, part2) where

import Data.List (transpose)
import Data.Set (Set, member)
import Debug.Trace (traceShow, traceShowId)
import Geometry (Grid, Pos, getGridElement, getGridElementWithDefault, pointList)
import Text.Regex.TDFA

part1, part2 :: [String] -> Int
part1 s = sum (map countXmas s) + sum (map countXmas (map reverse s)) + sum (map countXmas (transpose s)) + sum (map countXmas (map reverse (transpose s))) + sum (map countXmas (diagonals s)) + sum (map countXmas (map reverse (diagonals s)))
part2 s = length $ filter (posMas s) (pointList s)

countXmas :: String -> Int
countXmas input = input =~ "XMAS" :: Int

posMas :: Grid Char -> Pos -> Bool
posMas g pos@(x, y)
  | c /= 'A' = False
  | otherwise = ((lu == 'M' && rd == 'S') || (rd == 'M' && lu == 'S')) && ((ru == 'M' && ld == 'S') || (ld == 'M' && ru == 'S'))
  where
    c = getGridElement g pos
    lu = getGridElementWithDefault ' ' g (x - 1, y - 1)
    ru = getGridElementWithDefault ' ' g (x + 1, y - 1)
    ld = getGridElementWithDefault ' ' g (x - 1, y + 1)
    rd = getGridElementWithDefault ' ' g (x + 1, y + 1)

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals xss = mainDiags ++ antiDiags
  where
    n = length xss
    m = length (head xss)
    mainDiags = [[xss !! (i - k) !! k | k <- [0 .. min i (m - 1)], i - k < n] | i <- [0 .. n + m - 2]]
    antiDiags = [[xss !! (i - k) !! (m - 1 - k) | k <- [0 .. min i (m - 1)], i - k < n] | i <- [0 .. n + m - 2]]