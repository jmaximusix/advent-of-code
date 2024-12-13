module Day08 (part1, part2) where

import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import MyLib.Geometry (Grid, Pos)

part1, part2 :: Grid Char -> Int
part1 =
  length
    . uncurry filter
    . (&&&) isVisible (points . length)
    . map (map digitToInt)
part2 =
  maximum
    . uncurry map
    . (&&&) scenicScore (points . length)
    . map (map digitToInt)

points :: Int -> [(Int, Int)]
points size = [(x, y) | x <- [0 .. size - 1], y <- [0 .. size - 1]]

isVisible :: Grid Int -> Pos -> Bool
isVisible grid (x, y) =
  let (l, n : r) = splitAt y $ grid !! x
      (t, _ : b) = splitAt x $ map (!! y) grid
   in any ((n >) . maximum . (n - 1 :)) [l, r, t, b]

scenicScore :: Grid Int -> Pos -> Int
scenicScore grid (x, y) =
  let (l, n : r) = splitAt y $ grid !! x
      (t, _ : b) = splitAt x $ map (!! y) grid
   in product $ map (viewDist n) [reverse l, r, reverse t, b]

viewDist :: Int -> [Int] -> Int
viewDist _ [] = 0
viewDist x (a : xs)
  | x > a = 1 + viewDist x xs
  | otherwise = 1