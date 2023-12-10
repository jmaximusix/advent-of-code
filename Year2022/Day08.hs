module Day08 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.Tuple.Extra (dupe)

part1, part2 :: [String] -> Int
part1 =
  length
    . uncurry filter
    . bimap isVisible (points . length)
    . dupe
    . map (map (read . pure))
part2 =
  maximum
    . uncurry map
    . bimap scenicScore (points . length)
    . dupe
    . map (map (read . pure))

points :: Int -> [(Int, Int)]
points size = [(x, y) | x <- [0 .. size - 1], y <- [0 .. size - 1]]

isVisible :: [[Int]] -> (Int, Int) -> Bool
isVisible grid (x, y) =
  let (l, n : r) = splitAt y $ grid !! x
      (t, _ : b) = splitAt x $ map (!! y) grid
   in any ((n >) . maximum . (n - 1 :)) [l, r, t, b]

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore grid (x, y) =
  let (l, n : r) = splitAt y $ grid !! x
      (t, _ : b) = splitAt x $ map (!! y) grid
   in product $ map (viewDist n) [reverse l, r, reverse t, b]

viewDist :: Int -> [Int] -> Int
viewDist _ [] = 0
viewDist x (a : xs)
  | x > a = 1 + viewDist x xs
  | otherwise = 1