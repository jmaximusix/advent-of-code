module Day07 (part1, part2) where

import Debug.Trace (traceShow, traceShowId)
import Geometry (Grid, Pos, index2d, replace2d)

part1, part2 :: [String] -> Int
part1 = sum . map (\x -> read $ init (head (words x))) . filter canBeTrue
part2 = undefined

canBeTrue :: String -> Bool
canBeTrue input = canCreateFrom res' nums'
  where
    (res : nums) = words input
    nums' = reverse $ map read nums
    res' = read $ init res

canCreateFrom :: Int -> [Int] -> Bool
canCreateFrom res [a] = res == a
canCreateFrom res (n : ns)
  | canCreateFrom (res - n) ns = True
  | res `mod` n == 0 && canCreateFrom (res `div` n) ns = True
  | otherwise = False