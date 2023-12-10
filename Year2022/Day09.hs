module Day09 (part1, part2) where

import Data.List (nub)

part1, part2 :: [String] -> Int
part1 = length . nub . moveNTails 1 (0, 0)
part2 = length . nub . moveNTails 9 (0, 0)

moveHead :: [(Int, Int)] -> String -> [(Int, Int)]
moveHead positions instruction
  | dir == 'R' = positions ++ [(x + a, y) | a <- range]
  | dir == 'L' = positions ++ [(x - a, y) | a <- range]
  | dir == 'U' = positions ++ [(x, y + a) | a <- range]
  | dir == 'D' = positions ++ [(x, y - a) | a <- range]
  where
    (x, y) = last positions
    dir = head instruction
    range = [1 .. read $ drop 2 instruction]

moveTail :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
moveTail positions (headx, heady)
  | hasToMove = positions ++ [(x + signum dx, y + signum dy)]
  | otherwise = positions
  where
    (x, y) = last positions
    (dx, dy) = (headx - x, heady - y)
    hasToMove = any ((1 <) . abs) [dx, dy]

moveNTails :: Int -> (Int, Int) -> [String] -> [(Int, Int)]
moveNTails 0 start instructs = foldl moveHead [start] instructs
moveNTails n start instructs = foldl moveTail [start] $ moveNTails (n - 1) start instructs