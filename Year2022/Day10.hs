module Day10 (part1, part2) where

import Data.List.Extra (chunksOf)

part1, part2 :: [String] -> String
part1 =
  show
    . sum
    . flip map [20, 60 .. 220]
    . signalStrength
    . foldl operate [1]
part2 =
  ("\n" ++)
    . unlines
    . chunksOf 40
    . zipWith pixel [0 .. 239]
    . foldl operate [1]

operate :: [Int] -> String -> [Int]
operate register instruction
  | head cmd == "noop" = register ++ [last register]
  | head cmd == "addx" = register ++ [last register, last register + read (last cmd)]
  where
    cmd = words instruction

signalStrength :: [Int] -> Int -> Int
signalStrength register index = register !! (index - 1) * index

pixel :: Int -> Int -> Char
pixel a b
  | abs (b - a `mod` 40) <= 1 = '#'
  | otherwise = '.'