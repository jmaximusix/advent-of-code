module Day20 (part1, part2) where

import Data.List (elemIndex)
import Data.List.Index (deleteAt, indexed, insertAt)
import Data.Maybe (fromJust)

part1, part2 :: [String] -> Int
part1 = solve 1 . map read
part2 = solve 10 . map ((* 811589153) . read)

solve :: Int -> [Int] -> Int
solve n input = sum $ map (g (mixed n input)) [1000, 2000, 3000]
  where
    mixed n' xs = map snd $ mixN n' xs (indexed xs)
    g xs x = xs !! ((x + fromJust (elemIndex 0 xs)) `mod` length xs)

mixN :: Int -> [Int] -> [(Int, Int)] -> [(Int, Int)]
mixN 0 _ input = input
mixN n original input = mixN (n - 1) original $ foldl mix input (indexed original)

mix :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
mix input (_, 0) = input
mix input next = insertAt i' next input'
  where
    i = fromJust $ elemIndex next input
    input' = deleteAt i input
    i' = (i + snd next) `mod` length input'
