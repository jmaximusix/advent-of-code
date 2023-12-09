module Day05 (part1, part2) where

import Advent (Part (Part1, Part2))
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import MyLib (replace)
import Text.Read (readMaybe)

part1, part2 :: [String] -> String
part1 = map head . uncurry (foldl (operate Part1)) . parseInput
part2 = map head . uncurry (foldl (operate Part2)) . parseInput

parseInput :: [String] -> ([String], [[Int]])
parseInput = bimap parseInitial (map (mapMaybe readMaybe . words) . drop 2) . splitAt 8

parseInitial :: [String] -> [String]
parseInitial ls
  | all null ls = []
  | otherwise = filter (/= ' ') (map (!! 1) ls) : parseInitial (map (drop 4) ls)

operate :: Part -> [String] -> [Int] -> [String]
operate part stacks [a, s1', s2'] =
  let [s1, s2] = map (\x -> x - 1) [s1', s2']
      b = drop a (stacks !! s1)
      c = case part of
        Part1 -> reverse (take a (stacks !! s1)) ++ (stacks !! s2)
        Part2 -> take a (stacks !! s1) ++ (stacks !! s2)
   in replace s2 c (replace s1 b stacks)
