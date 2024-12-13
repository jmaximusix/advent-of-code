module Day01 (part1, part2) where

import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex, find, isPrefixOf)
import MyLib (Part (..))

part1, part2 :: [String] -> Int
part1 = sum . map (calibrationValue Part1)
part2 = sum . map (calibrationValue Part2)

digitWords :: [String]
digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

calibrationValue :: Part -> String -> Int
calibrationValue part line = 10 * firstDigit digitWords line + firstDigit (map reverse digitWords) (reverse line)
  where
    firstDigit ds l@(x : xs)
      | isDigit x = digitToInt x
      | part == Part1 = firstDigit ds xs
      | otherwise = maybe (firstDigit ds xs) (maybe 0 (+ 1) . (`elemIndex` ds)) (find (`isPrefixOf` l) ds)
