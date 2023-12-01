module Day1 (part1, part2) where

import Advent (Part (Part1, Part2))
import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex, find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)

part1, part2 :: String -> Int
part1 input = sum $ map (calibrationValue Part1) (lines input)
part2 input = sum $ map (calibrationValue Part2) (lines input)

calibrationValue :: Part -> String -> Int
calibrationValue part line = 10 * firstDigit line + lastDigit line
  where
    digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    maybe_digit condition = maybe 0 (+ 1) . (`elemIndex` digitWords) <$> find condition digitWords
    firstDigit l@(x : xs)
      | isDigit x = digitToInt x
      | part == Part1 = firstDigit xs
      | otherwise = fromMaybe (firstDigit xs) (maybe_digit (`isPrefixOf` l))
    lastDigit l
      | isDigit (last l) = digitToInt (last l)
      | part == Part1 = lastDigit (init l)
      | otherwise = fromMaybe (lastDigit (init l)) (maybe_digit (`isSuffixOf` l))
