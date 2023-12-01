module Day1 (part1, part2) where

import Data.Char (digitToInt, isDigit)
import Data.List (dropWhileEnd, elemIndex, find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)

part1, part2 :: String -> Int
part1 input = sum $ map calibrationValue1 (lines input)
part2 input = sum $ map calibrationValue2 (lines input)

calibrationValue1 :: String -> Int
calibrationValue1 line = read (firstDigit : [lastDigit])
  where
    firstDigit = head $ dropWhile (not . isDigit) line
    lastDigit = last $ dropWhileEnd (not . isDigit) line

calibrationValue2 :: String -> Int
calibrationValue2 line = 10 * firstDigit line + lastDigit line
  where
    digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    maybe_digit condition = maybe 0 (+ 1) . (`elemIndex` digitWords) <$> find condition digitWords
    firstDigit l@(x : xs)
      | isDigit x = digitToInt x
      | otherwise = fromMaybe (firstDigit xs) (maybe_digit (`isPrefixOf` l))
    lastDigit l
      | isDigit (last l) = digitToInt (last l)
      | otherwise = fromMaybe (lastDigit (init l)) (maybe_digit (`isSuffixOf` l))