module Day3 (part1, part2) where

import Data.Char (isDigit)
import Data.List (nub)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Debug.Trace (traceShow, traceShowId)

part1, part2 :: String -> Int
part1 input = sum $ mapMaybe (isPartNumber (lines input)) [(x, y) | y <- [0 .. length (lines input)], x <- [0 .. length (head (lines input))]]
part2 input = sum $ mapMaybe (maybeGearRatio (lines input)) [(x, y) | y <- [0 .. length (lines input)], x <- [0 .. length (head (lines input))]]

isPartNumber :: [[Char]] -> (Int, Int) -> Maybe Int
isPartNumber input p@(x, y)
  | not $ isDigit (lookupChar input p) = Nothing
  | isDigit (lookupChar input (x - 1, y)) = Nothing
  | any scanAround [(input, x', y) | x' <- [x .. x + length number - 1]] = Just $ read $ traceShowId number
  | otherwise = Nothing
  where
    number = takeWhile isDigit $ drop x $ input !! y

maybeGearRatio :: [[Char]] -> (Int, Int) -> Maybe Int
maybeGearRatio input p@(x, y)
  | lookupChar input p /= '*' = Nothing
  | otherwise = calculateGearRatio adjacentNums
  where
    adjacentNums = traceShowId $ chunksOf 3 [captureNumber input (x', y') | y' <- [y - 1 .. y + 1], x' <- [x - 1 .. x + 1]]

calculateGearRatio :: [[Maybe Int]] -> Maybe Int
calculateGearRatio array
  | length fixed /= 2 = Nothing
  | otherwise = Just $ product fixed
  where
    firstRow = head array
    lastRow = last array
    firstRow' = if isJust (firstRow !! 1) then nub firstRow else firstRow
    lastRow' = if isJust (lastRow !! 1) then nub lastRow else lastRow
    fixed = catMaybes $ firstRow' ++ array !! 1 ++ lastRow'

captureNumber :: [[Char]] -> (Int, Int) -> Maybe Int
captureNumber array (x, y)
  | (not . isDigit) (lookupChar array (x, y)) = Nothing
  | otherwise = Just $ read $ reverse toLeft ++ toRight
  where
    toLeft = takeWhile isDigit $ reverse $ take x $ array !! y
    toRight = takeWhile isDigit $ drop x $ array !! y

isSymbol :: Char -> Bool
isSymbol c = not (c == '.' || isDigit c)

lookupChar :: [[Char]] -> (Int, Int) -> Char
lookupChar input (x, y)
  | x < 0 || y < 0 || y >= length input || x >= length (head input) = '.'
  | otherwise = input !! y !! x

scanAround :: ([[Char]], Int, Int) -> Bool
scanAround (input, x, y) = any isSymbol [lookupChar input (x', y') | y' <- [y - 1 .. y + 1], x' <- [x - 1 .. x + 1]]