module Day03 (part1, part2) where

import Data.Char (isDigit)
import Data.List (nub)
import Data.List.Extra (chunksOf)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import MyLib.Geometry (Grid, Pos, getGridElementWithDefault, neighborsOct, pointList)

part1, part2 :: Grid Char -> Int
part1 grid = (sum . mapMaybe (maybePartNumber grid) . pointList) grid
part2 grid = (sum . mapMaybe (maybeGearRatio grid) . pointList) grid

maybePartNumber :: Grid Char -> Pos -> Maybe Int
maybePartNumber grid pos@(x, y)
  | (not . isDigit) (getSym grid pos) || isDigit (getSym grid (x - 1, y)) = Nothing
  | any (\c -> c == '.' || isDigit c) numNeighbors = number
  | otherwise = Nothing
  where
    numNeighbors = map (getSym grid) $ concat [neighborsOct (x', y) | x' <- [x .. x + len - 1]]
    number = captureNumber grid pos
    len = (length . show . fromJust) number

maybeGearRatio :: Grid Char -> Pos -> Maybe Int
maybeGearRatio grid pos
  | getSym grid pos /= '*' = Nothing
  | otherwise = calculateGearRatio adjacentNums
  where
    adjacentNums = map (captureNumber grid) (neighborsOct pos)

calculateGearRatio :: [Maybe Int] -> Maybe Int
calculateGearRatio array
  | length gears /= 2 = Nothing
  | otherwise = Just $ product gears
  where
    smartnub r = if isJust (r !! 1) then nub r else r
    gears = concatMap (catMaybes . smartnub) $ chunksOf 3 array

captureNumber :: Grid Char -> Pos -> Maybe Int
captureNumber array (x, y)
  | (not . isDigit) (getSym array (x, y)) = Nothing
  | otherwise = (Just . read) $ reverse toLeft ++ toRight
  where
    toLeft = takeWhile isDigit $ reverse $ take x $ array !! y
    toRight = takeWhile isDigit $ drop x $ array !! y

getSym :: Grid Char -> Pos -> Char
getSym = getGridElementWithDefault '.'
