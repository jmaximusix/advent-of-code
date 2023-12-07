module Year2023.Day3 (part1, part2) where

import Data.Char (isDigit)
import Data.List (nub)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)

type Grid = [[Char]]

type Pos = (Int, Int)

part1, part2 :: Grid -> Int
part1 grid = (sum . mapMaybe (maybePartNumber grid) . posList) grid
part2 grid = (sum . mapMaybe (maybeGearRatio grid) . posList) grid

posList :: Grid -> [Pos]
posList grid = [(x, y) | y <- [0 .. length grid], x <- [0 .. length (head grid)]]

maybePartNumber :: Grid -> Pos -> Maybe Int
maybePartNumber grid pos@(x, y)
  | (not . isDigit) (getSym grid pos) || isDigit (getSym grid (x - 1, y)) = Nothing
  | any isSymbol numNeighbors = number
  | otherwise = Nothing
  where
    numNeighbors = map (getSym grid) $ concat [neighbors (x', y) | x' <- [x .. x + len - 1]]
    number = captureNumber grid pos
    len = (length . show . fromJust) number

maybeGearRatio :: Grid -> Pos -> Maybe Int
maybeGearRatio grid pos
  | getSym grid pos /= '*' = Nothing
  | otherwise = calculateGearRatio adjacentNums
  where
    adjacentNums = map (captureNumber grid) (neighbors pos)

calculateGearRatio :: [Maybe Int] -> Maybe Int
calculateGearRatio array
  | length gears /= 2 = Nothing
  | otherwise = Just $ product gears
  where
    smartnub r = if isJust (r !! 1) then nub r else r
    gears = concatMap (catMaybes . smartnub) $ chunksOf 3 array

captureNumber :: Grid -> Pos -> Maybe Int
captureNumber array (x, y)
  | (not . isDigit) (getSym array (x, y)) = Nothing
  | otherwise = (Just . read) $ reverse toLeft ++ toRight
  where
    toLeft = takeWhile isDigit $ reverse $ take x $ array !! y
    toRight = takeWhile isDigit $ drop x $ array !! y

isSymbol :: Char -> Bool
isSymbol c = not (c == '.' || isDigit c)

getSym :: Grid -> Pos -> Char
getSym g (x, y)
  | x < 0 || y < 0 || y >= length g || x >= length (head g) = '.'
  | otherwise = g !! y !! x

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x', y') | y' <- [y - 1 .. y + 1], x' <- [x - 1 .. x + 1]]
