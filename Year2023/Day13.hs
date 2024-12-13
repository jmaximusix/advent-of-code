module Day13 (part1, part2) where

import Data.List.Extra (splitOn, transpose)
import Data.Maybe (fromJust, mapMaybe)
import MyLib.Geometry (Grid)

part1, part2 :: Grid Char -> Int
part1 = sum . map (checkAllSymmetries 0) . parseInput
part2 = sum . map (checkAllSymmetries 1) . parseInput

parseInput :: Grid Char -> [Grid Char]
parseInput = splitOn [""]

checkAllSymmetries :: Int -> Grid Char -> Int
checkAllSymmetries n g = maybe (fromJust horizontal) (100 *) vertical
  where
    (vertical, horizontal) = (verticalSymmetry n g, verticalSymmetry n (transpose g))

verticalSymmetry :: Int -> Grid Char -> Maybe Int
verticalSymmetry n g
  | bottom > top = (length g -) <$> bottom
  | otherwise = top
  where
    top = verticalSymmetryFromTop n g
    bottom = verticalSymmetryFromTop n (reverse g)

verticalSymmetryFromTop :: Int -> Grid Char -> Maybe Int
verticalSymmetryFromTop n g
  | null symmetries = Nothing
  | otherwise = Just $ last symmetries
  where
    symmetries = mapMaybe hasSymmetry [1 .. (length g `div` 2)]
    hasSymmetry i
      | differInExactlyN n (take i g) (reverse (take i (drop i g))) = Just i
      | otherwise = Nothing

differInExactlyN :: Int -> Grid Char -> Grid Char -> Bool
differInExactlyN n a b = length (filter id $ zipWith (/=) (concat a) (concat b)) == n