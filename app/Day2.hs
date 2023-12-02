module Day2 (part1, part2) where

import Data.Char (toUpper)
import Data.List (uncons)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as Map (Map, empty, fromList, insertWith, toList, (!))
import Data.Maybe (fromJust)

data Color = Red | Green | Blue deriving (Show, Eq, Ord, Read)

part1, part2 :: String -> Int
part1 input = sum $ map fst $ filter (compareCubes (Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]) . snd) $ map parseInput (lines input)
part2 input = sum $ map (setPower . snd . parseInput) (lines input)

compareCubes :: Map.Map Color Int -> Map.Map Color Int -> Bool
compareCubes a b = all (\color -> b Map.! color <= a Map.! color) [Red, Green, Blue]

setPower :: Map.Map Color Int -> Int
setPower cubes = product $ map snd $ Map.toList cubes

parseInput :: String -> (Int, Map.Map Color Int)
parseInput input = (game_id, foldl update Map.empty cubes)
  where
    update m [a, b : bs] = Map.insertWith max (read (toUpper b : init bs)) (read a) m
    pairs = chunksOf 2 $ splitOn " " (input ++ ",")
    (game, cubes) = fromJust $ uncons pairs
    game_id = read $ init $ last game