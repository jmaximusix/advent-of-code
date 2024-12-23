module Day02 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.List.Extra (chunksOf)
import qualified Data.Map as Map (Map, empty, fromList, insertWith, toList, (!))
import MyLib (capitalise)

data Color = Red | Green | Blue deriving (Show, Eq, Ord, Read)

part1, part2 :: [String] -> Int
part1 =
  sum
    . map fst
    . filter (hasMoreCubes (Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]) . snd)
    . map parseInput
part2 = sum . map (setPower . snd . parseInput)

hasMoreCubes :: Map.Map Color Int -> Map.Map Color Int -> Bool
hasMoreCubes a b = all (\color -> a Map.! color >= b Map.! color) [Red, Green, Blue]

setPower :: Map.Map Color Int -> Int
setPower = product . map snd . Map.toList

parseInput :: String -> (Int, Map.Map Color Int)
parseInput input = (game_id, foldl update Map.empty cubes)
  where
    update m (count, color) = Map.insertWith max color count m
    pairs = map (\[a, b] -> (a, init b)) $ chunksOf 2 $ words (input ++ ",")
    cubes = map (bimap read (read . capitalise)) (tail pairs)
    game_id = (read . snd . head) pairs