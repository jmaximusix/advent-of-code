module Day19 (part1, part2) where

import Algorithm.Search (dfs)
import Data.List (inits, tails)
import Data.List.Extra (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set

part1, part2 :: [String] -> Int
part1 input = length $ filter id $ map (possible patterns) towels
  where
    (patterns, towels) = parseInput input
part2 input = sum $ map (snd . countWays patterns (Map.singleton "" 1)) towels
  where
    (patterns, towels) = parseInput input

possible :: Set.Set String -> String -> Bool
possible patterns s = isJust $ dfs (next patterns) (== "") s

countWays :: Set.Set String -> Map.Map String Int -> String -> (Map.Map String Int, Int)
countWays patterns cache s = case Map.lookup s cache of
  Just x -> (cache, x)
  Nothing -> if cache' == cache then (cache, 0) else countWays patterns cache' s
    where
      soos = tail (reverse (tails s))
      cache' =
        foldl
          ( \c t ->
              foldl
                ( \c' i ->
                    if i `Set.member` patterns
                      then Map.insertWith (+) t (fromMaybe 0 $ Map.lookup (rest i t) c) c'
                      else Map.insertWith (+) t 0 c'
                )
                c
                (take 8 $ tail $ inits t)
          )
          cache
          soos
      rest i' = drop (length i')

next :: Set.Set String -> String -> [String]
next patterns s = map (`drop` s) soos
  where
    soos = map length $ filter (`Set.member` patterns) $ tail $ inits s

parseInput :: [String] -> (Set.Set String, [String])
parseInput input = (Set.fromList patterns', towels)
  where
    patterns' = splitOn ", " $ head patterns
    [patterns, towels] = splitOn [""] input