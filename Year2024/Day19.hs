module Day19 (part1, part2) where

import Algorithm.Search (dfs)
import Data.List (inits, tails)
import Data.List.Extra (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set

part1, part2 :: [String] -> Int
part1 input = length $ filter id $ map (possible patterns) towels
  where
    (patterns, towels) = parseInput input
part2 input = sum $ map (countWays patterns maxlen) towels
  where
    maxlen = Set.findMax . Set.map length $ patterns
    (patterns, towels) = parseInput input

possible :: Set.Set String -> String -> Bool
possible patterns s = isJust $ dfs (next patterns) (== "") s

countWays :: Set.Set String -> Int -> String -> Int
countWays patterns maxlen s = cache Map.! s
  where
    cache =
      foldl
        ( \c t ->
            foldl
              ( \c' i ->
                  if i `Set.member` patterns
                    then Map.insertWith (+) t (c Map.! drop (length i) t) c'
                    else Map.insertWith (+) t 0 c'
              )
              c
              (take maxlen $ tail $ inits t)
        )
        (Map.singleton "" 1)
        . tail
        . reverse
        . tails
        $ s

next :: Set.Set String -> String -> [String]
next patterns s = map ((`drop` s) . length) . filter (`Set.member` patterns) . tail . inits $ s

parseInput :: [String] -> (Set.Set String, [String])
parseInput input = (Set.fromList patterns', towels)
  where
    patterns' = splitOn ", " $ head patterns
    [patterns, towels] = splitOn [""] input