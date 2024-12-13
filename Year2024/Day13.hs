module Day13 (part1, part2) where

import Data.List.Extra (chunksOf, splitOn)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (third3)
import Linear.V2 (V2 (V2))

part1, part2 :: [String] -> Int
part1 = sum . mapMaybe solve . parseInput
part2 = sum . mapMaybe (solve . third3 (fmap (+ 10000000000000))) . parseInput

solve :: (V2 Int, V2 Int, V2 Int) -> Maybe Int
solve (V2 ax ay, V2 bx by, V2 tx ty)
  | enumerator `mod` denominator /= 0 = Nothing
  | aenum `mod` adenom /= 0 = Nothing
  | otherwise = Just $ b + 3 * aenum `div` adenom
  where
    enumerator = tx * ay - ty * ax
    denominator = bx * ay - ax * by
    b = enumerator `div` denominator
    aenum = tx - b * bx
    adenom = ax

saveDiv :: Int -> Int -> Maybe Int
saveDiv a b
  | a `mod` b == 0 = Just $ a `div` b
  | otherwise = Nothing

parseInput :: [String] -> [(V2 Int, V2 Int, V2 Int)]
parseInput lines' = map parseMachine machines
  where
    parseMachine [a, b, c] = (parseButton a 2 '+', parseButton b 2 '+', parseButton c 1 '=')
    parseButton s n delim = V2 (read $ init x') (read y')
      where
        [x', y'] = map ((!! 1) . splitOn [delim]) $ drop n $ words s
    machines = map (take 3) $ chunksOf 4 lines'