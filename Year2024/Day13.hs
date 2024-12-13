module Day13 (part1, part2) where

import Control.Lens ((^.))
import Data.List.Extra (chunksOf)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (second)
import Linear (M22, V2 (V2), det22, _x)
import MyLib (readNumbers, tup2)

part1, part2 :: [String] -> Int
part1 = sum . mapMaybe solveLes . parseInput
part2 = sum . mapMaybe (solveLes . second (fmap (+ 10000000000000))) . parseInput

solveLes :: (M22 Int, V2 Int) -> Maybe Int
solveLes (m@(V2 a b), t) =
  safeDiv (det22 (V2 a t)) (det22 m)
    >>= (\b' -> (\a' -> 3 * a' + b') <$> safeDiv ((t ^. _x) - b' * (b ^. _x)) (a ^. _x))
  where
    safeDiv e d
      | e `mod` d == 0 = Just $ e `div` d
      | otherwise = Nothing

parseInput :: [String] -> [(M22 Int, V2 Int)]
parseInput = map ((\[a, b, t] -> (V2 a b, t)) . map (uncurry V2 . tup2 . readNumbers) . take 3) . chunksOf 4