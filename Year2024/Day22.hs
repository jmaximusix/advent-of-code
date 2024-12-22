module Day22 (part1, part2) where

import Data.Bits (xor)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map.Strict as Map

part1, part2 :: [String] -> Int
part1 = sum . map ((!! 2000) . iterate next . read)
part2 =
  maximum
    . foldl (\m (i, ps) -> Map.insertWith (+) ps i m) Map.empty
    . concatMap ((\as -> nubOrdOn snd $ map (cumdiffs as) [0 .. (length as - 4)]) . diffs 2000 . read)

next :: Int -> Int
next = next' (* 2048) . next' (`div` 32) . next' (* 64)
  where
    next' f a = (a `xor` f a) `mod` 16777216

diffs :: Int -> Int -> [(Int, Int)]
diffs n i = zipWith (\a b -> (b `mod` 10, b `mod` 10 - a `mod` 10)) (iterate next i) (tail . take n . iterate next $ i)

cumdiffs :: [(Int, Int)] -> Int -> (Int, [Int])
cumdiffs ds i = (\[(_, a), (_, b), (_, c), (i', d)] -> (i', [a, b, c, d])) . take 4 $ drop i ds