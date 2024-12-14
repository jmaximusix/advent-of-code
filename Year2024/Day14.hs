module Day14 (part1, part2) where

import Control.Lens ((^.))
import Data.List.Extra (maximumOn)
import Data.Maybe (mapMaybe)
import Linear (V2 (..), _x, _y)
import MyLib.GridV (VecPos)
import MyLib.Utils (countAll, readNumbers)

type Robot = (VecPos, VecPos)

part1, part2 :: [String] -> Int
part1 = product . countAll . mapMaybe (quadrant . move 100 . parseInput)
-- (-50) is t in the equation s*101 + t*103 = 1, solved with extended Euclidean algorithm
part2 = (\(a, b) -> ((a - b) * (-50) * 103 + b) `mod` (101 * 103)) . axisClusters . map parseInput

quadrant :: VecPos -> Maybe Int
quadrant (V2 x y)
  | x > 50 && y > 51 = Just 1
  | x < 50 && y > 51 = Just 2
  | x < 50 && y < 51 = Just 3
  | x > 50 && y < 51 = Just 4
  | otherwise = Nothing

axisClusters :: [Robot] -> (Int, Int)
axisClusters robots = (cluster _x 101, cluster _y 103)
  where
    cluster xory len =
      fst
        . maximumOn snd
        . zip [1 ..]
        . map (maximum . countAll . \x -> map ((^. xory) . move x) robots)
        $ [1 .. len]

move :: Int -> Robot -> VecPos
move n (pos, vel) = (\(V2 a b) -> V2 (a `mod` 101) (b `mod` 103)) $ pos + ((n *) <$> vel)

parseInput :: String -> Robot
parseInput input = (V2 a b, V2 c d)
  where
    [a, b, c, d] = readNumbers input