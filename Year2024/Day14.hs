module Day14 (part1, part2) where

import Control.Lens (over, (^.))
import Data.List.Extra (maximumOn)
import Data.Maybe (mapMaybe)
import Linear (V2 (..), (*^), _x, _y)
import MyLib.GridV (VecPos)
import MyLib.Utils (countAll, readNumbers)

part1, part2 :: [String] -> Int
part1 = product . countAll . mapMaybe (quadrant . move 100 . parseInput)
-- 51 is s in the equation s*101 + t*103 = 1, solved with extended Euclidean algorithm
part2 input = ((b - a) * 51 * 101 + a) `mod` (101 * 103)
  where
    a = cluster _x 101
    b = cluster _y 103
    robots = map parseInput input
    cluster axis len =
      fst
        . maximumOn snd
        . zip [1 ..]
        . map (maximum . countAll . \t -> map ((^. axis) . move t) robots)
        $ [1 .. len]

quadrant :: VecPos -> Maybe Int
quadrant (V2 x y)
  | x > 50 && y > 51 = Just 1
  | x < 50 && y > 51 = Just 2
  | x < 50 && y < 51 = Just 3
  | x > 50 && y < 51 = Just 4
  | otherwise = Nothing

move :: Int -> (VecPos, VecPos) -> VecPos
move n (pos, vel) = over _x (`mod` 101) . over _y (`mod` 103) $ pos + (n *^ vel)

parseInput :: String -> (VecPos, VecPos)
parseInput = (\[px, py, vx, vy] -> (V2 px py, V2 vx vy)) . readNumbers