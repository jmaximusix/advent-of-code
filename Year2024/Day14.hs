module Day14 (part1, part2) where

import Debug.Trace (traceShowId)
import Linear (V2 (..))
import MyLib (count, readNumbers)
import MyLib.GridV (VecPos)

part1, part2 :: [String] -> Int
part1 = countQs . map (move 100 . parseInput)
part2 i = traceShow (iterate move 1 $ map parseInput i) 9

countQs :: [VecPos] -> Int
countQs i =
  product $
    map
      (`count` i)
      [ \(V2 x y) -> (x > 0) && (y > 0),
        \(V2 x y) -> (x < 0) && (y > 0),
        \(V2 x y) -> (x < 0) && (y < 0),
        \(V2 x y) -> (x > 0) && (y < 0)
      ]

move :: Int -> Robot -> VecPos
move n (pos, vel) = (\(V2 a b) -> V2 ((a `mod` 101) - 50) ((b `mod` 103) - 51)) $ pos + ((n *) <$> vel)

-- move n (pos, vel) = (\(V2 a b) -> V2 ((a `mod` 11) - 5) ((b `mod` 7) - 3)) $ pos + ((n *) <$> vel)

type Robot = (VecPos, VecPos)

parseInput :: String -> Robot
parseInput input = (V2 a b, V2 c d)
  where
    [a, b, c, d] = readNumbers input