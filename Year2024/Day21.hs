{-# LANGUAGE DataKinds #-}

module Day21 (part1, part2) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.IsList (fromList)
import Linear (V2 (V2), dot, (!*))
import Linear.V (V, fromVector)
import MyLib.Utils (matrixPower)

type V13 = V 13

type Matrix13 a = V13 (V13 a)

toV13 :: [a] -> V13 a
toV13 = fromJust . fromVector . fromList

-- since this solution runs in O(n log n) I'm using Integers just for fun
part1, part2 :: [String] -> Integer
part1 = solve 2
part2 = solve 25

solve :: Int -> [String] -> Integer
solve n = sum . map (uncurry (*) . (\s -> (chainNRobots n s, read $ init s)))

chainNRobots :: Int -> [Char] -> Integer
chainNRobots n = fst . foldl (\(count, l) l' -> (count + moves l l', l')) (0, 'A')
  where
    transitions = matrixPower arrowmatrix n
    moves l l' = (transitions !* (keypad l l')) `dot` (toV13 (1 : replicate 12 0))

keymap :: Map.Map Char (V2 Int)
keymap =
  Map.fromList
    [ ('1', V2 0 0),
      ('2', V2 1 0),
      ('3', V2 2 0),
      ('4', V2 0 1),
      ('5', V2 1 1),
      ('6', V2 2 1),
      ('7', V2 0 2),
      ('8', V2 1 2),
      ('9', V2 2 2),
      ('A', V2 1 3),
      ('0', V2 0 3)
    ]

keypad :: Char -> Char -> V13 Integer
keypad c1 c2
  | x1 == 0 && y2 == 3 = keyV 10
  | x2 == 0 && y1 == 3 = keyV 12
  | otherwise = case sv of
      V2 0 (-1) -> keyV 1
      V2 1 0 -> keyV 2
      V2 0 1 -> keyV 3
      V2 (-1) 0 -> keyV 4
      V2 1 (-1) -> keyV 5
      V2 1 1 -> keyV 6
      V2 (-1) 1 -> keyV 7
      V2 (-1) (-1) -> keyV 8
  where
    v1@(V2 x1 y1) = keymap Map.! c1
    v2@(V2 x2 y2) = keymap Map.! c2
    (V2 x y) = v2 - v1
    sv = V2 (signum x) (signum y)
    d = abs x + abs y
    keyV i =
      toV13 $
        (fromIntegral d + 1 : replicate (i - 1) 0)
          ++ (1 : replicate (12 - i) 0)

arrowmatrix :: Matrix13 Integer
arrowmatrix =
  toV13 . map toV13 $
    [ [1, 2, 2, 4, 6, 4, 4, 6, 6, 4, 4, 6, 6],
      [0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0],
      [0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],
      [0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1],
      [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0],
      [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
      [0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ]
