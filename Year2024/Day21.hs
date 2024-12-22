module Day21 (part1, part2) where

import Data.Bifunctor (bimap, first)
import qualified Data.Map.Strict as Map
import Linear (V2 (V2))

-- since my solution runs in linear time I'm using Integers just for fun and can calculate up to 10k robots in <1s
part1, part2 :: [String] -> Integer
part1 = solve 2
part2 = solve 25

parseInput :: String -> (String, Integer)
parseInput s = ('A' : s, read $ init s)

solve :: Int -> [String] -> Integer
solve n = sum . map (uncurry (*) . first (fst . chainNRobots n . map (numpad Map.!)) . parseInput)
  where
    numpad =
      Map.fromList
        [ ('7', V2 0 0),
          ('8', V2 1 0),
          ('9', V2 2 0),
          ('4', V2 0 1),
          ('5', V2 1 1),
          ('6', V2 2 1),
          ('1', V2 0 2),
          ('2', V2 1 2),
          ('3', V2 2 2),
          ('0', V2 1 3),
          ('A', V2 2 3)
        ]

chainNRobots :: Int -> [V2 Integer] -> (Integer, Map.Map [Char] Integer)
chainNRobots 0 [a, b] = bimap (+ 1) (`Map.singleton` 1) (keypad a b)
chainNRobots 0 (a : b : r) = bimap sum (Map.unionsWith (+)) $ unzip [chainNRobots 0 [a, b], chainNRobots 0 (b : r)]
chainNRobots n keysToPress = Map.foldlWithKey update (total, Map.empty) moves
  where
    (total, moves) = chainNRobots (n - 1) keysToPress
    update (acc, movemap) move c =
      let (dacc, newmoves) = arrowpad move
       in (acc + c * dacc, foldl (\mm newm -> Map.insertWith (+) newm c mm) movemap newmoves)

arrowpad :: String -> (Integer, [String])
arrowpad "v" = (4, ["<v", "^>"])
arrowpad "^" = (2, ["<", ">"])
arrowpad ">" = (2, ["v", "^"])
arrowpad "<" = (6, ["v<", ">^"])
arrowpad "v>" = (4, ["<v", ">", "^"])
arrowpad ">v" = (4, ["v", "<", "^>"])
arrowpad ">^" = (4, ["v", "<^", ">"])
arrowpad "^>" = (4, ["<", "v>", "^"])
arrowpad "<v" = (6, ["v<", ">", "^>"])
arrowpad "<^" = (6, ["v<", ">^", ">"])
arrowpad "^<" = (6, ["<", "v<", ">^"])
arrowpad "v<" = (6, ["<v", "<", ">^"])

keypad :: V2 Integer -> V2 Integer -> (Integer, String)
keypad v1@(V2 x1 y1) v2@(V2 x2 y2)
  | x2 == 0 && y1 == 3 = (d, "^<")
  | x1 == 0 && y2 == 3 = (d, ">v")
  | otherwise = case sv of
      V2 0 1 -> (dy, "v")
      V2 0 (-1) -> (dy, "^")
      V2 1 0 -> (dx, ">")
      V2 (-1) 0 -> (dx, "<")
      V2 1 1 -> (d, "v>")
      V2 1 (-1) -> (d, "^>")
      V2 (-1) 1 -> (d, "<v")
      V2 (-1) (-1) -> (d, "<^")
  where
    (V2 x y) = v2 - v1
    sv = V2 (signum x) (signum y)
    dx = abs x
    dy = abs y
    d = dx + dy
