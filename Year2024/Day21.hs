module Day21 (part1, part2) where

import Algorithm.Search (bfs, dijkstraAssoc, pruning)
import Data.Bifunctor (first)
import Data.Foldable (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import Linear (V2 (V2))
import MyLib.GridV
import MyLib.Utils

part1, part2 :: [String] -> Int
part1 input = sum $ map (uncurry (*) . first (solve arrowkeys numpad) . parseInput) input
  where
    state0 = State (V2 2 3) (V2 2 0) (V2 2 0)
    numpad = Map.fromList $ map swap [('7', V2 0 0), ('8', V2 1 0), ('9', V2 2 0), ('4', V2 0 1), ('5', V2 1 1), ('6', V2 2 1), ('1', V2 0 2), ('2', V2 1 2), ('3', V2 2 2), ('0', V2 1 3), ('A', V2 2 3)]
    arrowkeys = Map.fromList $ map swap [('^', V2 1 0), ('A', V2 2 0), ('<', V2 0 1), ('v', V2 1 1), ('>', V2 2 1)]
part2 input = undefined

solveC :: Map.Map VecPos Char -> Map.Map VecPos Char -> Char -> Char -> Int
solveC apad numpad r1c c = length $ fromJust $ bfs (next apad numpad `pruning` wrongAnswer c) (rightAnswer c) state0
  where
    r1loc = fst $ Map.findMin $ Map.filter (== r1c) numpad
    state0 = State r1loc (V2 2 0) (V2 2 0)

parseInput :: String -> (String, Int)
parseInput s = ('A' : s, read $ init s)

solve :: Map.Map VecPos Char -> Map.Map VecPos Char -> String -> Int
solve apad numpad [a, b] = solveC apad numpad a b
solve apad numpad (a : b : rest) = solveC apad numpad a b + solve apad numpad (b : rest)

data State
  = State
      { robot1 :: VecPos,
        robot2 :: VecPos,
        robot3 :: VecPos
      }
  | Output Char
  deriving (Show, Eq, Ord)

wrongAnswer :: Char -> State -> Bool
wrongAnswer c (Output o) = o /= c
wrongAnswer _ _ = False

rightAnswer :: Char -> State -> Bool
rightAnswer c (Output o) = c == o
rightAnswer _ _ = False

next :: Map.Map VecPos Char -> Map.Map VecPos Char -> State -> [State]
next apad numpad state = mapMaybe (simulateInput apad numpad state) "^v<>A"

simulateInput :: Map.Map VecPos Char -> Map.Map VecPos Char -> State -> Char -> Maybe State
simulateInput apad numpad state@(State r1 r2 r3) c
  | c == 'A' = press3 apad numpad state
  | isNothing mr3' = Nothing
  | otherwise = Just $ State r1 r2 (r3 + toDir c)
  where
    mr3' = Map.lookup (r3 + toDir c) apad

press3 :: Map.Map VecPos Char -> Map.Map VecPos Char -> State -> Maybe State
press3 apad numpad state@(State r1 r2 r3)
  | c == 'A' = press2 apad numpad state
  | isNothing mr2' = Nothing
  | otherwise = Just $ State r1 (r2 + toDir c) r3
  where
    mr2' = Map.lookup (r2 + toDir c) apad
    c = apad Map.! r3

press2 :: Map.Map VecPos Char -> Map.Map VecPos Char -> State -> Maybe State
press2 apad numpad state@(State r1 r2 r3)
  | c == 'A' = Just $ Output (numpad Map.! r1)
  | isNothing mr1' = Nothing
  | otherwise = Just $ State (r1 + toDir c) r2 r3
  where
    mr1' = Map.lookup (r1 + toDir c) numpad
    c = apad Map.! r2

toDir :: Char -> Direction
toDir '^' = dir N
toDir 'v' = dir S
toDir '<' = dir W
toDir '>' = dir E

-- keypadNeighbors :: Map.Map Char (V2 Int) -> Char -> [Char]
-- keypadNeighbors pad c = Map.keys $ Map.filter (\p' -> tcabDistV p p' == 1) pad
--   where
--     p = pad Map.! c

-- transitionCostOnOuter :: Map.Map Char (V2 Int) -> Char -> Char -> Int
-- transitionCostOnOuter arrows pos target = tcabDistV (arrows Map.! pos) (arrows Map.! target)

-- transitionCostOnSecond :: Map.Map Char (V2 Int) -> Char -> Char -> Char -> Int
-- transitionCostOnSecond arrows r3 r2 target = fst $ fromJust $ dijkstraAssoc (map (\c -> (c, transitionCostOnOuter arrows r3 c)) . keypadNeighbors arrows) (== target) r2

-- nextOnSecond :: Char -> ((Char, Char) Bool)-> [(((Char, Char), Bool), Int)]
-- nextOnSecond otw ((r3, r2),visitedotw)

-- translateArrowMoveToArrow:: Map.Map Char (V2 Int) -> Char -> Char -
-- translateArrowMoveToArrow pad p1 p2

-- transitionCostOnInner