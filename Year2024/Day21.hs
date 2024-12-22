{-# LANGUAGE TupleSections #-}

module Day21 (part1, part2) where

import Algorithm.Search (bfs, dijkstraAssoc, dijkstraAssocM, pruning)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import Linear (V2 (V2))
import MyLib.GridV
import MyLib.Utils

part1, part2 :: [String] -> Int
part1 input = sum $ map (uncurry (*) . first (solve arrowkeys numpad 2) . parseInput) input
  where
    numpad = Map.fromList $ map swap [('7', V2 0 0), ('8', V2 1 0), ('9', V2 2 0), ('4', V2 0 1), ('5', V2 1 1), ('6', V2 2 1), ('1', V2 0 2), ('2', V2 1 2), ('3', V2 2 2), ('0', V2 1 3), ('A', V2 2 3)]
    arrowkeys = Map.fromList $ map swap [('^', V2 1 0), ('A', V2 2 0), ('<', V2 0 1), ('v', V2 1 1), ('>', V2 2 1)]
part2 input = traceShow (moves (V2 0 0) (V2 2 3)) 0

solveC :: Map.Map VecPos Char -> Map.Map VecPos Char -> Int -> Char -> Char -> Int
solveC apad numpad n r1c c = fst $ fromJust $ dijkstraAssoc (filter (not . wrongAnswer c . fst) . next apad numpad) (rightAnswer c) state0
  where
    r1loc = fst $ Map.findMin $ Map.filter (== r1c) numpad
    state0 = State r1loc 0 $ replicate n $ V2 2 0

parseInput :: String -> (String, Int)
parseInput s = ('A' : s, read $ init s)

solve :: Map.Map VecPos Char -> Map.Map VecPos Char -> Int -> String -> Int
solve apad numpad n [a, b] = solveC apad numpad n a b
solve apad numpad n (a : b : rest) = solveC apad numpad n a b + solve apad numpad n (b : rest)

data State
  = State
      { robot1 :: VecPos,
        index :: Int,
        betweens :: [VecPos]
      }
  | Output Char
  deriving (Show, Eq, Ord)

data Tree = PressNAfter Int [Tree] | Leaf Int Char

wrongAnswer :: Char -> State -> Bool
wrongAnswer c (Output o) = o /= c
wrongAnswer _ _ = False

rightAnswer :: Char -> State -> Bool
rightAnswer c (Output o) = c == o
rightAnswer _ _ = False

next :: Map.Map VecPos Char -> Map.Map VecPos Char -> State -> [(State, Int)]
next apad numpad state = map (,1) $ mapMaybe (simulateInput apad numpad state) "^v<>A"

simulateInput :: Map.Map VecPos Char -> Map.Map VecPos Char -> State -> Char -> Maybe State
simulateInput apad numpad state@(State r1 i rs) c
  | c == 'A' = pressbetween apad numpad state {index = i + 1}
  | isNothing mr' = Nothing
  | otherwise = Just $ State r1 0 (replace i (r + toDir c) rs)
  where
    mr' = Map.lookup (r + toDir c) apad
    r = rs !! i

apad :: [(Int, Char)] -> [[(Int, Char)]]
apad [(_, 'v')] = [[(1, 'v'), (1, '<')], [(1, '>'), (1, '^')]]
apad [(_, '^')] = [[(1, '<')], [(1, '>')]]
apad [(_, '>')] = [[(1, 'v')], [(1, '^')]]
apad [(_, '<')] = [[(1, 'v'), (2, '<')], [(2, '>'), (1, '^')]]
apad [(_, '>'), (_, 'v')] = [[(1, 'v')], [(1, '<')], [(1, '>'), (1, '^')]]
apad [(_, '>'), (_, '^')] = [[(1, '<')], [(1, '>'), (1, 'v')], [(1, '^')]]
apad [(_, '<'), (_, 'v')] = [[(1, 'v'), (2, '<')], [(1, '>')], [(1, '>'), (1, '^')]]
apad [(_, '<'), (_, '^')] = [[(1, 'v'), (2, '<')], [(1, '>'), (1, '^')], [(1, '>')]]
apad [(_, '^'), (_, '<')] = [[(1, '<')], [(1, 'v'), (1, '<')], [(2, '>'), (1, '^')]]

moves :: VecPos -> VecPos -> [(Int, Char)]
moves v1@(V2 _ y1) v2@(V2 x2 _)
  | x2 == 0 && y1 == 3 = [(ay, '^'), (ax, '<')]
  | otherwise = case dv of
      V2 0 1 -> [(ay, 'v')]
      V2 0 (-1) -> [(ay, '^')]
      V2 1 0 -> [(ax, '>')]
      V2 (-1) 0 -> [(ax, '<')]
      V2 1 1 -> [(ax, '>'), (ay, 'v')]
      V2 1 (-1) -> [(ax, '>'), (ay, '^')]
      V2 (-1) 1 -> [(ax, '<'), (ay, 'v')]
      V2 (-1) (-1) -> [(ax, '<'), (ay, '^')]
  where
    (V2 x y) = v2 - v1
    dv = V2 (signum x) (signum y)
    ax = abs x
    ay = abs y

pressbetween :: Map.Map VecPos Char -> Map.Map VecPos Char -> State -> Maybe State
-- pressbetween _ _ (State _ i rs) | traceShow (i, rs) False = undefined
pressbetween apad numpad state@(State r1 i rs)
  | i == length rs && c == 'A' = Just $ Output (numpad Map.! r1)
  | i == length rs && isNothing mr1' = Nothing
  | i == length rs = Just $ State (r1 + toDir c) 0 rs
  | c == 'A' = pressbetween apad numpad state {index = i + 1}
  | isNothing mr' = Nothing
  | otherwise = Just $ State r1 0 (replace i (r + toDir c) rs)
  where
    mr' = Map.lookup (r + toDir c) apad
    mr1' = Map.lookup (r1 + toDir c) numpad
    r = rs !! i
    c = apad Map.! (rs !! (i - 1))

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

-- A A A A A A A A X
-- moving up 1
-- A A A A A A A U X
-- A A A A A A L
