{-# LANGUAGE TupleSections #-}

module Day21 (part1, part2) where

import Algorithm.Search (bfs, dijkstraAssoc, dijkstraAssocM, pruning)
import Combinatorics (variate)
import Data.Bifunctor (Bifunctor (bimap), first)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow, traceShowId)
import Linear (V2 (V2))
import MyLib.GridV
import MyLib.Utils

part1, part2 :: [String] -> Int
part1 input = sum $ map (uncurry (*) . first (solve arrowkeys numpad 2) . parseInput) input
  where
    numpad = Map.fromList $ map swap [('7', V2 0 0), ('8', V2 1 0), ('9', V2 2 0), ('4', V2 0 1), ('5', V2 1 1), ('6', V2 2 1), ('1', V2 0 2), ('2', V2 1 2), ('3', V2 2 2), ('0', V2 1 3), ('A', V2 2 3)]
    arrowkeys = Map.fromList $ map swap [('^', V2 1 0), ('A', V2 2 0), ('<', V2 0 1), ('v', V2 1 1), ('>', V2 2 1)]
part2 input = sum solution2
  where
    solution2 = map (uncurry (*) . first (fst . robotifySeq2 numpad 100000) . parseInput) input
    solution = map (uncurry (*) . first (seqLen . robotifySeq numpad 2) . parseInput) input
    keylist = [('7', V2 0 0), ('8', V2 1 0), ('9', V2 2 0), ('4', V2 0 1), ('5', V2 1 1), ('6', V2 2 1), ('1', V2 0 2), ('2', V2 1 2), ('3', V2 2 2), ('0', V2 1 3), ('A', V2 2 3)]
    troubleshoot = filter (\s -> length s /= 6) $ zipWith (\(a, b) d -> if b == d then a ++ " top" else a ++ " passt nicht: " ++ show b ++ ", " ++ show d) correct incorrect
    correct = map (\x -> (x, solve arrowkeys numpad' 5 x)) $ variate 2 "0123456789A"
    incorrect = map (seqLen . robotifySeq numpad 5) $ variate 2 "0123456789A"
    numpad' = Map.fromList $ map swap keylist
    numpad = Map.fromList keylist
    arrowkeys = Map.fromList $ map swap [('^', V2 1 0), ('A', V2 2 0), ('<', V2 0 1), ('v', V2 1 1), ('>', V2 2 1)]

solveC :: Map.Map VecPos Char -> Map.Map VecPos Char -> Int -> Char -> Char -> Int
solveC apad numpad n r1c c = fst $ fromJust $ dijkstraAssoc (filter (not . wrongAnswer c . fst) . next apad numpad) (rightAnswer c) state0
  where
    r1loc = fst $ Map.findMin $ Map.filter (== r1c) numpad
    state0 = State r1loc 0 $ replicate n $ V2 2 0

-- v<<A>>^A<A>A<A>vAA^Av<AAA>^A
-- v<<A>>^A<A>AvA<^AA>A<vAAA>^A
-- v<<A>>^A<A>AvA<^AA>A<vAAA>^A

-- v<A<AA>>^A<AA>vA^Av<<A>>^AvA^Av<<A>>^AvA<A>^AA<A>Av<A<A>>^AAA<A>vA^A
-- <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
-- <vA<AA>>^AvAA<^A>Av<<A>>^AvA^A<vA>^Av<<A>^A>AAvA^Av<<A>A>^AAAvA<^A>A

parseInput :: String -> (String, Int)
parseInput s = ('A' : s, read $ init s)

printSeq :: Sequence -> String
printSeq (Seq xs) = concatMap printSeq xs
printSeq (Nt n c) = replicate n c
printSeq (Press n) = replicate n 'A'

seqLen :: Sequence -> Int
seqLen (Press n) = n
seqLen (Nt n _) = n
seqLen (Seq xs) = sum $ map seqLen xs

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

data Sequence
  = Seq [Sequence]
  | Nt Int Char
  | Press Int
  deriving (Show, Eq, Ord)

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

mapad :: Sequence -> Sequence
mapad p@(Press _) = p
mapad (Nt a 'v') = Seq [Seq [Nt 1 '<', Nt 1 'v'], Press a, Seq [Nt 1 '^', Nt 1 '>']]
mapad (Nt a '^') = Seq [Nt 1 '<', Press a, Nt 1 '>']
mapad (Nt a '>') = Seq [Nt 1 'v', Press a, Nt 1 '^']
mapad (Nt a '<') = Seq [Seq [Nt 1 'v', Nt 2 '<'], Press a, Seq [Nt 2 '>', Nt 1 '^']]
mapad (Seq [Nt a 'v', Nt b '>']) = Seq [Seq [Nt 1 '<', Nt 1 'v'], Press a, Nt 1 '>', Press b, Nt 1 '^']
mapad (Seq [Nt a '>', Nt b 'v']) = Seq [Nt 1 'v', Press a, Nt 1 '<', Press b, Seq [Nt 1 '^', Nt 1 '>']]
mapad (Seq [Nt a '>', Nt b '^']) = Seq [Nt 1 'v', Press a, Seq [Nt 1 '<', Nt 1 '^'], Press b, Nt 1 '>']
mapad (Seq [Nt a '^', Nt b '>']) = Seq [Nt 1 '<', Press a, Seq [Nt 1 'v', Nt 1 '>'], Press b, Nt 1 '^']
mapad (Seq [Nt a '<', Nt b 'v']) = Seq [Seq [Nt 1 'v', Nt 2 '<'], Press a, Nt 1 '>', Press b, Seq [Nt 1 '^', Nt 1 '>']]
mapad (Seq [Nt a '<', Nt b '^']) = Seq [Seq [Nt 1 'v', Nt 2 '<'], Press a, Seq [Nt 1 '>', Nt 1 '^'], Press b, Nt 1 '>']
mapad (Seq [Nt a '^', Nt b '<']) = Seq [Nt 1 '<', Press a, Seq [Nt 1 'v', Nt 1 '<'], Press b, Seq [Nt 2 '>', Nt 1 '^']]
mapad (Seq [Nt a 'v', Nt b '<']) = Seq [Seq [Nt 1 '<', Nt 1 'v'], Press a, Nt 1 '<', Press b, Seq [Nt 2 '>', Nt 1 '^']]
mapad (Seq xs) = Seq $ map mapad xs

mapad2 :: String -> (Int, [String])
mapad2 "v" = (4, ["<v", "^>"])
mapad2 "^" = (2, ["<", ">"])
mapad2 ">" = (2, ["v", "^"])
mapad2 "<" = (6, ["v<", ">^"])
mapad2 "v>" = (4, ["<v", ">", "^"])
mapad2 ">v" = (4, ["v", "<", "^>"])
mapad2 ">^" = (4, ["v", "<^", ">"])
mapad2 "^>" = (4, ["<", "v>", "^"])
mapad2 "<v" = (6, ["v<", ">", "^>"])
mapad2 "<^" = (6, ["v<", ">^", ">"])
mapad2 "^<" = (6, ["<", "v<", ">^"])
mapad2 "v<" = (6, ["<v", "<", ">^"])

type Sequence2 = (Int, Map.Map [Char] Int)

moves2 :: VecPos -> VecPos -> (Int, String)
moves2 v1@(V2 x1 y1) v2@(V2 x2 y2)
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

moves :: VecPos -> VecPos -> Sequence
moves v1@(V2 x1 y1) v2@(V2 x2 y2)
  | x2 == 0 && y1 == 3 = Seq [Nt ay '^', Nt ax '<']
  | x1 == 0 && y2 == 3 = Seq [Nt ay '>', Nt ax 'v']
  | otherwise = case dv of
      V2 0 1 -> Nt ay 'v'
      V2 0 (-1) -> Nt ay '^'
      V2 1 0 -> Nt ax '>'
      V2 (-1) 0 -> Nt ax '<'
      V2 1 1 -> Seq [Nt ax 'v', Nt ay '>']
      V2 1 (-1) -> Seq [Nt ax '^', Nt ay '>']
      V2 (-1) 1 -> Seq [Nt ax '<', Nt ay 'v']
      V2 (-1) (-1) -> Seq [Nt ax '<', Nt ay '^']
  where
    (V2 x y) = v2 - v1
    dv = V2 (signum x) (signum y)
    ax = abs x
    ay = abs y

robotifySeq2 :: Map.Map Char VecPos -> Int -> [Char] -> Sequence2
robotifySeq2 numpad 0 [a, b] = bimap (+ 1) (`Map.singleton` 1) (moves2 (numpad Map.! a) (numpad Map.! b))
robotifySeq2 numpad 0 (a : b : rest) = (\[(a', b'), (c, d)] -> (a' + c, Map.unionWith (+) b' d)) bots
  where
    bots = [robotifySeq2 numpad 0 [a, b], robotifySeq2 numpad 0 (b : rest)]
robotifySeq2 np n cs = (presses, moves')
  where
    -- (c, d) = bimap ((+ a) . sum) (countAll . concat) $ unzip $ map mapad2 b
    (presses, moves') = Map.foldlWithKey update (a, Map.empty) b
    update (acc, ms) move c = (acc + c * dacc, foldl (update2 c) ms newmoves)
      where
        (dacc, newmoves) = mapad2 move
    update2 c ms newm = Map.insertWith (+) newm c ms
    (a, b) = robotifySeq2 np (n - 1) cs

robotifySeq :: Map.Map Char VecPos -> Int -> [Char] -> Sequence
robotifySeq numpad 0 [a, b] = Seq [moves (numpad Map.! a) (numpad Map.! b), Press 1]
robotifySeq numpad 0 (a : b : rest) = Seq [robotifySeq numpad 0 [a, b], robotifySeq numpad 0 (b : rest)]
robotifySeq np n cs = mapad $ robotifySeq np (n - 1) cs

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
