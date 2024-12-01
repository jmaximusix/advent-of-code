module Day21 (solution21) where

import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, (!))

solution21 :: IO ()
solution21 = do
  mylines <- lines <$> readFile "input21"
  let tree = buildTree "root" $ foldl (\m l -> uncurry Map.insert (parseLine l) m) Map.empty mylines
  print $ evalTree tree
  print $ solveForHumn tree

data MonkeyTree = Full MonkeyTree Char MonkeyTree | Half Int MonkeyTree | Leaf String Int deriving (Show)

data Job = Operation String Char String | Value Int deriving (Show)

solveForHumn :: MonkeyTree -> Int
solveForHumn (Full a _ b)
  | containsHumn a = solveForHumn (Half (evalTree b) a)
  | otherwise = solveForHumn (Half (evalTree a) b)
solveForHumn (Half n (Leaf "humn" _)) = n
solveForHumn (Half n (Full a op b)) = solveForHumn (Half n' humanbranch)
  where
    humanbranch = if containsHumn a then a else b
    nonhuman = if containsHumn a then b else a
    n' = case op of
      '+' -> n - evalTree nonhuman
      '-' -> if containsHumn a then n + evalTree b else evalTree a - n
      '*' -> n `div` evalTree nonhuman
      '/' -> if containsHumn a then n * evalTree b else evalTree a `div` n

containsHumn :: MonkeyTree -> Bool
containsHumn (Leaf "humn" _) = True
containsHumn (Leaf _ _) = False
containsHumn (Full a _ b) = containsHumn a || containsHumn b

evalTree :: MonkeyTree -> Int
evalTree (Leaf _ n) = n
evalTree (Full a op b) = case op of
  '+' -> evalTree a + evalTree b
  '-' -> evalTree a - evalTree b
  '*' -> evalTree a * evalTree b
  '/' -> evalTree a `div` evalTree b

buildTree :: String -> Map String Job -> MonkeyTree
buildTree name ms = case ms Map.! name of
  Value n -> Leaf name n
  Operation a op b -> Full (buildTree a ms) op (buildTree b ms)

parseLine :: String -> (String, Job)
parseLine s
  | length ws == 4 = (name, Operation (ws !! 1) (head $ ws !! 2) (ws !! 3))
  | otherwise = (name, Value (read (ws !! 1)))
  where
    ws = words s
    name = (init . head) ws