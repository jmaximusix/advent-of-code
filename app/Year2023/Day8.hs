module Year2023.Day8 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.List (findIndex, uncons)
import Data.List.Extra (splitOn)
import qualified Data.Map as Map (Map, fromList, keys, (!))
import Data.Maybe (fromJust)

data Instruction = L | R deriving (Show, Eq, Read)

type Nodes = Map.Map String (String, String)

part1, part2 :: [String] -> Int
part1 input = findLen (== "ZZZ") (parseInput input) "AAA"
part2 input = foldl1 lcm $ map (findLen ((== 'Z') . last) parsed) endingInA
  where
    parsed@(_, nodes) = parseInput input
    endingInA = filter ((== 'A') . last) $ Map.keys nodes

findLen :: (String -> Bool) -> ([Instruction], Nodes) -> String -> Int
findLen stopc (insts, nodes) start = fromJust $ findIndex stopc $ scanl (doStep nodes) start insts

parseInput :: [String] -> ([Instruction], Nodes)
parseInput = bimap (cycle . map (read . (: []))) (Map.fromList . map parseNode . tail) . fromJust . uncons

parseNode :: String -> (String, (String, String))
parseNode node = (name, (left, right))
  where
    [name, adjacent] = splitOn " = " node
    [left, right] = (splitOn ", " . init . tail) adjacent

doStep :: Nodes -> String -> Instruction -> String
doStep nodes current L = fst $ nodes Map.! current
doStep nodes current R = snd $ nodes Map.! current