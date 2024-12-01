module Day19 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.IntMap (adjust)
import Data.List.Extra (partition, splitOn, unsnoc)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)
import Debug.Trace (traceShow, traceShowId)
import Geometry (Range, intersect)

data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show)

data Part2 = Part2 {x2 :: Range, m2 :: Range, a2 :: Range, s2 :: Range} deriving (Show)

getAcceptedRanges :: Instructions -> [(String, Part2)] -> [Part2]
getAcceptedRanges _ soos | traceShow soos False = undefined
getAcceptedRanges _ [] = []
getAcceptedRanges instrs parts = map snd accepted ++ getAcceptedRanges instrs open
  where
    next = concatMap (followInstruction2 instrs) parts
    open = filter (not . invalid . snd) . filter ((/= "R") . fst) $ tbd
    (accepted, tbd) = partition ((== "A") . fst) next

eval :: Part2 -> Int
eval (Part2 x' m' a' s') = rl x' * rl m' * rl a' * rl s'
  where
    rl (a'', b) = b - a'' + 1

followInstruction2 :: Instructions -> (String, Part2) -> [(String, Part2)]
followInstruction2 instrs (key, part) = (else', last') : yeet
  where
    (yeet, last') = foldl soos ([], part) instr
    (instr, else') = instrs Map.! key

soos :: ([(String, Part2)], Part2) -> String -> ([(String, Part2)], Part2)
soos a b | traceShow (a, b) False = undefined
soos (sorted, part) instr = case var of
  'x' -> let (in', out) = inOut (x2 part) in ((next, part {x2 = in'}) : sorted, part {x2 = out})
  'm' -> let (in', out) = inOut (m2 part) in ((next, part {m2 = in'}) : sorted, part {m2 = out})
  'a' -> let (in', out) = inOut (a2 part) in ((next, part {a2 = in'}) : sorted, part {a2 = out})
  's' -> let (in', out) = inOut (s2 part) in ((next, part {s2 = in'}) : sorted, part {s2 = out})
  where
    [var : op : val', next] = splitOn ":" instr
    range = case op of
      '<' -> (1, val - 1)
      '>' -> (val + 1, 4000)
    val = read val' :: Int
    inOut r = traceShow (r, range) traceShowId $ adjustOut $ both toSingle $ r `intersect` range

toSingle :: [Range] -> Range
toSingle [] = (-1, -1)
toSingle [r] = r

adjustOut :: (Range, Range) -> (Range, Range)
adjustOut (in'@(_, i2), (o1, o2))
  | o2 > i2 = (in', (o1 + 1, o2))
  | otherwise = (in', (o1, o2 - 1))

type Instruction = ([String], String)

type Instructions = Map.Map String Instruction

part1, part2 :: [String] -> Int
part1 input = sum $ map (\(Part x' m' a' s') -> x' + m' + a' + s') accepted
  where
    accepted = filter (getsAccepted instrs "in") parts
    (instrs, parts) = parseInput input
part2 input = sum $ map eval $ traceShowId $ getAcceptedRanges instrs [("in", Part2 (1, 4000) (1, 4000) (1, 4000) (1, 4000))]
  where
    (instrs, _) = parseInput input

getsAccepted :: Instructions -> String -> Part -> Bool
getsAccepted instrs key part = case next of
  "A" -> True
  "R" -> False
  _ -> getsAccepted instrs next part
  where
    next = followInstruction (instrs Map.! key) part

followInstruction :: Instruction -> Part -> String
followInstruction ([], else') _ = else'
followInstruction (first : rest, else') part
  | applies = next
  | otherwise = followInstruction (rest, else') part
  where
    (applies, next) = applyInstruction first part

applyInstruction :: String -> Part -> (Bool, String)
applyInstruction instr (Part x' m' a' s') = case op of
  '<' -> (var < val, next)
  '>' -> (var > val, next)
  where
    [var' : op : val', next] = splitOn ":" instr
    val = read val'
    var = case var' of
      'x' -> x'
      'm' -> m'
      'a' -> a'
      's' -> s'

invalid :: Part2 -> Bool
invalid (Part2 x' m' a' s') = (-1, -1) `elem` [x', m', a', s']

parseInput :: [String] -> (Instructions, [Part])
parseInput input = bimap (Map.fromList . map parseInstruction) (map parsePart . tail) (break (== "") input)

parseInstruction :: String -> (String, Instruction)
parseInstruction str = (name, instr)
  where
    (name, instr') = break (== '{') str
    instr = fromJust $ unsnoc $ splitOn "," $ init $ tail instr'

parsePart :: String -> Part
parsePart p = Part x' m' a' s'
  where
    [x', m', a', s'] = map (read . last . splitOn "=") $ splitOn "," $ init $ tail p