module Day18 (part1, part2) where

import Data.List (nub)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow, traceShowId)
import qualified GHC.Float as Set
import MyLib.Geometry
import Numeric (readHex)

part1, part2 :: [String] -> Int
part1 instrs = Set.size $ fill (edge instrs) (Set.singleton (1, 1))
part2 instrs = traceShow (foldl1 gcd $ map (snd . parseInstr) instrs) 1

edge :: [String] -> Set.Set Pos
edge = fst . foldl doInstr (Set.empty, (0, 0))

fill :: Set.Set Pos -> Set.Set Pos -> Set.Set Pos
fill old ps
  | null new = old
  | otherwise = fill (Set.union old new) new
  where
    new = Set.unions $ Set.map (Set.filter (`Set.notMember` old) . traceShowId . Set.fromList . neighborsOct) ps

doInstr :: (Set.Set Pos, Pos) -> String -> (Set.Set Pos, Pos)
doInstr (set, last@(x', y')) instr = (Set.union interpol set, next)
  where
    interpol = Set.fromList $ [(x, y) | x <- [min x' x'' .. max x' x''], y <- [min y' y'' .. max y' y'']]
    next@(x'', y'') = goNSteps (read b) (read a) last
    (a : b : xs) = words instr

parseInstr :: String -> (Direction, Int)
parseInstr instr = (dir, len)
  where
    dir = case last hexcode of
      '0' -> R
      '1' -> D
      '2' -> L
      '3' -> U
    len = fst $ head $ readHex $ init hexcode
    hexcode = init . tail . tail $ (words instr) !! 2