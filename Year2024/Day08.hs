module Day08 (part1, part2) where

import Combinatorics
import Data.List (groupBy, sort, sortOn)
import Data.List.Extra (groupOn)
import qualified Data.Set as Set (Set, empty, filter, fromList, insert, union)
import Debug.Trace (traceShow, traceShowId)
import GHC.Cmm (vec)
import Geometry (Grid, Pos, dimensions, isInside, zipPoints)

part1, part2 :: Grid Char -> Int
part1 input = length $ Set.filter (isInside (dimensions input)) $ foldl freqAntennas Set.empty (groupOn snd $ sortOn snd $ filter (\(_, a) -> a /= '.') $ zipPoints input)
part2 = undefined

freqAntennas :: Set.Set Pos -> [(Pos, Char)] -> Set.Set Pos
freqAntennas set freq = Set.union set $ traceShowId $ Set.fromList $ concatMap (\[a, b] -> antennas a b) (variate 2 $ map fst freq)

antennas :: Pos -> Pos -> [Pos]
antennas (a1, a2) (b1, b2) = [(2 * b1 - a1, 2 * b2 - a2), (2 * a1 - b1, 2 * a2 - b2)]

-- vecminus :: Pos -> Pos -> Pos
-- vecminus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- vecscalmult :: Int -> Pos -> Pos
-- vecscalmult s (x, y) = (s * x, s * y)

-- vecplus :: Pos -> Pos -> Pos
-- vecplus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)