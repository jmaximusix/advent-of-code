module Day18 (part1, part2) where

import Algorithm.Search (bfs, pruning)
import Control.Applicative (liftA2)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set
import Linear (V2 (..))
import MyLib.GridV (VecPos, neighbors, withinBounds)
import MyLib.Utils (binarySearch, readNumbers, tup2)

part1 :: [String] -> Int
part1 = fromJust . solveUntilN 1024 . blocks

part2 :: [String] -> String
part2 input = input !! pred (binarySearch (isNothing . (`solveUntilN` blocks input)) 1025 (length input))

blocks :: [String] -> [VecPos]
blocks = map (uncurry V2 . tup2 . readNumbers)

solveUntilN :: Int -> [VecPos] -> Maybe Int
solveUntilN n allblocks = length <$> bfs next (== V2 70 70) (V2 0 0)
  where
    nblocks = Set.fromList $ take n allblocks
    next = neighbors `pruning` liftA2 (||) (not . (`withinBounds` (70, 70))) (`Set.member` nblocks)