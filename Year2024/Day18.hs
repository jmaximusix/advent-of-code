module Day18 (part1, part2) where

import Algorithm.Search (bfs)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set
import Linear (V2 (..))
import MyLib.GridV (VecPos, neighbors)
import MyLib.Utils (binarySearch, readNumbers, tup2)

part1 :: [String] -> Int
part1 = fromJust . solveUntilN 1024 . blocks

part2 :: [String] -> String
part2 input = input !! pred (binarySearch (isNothing . (`solveUntilN` blocks input)) 1025 (length input))

next :: Set.Set VecPos -> VecPos -> [VecPos]
next nblocks = filter (\v'@(V2 a b) -> min a b >= 0 && max a b <= 70 && v' `Set.notMember` nblocks) . neighbors

blocks :: [String] -> [VecPos]
blocks = map (uncurry V2 . tup2 . readNumbers)

solveUntilN :: Int -> [VecPos] -> Maybe Int
solveUntilN n allblocks = length <$> bfs (next nblocks) (== V2 70 70) (V2 0 0)
  where
    nblocks = Set.fromList $ take n allblocks