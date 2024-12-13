module Day10 (part1, part2) where

import Control.Applicative (liftA2)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import MyLib.GridV (GridMap, VecPos, asGridMap, neighbors)

part1, part2 :: [[Char]] -> Int
part1 = sum . liftA2 map (trailScore nub) trailHeads . asGridMap
part2 = sum . liftA2 map (trailScore id) trailHeads . asGridMap

trailHeads :: GridMap Char -> [VecPos]
trailHeads = Map.keys . Map.filter (== '0')

trailScore :: ([VecPos] -> [VecPos]) -> GridMap Char -> VecPos -> Int
trailScore nubfunc g start = length (go '9')
  where
    go '0' = [start]
    go n = nubfunc $ concatMap (filter ((== Just n) . flip Map.lookup g) . neighbors) (go (pred n))
