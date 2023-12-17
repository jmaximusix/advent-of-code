{-# LANGUAGE TupleSections #-}

module Day17 (part1, part2) where

import Advent (Part (..))
import Algorithm.Search (dijkstraAssoc)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (both)
import Geometry

type State = (Pos, (Direction, Int))

type Cost = Int

part1, part2 :: Grid Char -> Int
part1 = solve Part1
part2 = solve Part2

solve :: Part -> Grid Char -> Int
solve part grid = cost
  where
    goal = both pred (dimensions grid)
    (Just (cost, _)) = dijkstraAssoc (next part grid) ((== goal) . fst) ((0, 0), (R, 1))

next :: Part -> Grid Char -> State -> [(State, Cost)]
next part grid (pos, dirlen) = mapMaybe moveTo (allowedTurns part dirlen)
  where
    moveTo dl@(d, _) =
      let pos' = neighborTo d pos
       in (((pos', dl),) . read . pure <$> getGridElementSafe grid pos')

allowedTurns :: Part -> (Direction, Int) -> [(Direction, Int)]
allowedTurns p (dir, len) = case p of
  Part1 -> [(dir, len + 1) | len < 3] ++ map ((,1) . (`turn` dir)) [L, R]
  Part2 -> [(dir, len + 1) | len < 10] ++ [(d `turn` dir, 1) | len >= 4, d <- [L, R]]
