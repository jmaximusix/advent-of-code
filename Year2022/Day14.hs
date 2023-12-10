module Day14 (part1, part2) where

import Advent (Part (Part1, Part2))
import Data.List (find)
import Data.List.Extra (splitOn)
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set, member)
import qualified Data.Set as Set (fromList, insert, map, unions)
import Data.Tuple.Extra (dupe, first)

part1, part2 :: [String] -> Int
part1 = solve Part1
part2 = solve Part2

solve :: Part -> [String] -> Int
solve part =
  uncurry (dropallsand part 0 (500, 0))
    . first (pred . maximum . Set.map snd)
    . dupe
    . Set.unions
    . map parseline

type Pos = (Int, Int)

parseline :: String -> Set Pos
parseline l = (Set.fromList . concat . zipWith interpolate corners) (tail corners)
  where
    corners = map ((\[x, y] -> (read x, read y)) . splitOn ",") $ splitOn " -> " l

interpolate :: Pos -> Pos -> [Pos]
interpolate (x1, y1) (x2, y2) = [(x, y) | x <- range x1 x2, y <- range y1 y2]
  where
    range a b = [min a b .. max a b]

dropsand :: Part -> Pos -> Int -> Set Pos -> Maybe Pos
dropsand part (x, y) lp occupied
  | part == Part1 && y > lp = Nothing
  | part == Part2 && y >= lp + 2 = Just (x, y)
  | any free moves = dropsand part (fromJust $ find free moves) lp occupied
  | otherwise = Just (x, y)
  where
    free p = not $ p `member` occupied
    moves = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

dropallsand :: Part -> Int -> Pos -> Int -> Set Pos -> Int
dropallsand part n org lp rocks
  | part == Part1 && isNothing next = n
  | part == Part2 && snd (fromJust next) == snd org = n + 1
  | otherwise = dropallsand part (n + 1) org lp (Set.insert (fromJust next) rocks)
  where
    next = dropsand part org lp rocks
