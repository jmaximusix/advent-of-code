module Day12 (part1, part2) where

import Data.Char (ord)
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust, isNothing)

-- Visualization
-- import Data.List.Extra (chunksOf)
-- import Debug.Trace (trace)

part1, part2 :: Grid -> Int
part1 grid = find (\o n -> o + 1 >= n) 'E' 0 grid ([], [index2d 'S' grid])
part2 grid = find (\o n -> o <= n + 1) 'a' 0 grid ([], [index2d 'E' grid])

type Pos = (Int, Int)

type Grid = [[Char]]

elevation :: Char -> Int
elevation 'S' = ord 'a'
elevation 'E' = ord 'z'
elevation c = ord c

index2d :: Char -> Grid -> Pos
index2d c g = (length g - length (trunc c), fromJust $ head (trunc c))
  where
    trunc c' = dropWhile isNothing $ map (elemIndex c') g

findNext :: (Int -> Int -> Bool) -> Grid -> ([Pos], [Pos]) -> ([Pos], [Pos])
findNext rule grid (old, new) = (old ++ new, nub $ concatMap next new)
  where
    val p = getGrid p grid
    next p@(x, y) =
      filter (`notElem` (p : old)) $
        map
          (go p)
          [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
    go p newp
      | isNothing (val newp) = p
      | rule (elevation $ fromJust $ val p) (elevation $ fromJust $ val newp) = newp
      | otherwise = p

find :: (Int -> Int -> Bool) -> Char -> Int -> Grid -> ([Pos], [Pos]) -> Int
find rule dest n grid pos
  | any (\(a, b) -> (grid !! a !! b) == dest) (fst np) = n
  | otherwise = find rule dest (n + 1) grid np
  where
    -- Visualization. Extremely laggy.
    -- np = findNext rule grid (trace (pprint pos grid) pos)
    np = findNext rule grid pos

getGrid :: (Int, Int) -> [[a]] -> Maybe a
getGrid (x, y) grid
  | x `elem` [0 .. (length grid - 1)]
      && y `elem` [0 .. (length (head grid) - 1)] =
      Just (grid !! x !! y)
  | otherwise = Nothing

-- Visualization

-- pprint :: ([Pos], [Pos]) -> Grid -> String
-- pprint (old, pos) grid = unlines $ chunksOf ylen [if x `elem` old then '.' else (if x `elem` pos then 'o' else ' ') | x <- [(x, y) | x <- [0 .. (xlen - 1)], y <- [0 .. (ylen - 1)]]]
--   where
--     ylen = length $ head grid
--     xlen = length grid