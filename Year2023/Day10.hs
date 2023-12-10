{-# LANGUAGE TupleSections #-}

module Day10 (part1, part2) where

import Data.List (elemIndex, find, nub, tails)
import Data.List.Extra (chunksOf)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Tuple (swap)
import Data.Tuple.Extra (dupe, first, second)
import Debug.Trace (traceShow, traceShowId)

type Pos = (Int, Int)

type Grid = [[Char]]

part1, part2 :: Grid -> String
part1 grid = show $ walkInLoop 0 grid (traceShowId $ start grid) `div` 2

-- part2 grid = traceShowId (length grid * length (head grid)) - traceShowId (length loop) - (traceShowId $ length (traceShowId $ notEnclosed grid loop [(0, 0)]) + 1)
--   where
--     loop = traceShowId $ traceLoop grid (start grid)
-- part2 grid = pprint (map fst $ filter snd mapped) grid
--   where
-- part2 grid = show $ pred $ length $ filter snd mapped

-- part2 grid = pprint loop grid
--   where
--     mapped = traceShowId $ map (second (isEnclosed grid loop [])) yeet
--     yeet = map (first (snd . head) . dupe . pure) ps :: [(Pos, [(Bool, Pos)])]
--     loop = traceLoop grid (start grid)
--     ps = map (False,) $ filter (`notElem` loop) [(x, y) | y <- [0 .. length grid - 1], x <- [0 .. length (head grid) - 1]]

walkInLoop :: Int -> Grid -> (Pos, Pos) -> Int
walkInLoop n grid p
  | snd p' == fst (start grid) = n + 2
  | otherwise = walkInLoop (n + 1) grid p'
  where
    p' = next grid p

-- isEnclosed :: Grid -> [Pos] -> [Pos] -> [(Bool, Pos)] -> Bool
-- isEnclosed _ _ _ [] = True
-- isEnclosed grid loop done ps
--   | isJust isOut = traceShow done $ fromJust isOut
--   | otherwise = isEnclosed grid loop (map snd yeet ++ done) yeet
--   where
--     yeet = filter ((`notElem` done) . snd) $ nextBfsAll grid loop ps
--     isOut = fst <$> find (isOutside grid . snd) yeet

-- nextBfsAll :: Grid -> [Pos] -> [(Bool, Pos)] -> [(Bool, Pos)]
-- nextBfsAll grid loop ps = nub $ concatMap (nextBfs grid loop) ps

part2 grid = show $ length $ filter id $ concat mapped
  where
    mapped = map (map (traceShowId . isEnclosed2) . tails) grid'
    grid' = pprint2 loop grid
    loop = traceLoop grid (start grid)

isEnclosed2 :: String -> Bool
isEnclosed2 s | traceShow s False = undefined
isEnclosed2 [] = False
isEnclosed2 (x : xs)
  | x /= '.' = False
  | even $ traceShowId (((ups - downs) `div` 2) + reals) = False
  | otherwise = True
  where
    ups = length $ filter hasVerticalUp xs
    downs = length $ filter hasVerticalDown xs
    reals = length $ filter hasRealVertical xs
    hasRealVertical c = c `elem` ['|', 'S']
    hasVerticalUp c = c `elem` ['J', 'F']
    hasVerticalDown c = c `elem` ['L', '7']

-- nextBfs :: Grid -> [Pos] -> (Bool, Pos) -> [(Bool, Pos)]
-- nextBfs grid loop (skip, (x, y)) = catMaybes [left, right, up, down, leftdown, rightdown, leftup, rightup]
--   where
--     left = if (x - 1, y) `elem` loop then left2 else Just (skip, (x - 1, y))
--     right = if (x + 1, y) `elem` loop then right2 else Just (skip, (x + 1, y))
--     up = if (x, y - 1) `elem` loop then up2 else Just (skip, (x, y - 1))
--     down = if (x, y + 1) `elem` loop then down2 else Just (skip, (x, y + 1))
--     leftdown = if (x - 1, y + 1) `elem` loop then Nothing else Just (skip, (x - 1, y + 1))
--     rightdown = if (x + 1, y + 1) `elem` loop then Nothing else Just (skip, (x + 1, y + 1))
--     leftup = if (x - 1, y - 1) `elem` loop then Nothing else Just (skip, (x - 1, y - 1))
--     rightup = if (x + 1, y - 1) `elem` loop then Nothing else Just (skip, (x + 1, y - 1))
--     left2 = if (x - 2, y) `elem` loop then Nothing else Just (not skip, (x - 2, y))
--     right2 = if (x + 2, y) `elem` loop then Nothing else Just (not skip, (x + 2, y))
--     up2 = if (x, y - 2) `elem` loop then Nothing else Just (not skip, (x, y - 2))
--     down2 = if (x, y + 2) `elem` loop then Nothing else Just (not skip, (x, y + 2))

isOutside :: Grid -> Pos -> Bool
isOutside grid (x, y) = x < 0 || y < 0 || y >= length grid || x >= length (head grid)

-- notEnclosed :: Grid -> [Pos] -> [Pos] -> [Pos]
-- notEnclosed grid loop [] = []
-- notEnclosed grid loop ps = nub $ next ++ notEnclosed grid loop next
--   where
--     next = nub $ concatMap (filter (`notElem` loop) . nextBfs grid) ps

-- nextBfs :: Grid -> Pos -> [Pos]
-- nextBfs grid (x, y)
--   | y < length grid - 1 && x < length (head grid) - 1 = [right, down]
--   | y < length grid - 1 = [down]
--   | x < length (head grid) - 1 = [right]
--   | otherwise = []
--   where
--     right = (x + 1, y)
--     down = (x, y + 1)

traceLoop :: Grid -> (Pos, Pos) -> [Pos]
traceLoop grid p
  | b == fst (start grid) = [a, b]
  | otherwise = a : traceLoop grid p'
  where
    p'@(a, b) = next grid p

start :: Grid -> (Pos, Pos)
start grid
  | getSym grid (x + 1, y) `elem` ['-', 'J', '7'] = (p, (x + 1, y))
  | getSym grid (x, y - 1) `elem` ['|', 'F', '7'] = (p, (x, y - 1))
  | getSym grid (x - 1, y) `elem` ['-', 'F', 'L'] = (p, (x - 1, y))
  | getSym grid (x, y + 1) `elem` ['|', 'J', 'L'] = (p, (x, y + 1))
  | otherwise = error "Invalid start"
  where
    p@(x, y) = swap $ index2d 'S' grid

next :: Grid -> (Pos, Pos) -> (Pos, Pos)
-- next g (p, p') | traceShow (g, p, p') False = undefined
next g ((x, y), p'@(x', y'))
  | cs == 'J' && y == y' = (p', (x', y' - 1))
  | cs == 'J' = (p', (x' - 1, y'))
  | cs == 'L' && y == y' = (p', (x', y' - 1))
  | cs == 'L' = (p', (x' + 1, y'))
  | cs == '7' && y == y' = (p', (x', y' + 1))
  | cs == '7' = (p', (x' - 1, y'))
  | cs == 'F' && y == y' = (p', (x', y' + 1))
  | cs == 'F' = (p', (x' + 1, y'))
  | cs == '-' = (p', (2 * x' - x, y'))
  | cs == '|' = (p', (x', 2 * y' - y))
  where
    cs = g !! y' !! x'

index2d :: Char -> Grid -> Pos
index2d c g = (length g - length (trunc c), fromJust $ head (trunc c))
  where
    trunc c' = dropWhile isNothing $ map (elemIndex c') g

getSym :: Grid -> Pos -> Char
getSym g (x, y)
  | x < 0 || y < 0 || y >= length g || x >= length (head g) = '.'
  | otherwise = g !! y !! x

pprint :: [Pos] -> Grid -> String
pprint loop grid = ('\n' :) $ unlines $ chunksOf (length (head grid)) $ map (\x -> if x `elem` loop then '#' else '.') allps
  where
    allps = [(x, y) | y <- [0 .. length grid - 1], x <- [0 .. length (head grid) - 1]]

pprint2 :: [Pos] -> Grid -> Grid
pprint2 loop grid = chunksOf (length (head grid)) $ map (\p@(x, y) -> if p `elem` loop then grid !! y !! x else '.') allps
  where
    allps = [(x, y) | y <- [0 .. length grid - 1], x <- [0 .. length (head grid) - 1]]