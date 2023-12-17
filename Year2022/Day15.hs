{-# LANGUAGE TypeApplications #-}

module Day15 (part1, part2) where

import Data.Char (isDigit)
import Data.List (find, nub, sort)
import Data.List.Extra (splitOn)
import Data.Maybe (fromJust, mapMaybe)

part1, part2 :: [String] -> Int
part1 input = blockedInRow r - sum [length (filter (\b -> snd b == r) $ nub $ map f sensors) | f <- [position, beacon]]
  where
    r = 2000000
    blockedInRow row = sum $ map (\(a, b) -> succ b - a) $ foldl mergeRanges [] $ sort $ mapMaybe (projection row) sensors
    sensors = map parseBeacon input
part2 input = 2 * r * locx + locy
  where
    r = 2000000
    sensors = map parseBeacon input
    inRange (x, y) = all (\a -> a >= 0 && a <= 2 * r) [x, y]
    (locx, locy) = (fromJust . find inRange . findDistress sensors) $ Subgrid r (r, r)

type Pos = (Int, Int)

data Sensor = Sensor {position :: Pos, distance :: Int, beacon :: Pos} deriving (Show)

data Subgrid = Subgrid {radius :: Int, center :: Pos} deriving (Show)

parseBeacon :: String -> Sensor
parseBeacon s = Sensor (bx, by) (tcabDist (bx, by) (sx, sy)) (sx, sy)
  where
    [bx, by, sx, sy] = map read $ tail $ map (takeWhile (\x -> isDigit x || x == '-')) $ splitOn "=" s

subGrids :: Subgrid -> [Subgrid]
subGrids (Subgrid r (x, y)) =
  let r' = ceiling @Double $ fromIntegral r / 2
      rs = [r', -r']
   in [Subgrid r' (x + dx, y + dy) | dx <- rs, dy <- rs]

genFromSubgrid :: Subgrid -> [Pos]
genFromSubgrid (Subgrid r (sx, sy)) = [(x, y) | x <- [sx - r .. sx + r], y <- [sy - r .. sy + r]]

inSensor :: Subgrid -> Sensor -> Bool
inSensor (Subgrid r (x, y)) (Sensor p dist _) =
  let rs = [r, -r]
   in all (\p' -> dist >= tcabDist p p') [(x + dx, y + dy) | dx <- rs, dy <- rs]

findDistress :: [Sensor] -> Subgrid -> [Pos]
findDistress sensors grid@(Subgrid r _)
  | r == 2 = locate sensors close
  | otherwise = nub $ concatMap (findDistress sensors) close
  where
    close = filter (not . (\x -> any (inSensor x) sensors)) (subGrids grid)

locate :: [Sensor] -> [Subgrid] -> [Pos]
locate sensors = concatMap (filter (canContainBeacon sensors) . genFromSubgrid)

canContainBeacon :: [Sensor] -> Pos -> Bool
canContainBeacon sensors p = not $ any (wouldBeCloser p) sensors

wouldBeCloser :: Pos -> Sensor -> Bool
wouldBeCloser pos (Sensor spos dist _)
  | tcabDist pos spos <= dist = True
  | otherwise = False

tcabDist :: Pos -> Pos -> Int
tcabDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

projection :: Int -> Sensor -> Maybe (Int, Int)
projection y (Sensor (sx, sy) dist _)
  | dx > 0 = Just (sx - dx, sx + dx)
  | otherwise = Nothing
  where
    dy = abs (y - sy)
    dx = dist - dy

mergeRanges :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
mergeRanges [] a = [a]
mergeRanges l@((a, b) : xs) (a', b')
  | a' > b + 1 = (a', b') : l
  | a' <= b + 1 && b' <= b = l
  | otherwise = (a, b') : xs