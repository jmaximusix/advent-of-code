module Geometry where

import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import MyLib (replace)

data Direction = L | U | R | D deriving (Show, Ord, Eq)

data Orientation = H | V deriving (Show, Ord, Eq)

type Grid a = [[a]]

type Pos = (Int, Int)

replace2d :: (Int, Int) -> a -> [[a]] -> [[a]]
replace2d (x, y) new grid = replace y (replace x new (grid !! y)) grid

index2d :: (Eq a) => a -> Grid a -> Pos
index2d c g = (fromJust $ head (trunc c), length g - length (trunc c))
  where
    trunc c' = dropWhile isNothing $ map (elemIndex c') g

getGridElementSafe :: Grid a -> Pos -> Maybe a
getGridElementSafe g (x, y)
  | x < 0 || y < 0 || y >= length g || x >= length (head g) = Nothing
  | otherwise = Just $ g !! y !! x

getGridElementWithDefault :: a -> Grid a -> Pos -> a
getGridElementWithDefault def g p = fromMaybe def $ getGridElementSafe g p

getGridElement :: Grid a -> Pos -> a
getGridElement g (x, y) = g !! y !! x

dimensions :: Grid a -> (Int, Int)
dimensions g = (length $ head g, length g)

pointList :: Grid a -> [Pos]
pointList g = let (xdim, ydim) = dimensions g in [(x, y) | y <- [0 .. ydim - 1], x <- [0 .. xdim - 1]]

zipPoints :: Grid a -> [(Pos, a)]
zipPoints g = zip (pointList g) (concat g)

invertDir :: Direction -> Direction
invertDir d = case d of
  L -> R
  U -> D
  R -> L
  D -> U

tcabDist :: Pos -> Pos -> Int
tcabDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

neighborsOct :: Pos -> [Pos]
neighborsOct (x, y) = [(x', y') | y' <- [y - 1 .. y + 1], x' <- [x - 1 .. x + 1]]

neighborTo :: Direction -> Pos -> Pos
neighborTo d (x, y) = case d of
  L -> (x - 1, y)
  U -> (x, y - 1)
  R -> (x + 1, y)
  D -> (x, y + 1)

isInside :: (Int, Int) -> Pos -> Bool
isInside (xdim, ydim) (x, y) = x >= 0 && y >= 0 && x < xdim && y < ydim

-- L is left, R is right, U is (continue) forward (no turn), D is backward (turn around)
turn :: Direction -> Direction -> Direction
turn turn' facing = case turn' of
  U -> facing
  D -> invertDir facing
  R -> invertDir $ turn L facing
  L -> case facing of
    L -> D
    U -> L
    R -> U
    D -> R

isOnEdge :: Grid a -> Pos -> Bool
isOnEdge g (x, y) = let (xm, ym) = dimensions g in x == 0 || y == 0 || x == xm - 1 || y == ym - 1