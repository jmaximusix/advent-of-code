module MyLib.Geometry
  ( Grid,
    dimensions,
    Pos,
    getGridElementWithDefault,
    neighborsOct,
    pointList,
    getGridElement,
    getGridElementSafe,
    Direction (..),
    index2d,
    invertDir,
    neighborTo,
    replace2d,
    zipPoints,
    goNSteps,
    isInside,
    turn,
    tcabDist,
    neighbors,
    Range,
    intersect,
  )
where

import Control.Applicative (liftA2)
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import MyLib.Utils (replace)

type Range = (Int, Int)

data Direction = L | U | R | D deriving (Show, Ord, Eq, Read)

data Orientation = H | V deriving (Show, Ord, Eq)

type Grid a = [[a]]

type Pos = (Int, Int)

replace2d :: (Int, Int) -> a -> Grid a -> Grid a
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
dimensions = liftA2 (,) (length . head) length

pointList :: Grid a -> [Pos]
pointList g = let (xdim, ydim) = dimensions g in [(x, y) | y <- [0 .. ydim - 1], x <- [0 .. xdim - 1]]

zipPoints :: Grid a -> [(Pos, a)]
zipPoints = liftA2 zip pointList concat

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

neighbors :: Pos -> [Pos]
neighbors p = map (`neighborTo` p) [L, U, R, D]

neighborTo :: Direction -> Pos -> Pos
neighborTo = goNSteps 1

goNSteps :: Int -> Direction -> Pos -> Pos
goNSteps n d (x, y) = case d of
  L -> (x - n, y)
  U -> (x, y - n)
  R -> (x + n, y)
  D -> (x, y + n)

isInside :: Grid a -> Pos -> Bool
isInside grid (x, y) =
  let (xdim, ydim) = dimensions grid
   in x >= 0 && y >= 0 && x < xdim && y < ydim

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

-- a is the range that gets split on b, seperated in (inside, outside)
intersect :: Range -> Range -> ([Range], [Range])
intersect a@(a1, a2) b@(b1, b2)
  | a1 >= b1 && a2 <= b2 = ([a], [])
  | a1 < b1 && a2 > b2 = ([b], [(a1, b1), (b2, a2)])
  | a1 < b2 && a2 > b2 = ([(a1, b2)], [(b2, a2)])
  | a1 < b1 && a2 > b1 = ([(b1, a2)], [(a1, b1)])
  | otherwise = ([], [a])
