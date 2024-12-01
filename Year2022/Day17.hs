module Day17 (solution17) where

import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust)
import Data.Set (Set, member)
import qualified Data.Set as Set (empty, filter, findMax, fromList, map, union)
import Linear.V2 (V2 (..))

solution17 :: IO ()
solution17 = do
  myline <- head . lines <$> readFile "input17"
  let n = 1000000000000
  let dirs = cycle $ parseInput myline
  let f x = returnMaxY $ fst $ foldl dropShape (Set.empty, dirs) $ take x shapes
  let period = fromJust $ findPeriod (Set.empty, dirs, [], 5) shapes
  let pdiff = f (2 * period) - f period
  let a = f period - pdiff
  print $ f 2022
  print $ f ((n - a) `mod` period + a) + ((n - a) `div` period) * pdiff

type Pos = V2 Int

type Grid = Set Pos

data Direction = L | R deriving (Show, Eq)

data Shape = Shape {points :: Set Pos} | Pattern [(Int, Int)]

shapes :: [Shape]
shapes =
  cycle
    [ Pattern [(x, 0) | x <- [0 .. 3]],
      Pattern [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
      Pattern [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
      Pattern [(0, y) | y <- [0 .. 3]],
      Pattern [(x, y) | x <- [0, 1], y <- [0, 1]]
    ]

findPeriod :: (Grid, [Direction], [Int], Int) -> [Shape] -> Maybe Int
findPeriod _ [] = Nothing
findPeriod (g, d, l, t) (x : xs)
  | needNextCheck && isRepeating t' (tail nl) = Just t'
  | otherwise = findPeriod (ng, nd, nl, t') xs
  where
    isRepeating n xs' = all (== take n xs') $ init $ tail (chunksOf n xs')
    needNextCheck = length l >= 5 * t
    t' = if needNextCheck then t + 1 else t
    nl = if null l then [nmax] else nmax : nmax - head l : tail l
    nmax = returnMaxY ng
    (ng, nd) = dropShape (g, d) x

returnMaxY :: Set Pos -> Int
returnMaxY s
  | null s = 0
  | otherwise = Set.findMax (Set.map (\(V2 _ y) -> y) s) + 1

removeUnnecessary :: Set Pos -> Set Pos
removeUnnecessary s = Set.filter (\(V2 _ y) -> y >= lowestRelevantY - 100) s
  where
    lowestRelevantY = minimum $ map (returnMaxY . getColumn) [0 .. 6]
    getColumn x = Set.filter (\(V2 x' _) -> x' == x) s

parseInput :: String -> [Direction]
parseInput = map parse
  where
    parse '<' = L
    parse '>' = R

spawnShape :: Grid -> Shape -> Shape
spawnShape blocked (Pattern ps) =
  let v = V2 2 (3 + returnMaxY blocked)
   in Shape (Set.fromList $ map ((+ v) . uncurry V2) ps)

dropShape :: (Grid, [Direction]) -> Shape -> (Grid, [Direction])
dropShape g@(b, _) s@(Pattern _) = dropShape g $ spawnShape b s
dropShape (blocked, d : ds) s@(Shape ps)
  | cllds mvdir && cllds (mvdown s) = (removeUnnecessary (blocked `Set.union` ps), ds)
  | cllds mvdir = dropShape (blocked, ds) (mvdown s)
  | cllds (mvdown mvdir) = (removeUnnecessary (blocked `Set.union` points mvdir), ds)
  | otherwise = dropShape (blocked, ds) (mvdown mvdir)
  where
    cllds = collides blocked
    mvdir = case d of
      L -> move s (V2 (-1) 0)
      R -> move s (V2 1 0)
    mvdown shape = move shape (V2 0 (-1))

move :: Shape -> Pos -> Shape
move (Shape ps) d = Shape (Set.map (+ d) ps)

collides :: Set Pos -> Shape -> Bool
collides blocked s = any (\x -> x `member` blocked || hitsbounds x) (points s)
  where
    hitsbounds (V2 x y) = x < 0 || x > 6 || y < 0

drawCave :: Grid -> String
drawCave blocked = unlines $ reverse $ map drawRow [0 .. returnMaxY blocked]
  where
    drawRow y = map (\x -> if V2 x y `member` blocked then '#' else '.') [0 .. 6]