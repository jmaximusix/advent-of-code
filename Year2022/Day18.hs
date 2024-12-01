module Day18 (solution18) where

import Data.List (nub)
import Data.List.Extra (splitOn)
import Data.List.Index (indexed)
import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, insert, member, toList, (!))
import Data.Set (Set, member)
import qualified Data.Set as Set (empty, findMax, findMin, fromList, insert, map, toList, (\\))
import Linear.V3 (V3 (..))

solution18 :: IO ()
solution18 = do
  mylines <- lines <$> readFile "input18"
  let cubes = map parseInput mylines

  -- let cubes = Set.toList $ (gencube (V3 0 0 0) (V3 6 6 6)) Set.\\ (gencube (V3 1 1 1) (V3 2 3 4))
  let sides = concatMap gensides $ indexed cubes
  let tbd = filter (`notElem` cubes) $ map snd sides
  let defaultwl = Map.fromList $ indexed (replicate (length tbd) $ List [])
  let (_, res) = nextStep (indexed tbd, State defaultwl Map.empty Map.empty Set.empty (Set.fromList cubes))
  let freed = Set.toList $ free res
  let fromwl = concatMap ((\(List a) -> a) . (\(k, v) -> if (k `elem` freed) && not (isPointer v) then v else List [])) $ Map.toList $ waitlist res
  print $ length tbd
  print $ length $ Set.fromList (freed ++ fromwl)

type Pair = (Int, V3 Int)

type Positions = Map (V3 Int) Int

data Waitlist = List [Int] | Pointer Int deriving (Show)

data State = State {waitlist :: Map Int Waitlist, positions :: Positions, visited :: Positions, free :: Set Int, out :: Set (V3 Int)} deriving (Show)

shouldcontinue :: State -> Pair -> Bool
shouldcontinue state (i, v)
  | (v `Map.member` visited state) && (visited state Map.! v) == i = False
  | v `member` out state = False
  | i `member` free state = False
  | isPointer (waitlist state Map.! i) = False
  | otherwise = True

nextStep :: ([Pair], State) -> ([Pair], State)
nextStep ([], state) = ([], state)
nextStep (tbd, state) = nextStep (tbd', state'')
  where
    nextgen = nub $ concatMap gensides tbd
    state' = foldl updateState state nextgen
    tbd' = filter (shouldcontinue state') nextgen
    state'' = state' {visited = positions state'}

updateState :: State -> Pair -> State
updateState s@(State w p vis f o) (i, v)
  | v `member` o || isPointer (w Map.! i) = s
  | v `outside` o = s {free = Set.insert i f}
  | v `Map.member` p = s {waitlist = transferWaitlist w i (p Map.! v), positions = Map.insert v i p}
  | otherwise = s {positions = Map.insert v i p}

followPointer :: Map Int Waitlist -> Int -> Int
followPointer w i = case w Map.! i of
  Pointer j -> followPointer w j
  List _ -> i

isPointer :: Waitlist -> Bool
isPointer (Pointer _) = True
isPointer (List _) = False

transferWaitlist :: Map Int Waitlist -> Int -> Int -> Map Int Waitlist
transferWaitlist w a b'
  | isPointer (w Map.! a) = Map.insert a (Pointer b) w
  | b /= a = Map.insert a (Pointer b) $ Map.insert b (List $ a : old ++ new) w
  | otherwise = w
  where
    b = followPointer w b'
    (List old) = w Map.! a
    (List new) = w Map.! b

outside :: V3 Int -> Set (V3 Int) -> Bool
outside (V3 x y z) cubes = x > maxX || x < minX || y > maxY || y < minY || z > maxZ || z < minZ
  where
    maxX = Set.findMax $ Set.map (\(V3 x _ _) -> x) cubes
    maxY = Set.findMax $ Set.map (\(V3 _ y _) -> y) cubes
    maxZ = Set.findMax $ Set.map (\(V3 _ _ z) -> z) cubes
    minX = Set.findMin $ Set.map (\(V3 x _ _) -> x) cubes
    minY = Set.findMin $ Set.map (\(V3 _ y _) -> y) cubes
    minZ = Set.findMin $ Set.map (\(V3 _ _ z) -> z) cubes

parseInput :: String -> V3 Int
parseInput s = (\[x, y, z] -> V3 x y z) $ map read $ splitOn "," s

gensides :: Pair -> [Pair]
gensides (o, v) = [(o, v + V3 x y z) | x <- r, y <- r, z <- r, sum (map abs [x, y, z]) == 1]
  where
    r = [-1 .. 1]

gencube :: V3 Int -> V3 Int -> Set (V3 Int)
gencube (V3 x y z) (V3 x' y' z') = Set.fromList [V3 x y z | x <- [x .. x'], y <- [y .. y'], z <- [z .. z']]