{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Day24 (part1, part2) where

import Algorithm.Search (aStar, bfs, dfs)
import Control.Lens (Bifunctor (bimap))
import Data.Bits (xor, (.&.), (.|.))
import Data.List.Extra (splitOn, uncons)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing, listToMaybe)
import Data.SBV (SBool, SInt64, Symbolic, isTheorem, oneIf, sBools, sMod, (.&&), (.<+>), (.<=>), (.==), (.>>.), (.||))
import qualified Data.Set as Set
import Data.Tuple.Extra (both, thd3)
import Debug.Trace (traceShow, traceShowId)
import MyLib (Part (..), binarySearch, tup2)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

type EntryMap = Map.Map String Entry

type Bools = Map.Map String SBool

part1 :: [String] -> Int
part1 input = result
  where
    result = foldl1 (\a b -> 2 * a + b) $ reverse solved
    solved = map ((\(LitRoot l) -> l) . snd) $ Map.toList $ Map.map (solve mappings) zs
    zs = Map.filterWithKey (\k _ -> listToMaybe k == Just 'z') mappings
    mappings = parseInput Part1 input

asSint :: Bools -> Char -> SInt64
asSint bs c = foldl (\acc a -> 2 * acc + (oneIf a :: SInt64)) 0 $ reverse bs'
  where
    bs' = map snd $ Map.toList $ Map.filterWithKey (\k _ -> listToMaybe k == Just c) bs

part2 :: [String] -> String
part2 input = show $ searchres
  where
    symmap = parseInput Part2 input
    zs = Map.filterWithKey (\k _ -> listToMaybe k == Just 'z') symmap
    zTrees = Map.map (buildtree symmap) zs
    searchres = recurse (symmap, Set.empty)

recurse :: (EntryMap, Set.Set String) -> Set.Set String
recurse (symmap, swaps) | traceShow swaps False = undefined
recurse (symmap, swaps)
  | isGoal nexte = snd nexte
  | otherwise = recurse nexte
  where
    nexte = head $ next (symmap, swaps)

isGoal :: (EntryMap, Set.Set String) -> Bool
isGoal (symmap, s)
  | Set.size s /= 8 = False
  | otherwise = isNothing maybeBreakingAt
  where
    zs = Map.filterWithKey (\k _ -> listToMaybe k == Just 'z') symmap
    zTrees = Map.map (buildtree symmap) zs
    maybeBreakingAt = binarySearch (not . flip correctUntilN zTrees) 0 45

cost :: (EntryMap, Set.Set String, Maybe Int) -> (EntryMap, Set.Set String, Maybe Int) -> Int
cost (_, _, Just a) (_, _, Just b) = b - a
cost (_, _, Just a) (_, _, Nothing) = 46 - a

heuristic :: (EntryMap, Set.Set String, Maybe Int) -> Int
heuristic (_, _, Just a) = 46 - a
heuristic (_, _, Nothing) = 0

next :: (EntryMap, Set.Set String) -> [(EntryMap, Set.Set String)]
next (_, swapped) | traceShow swapped False = undefined
next (symmap, swapped) = newsymmaps
  where
    trees = Map.map (buildtree symmap) symmap
    ds = Map.map deps trees
    zs = Map.filterWithKey (\k _ -> listToMaybe k == Just 'z') trees
    (Just br) = binarySearch (not . flip correctUntilN zs) 0 45
    toSwap = wiredeps $ zs Map.! cStr 'z' br
    excl1 = wiredeps $ zs Map.! cStr 'z' (br - 1)
    pot = Map.keysSet $ Map.filter (isRelevant br) ds
    toSwap' = toSwap `Set.difference` (Set.union swapped excl1)
    candidates = pot `Set.difference` (Set.unions [swapped, excl1])
    newsymmaps = traceShow (toSwap', candidates) filter (\(a, b) -> traceShow b emapCorrectUntil br a) $ map (swapInMap symmap swapped) [(a, b) | a <- Set.toList candidates, b <- Set.toList toSwap', swapPossible trees a b]

swapPossible :: Map.Map String TreeEntry -> String -> String -> Bool
swapPossible trees a b = a /= b && a `Set.notMember` wdb && b `Set.notMember` wda
  where
    wda = wiredeps aTree
    wdb = wiredeps bTree
    aTree = trees Map.! a
    bTree = trees Map.! b

emapCorrectUntil :: Int -> EntryMap -> Bool
emapCorrectUntil n _ | traceShow n False = undefined
emapCorrectUntil n symmap = correctUntilN n zTrees
  where
    zs = Map.filterWithKey (\k _ -> listToMaybe k == Just 'z') symmap
    zTrees = Map.map (buildtree symmap) zs

swapInMap :: EntryMap -> Set.Set String -> (String, String) -> (EntryMap, Set.Set String)
swapInMap m swapped (a, b) = (Map.insert a (m Map.! b) $ Map.insert b (m Map.! a) m, Set.union (Set.fromList [a, b]) swapped)

isRelevant :: Int -> Set.Set String -> Bool
isRelevant n s = (s `Set.isSubsetOf` (Set.fromList (xyStrsUntilN n))) && (Set.fromList [(cStr 'x' n), cStr 'y' n] `Set.isSubsetOf` s)

cStr :: Char -> Int -> String
cStr c n = printf "%c%02d" c n

xyStrsUntilN :: Int -> [String]
xyStrsUntilN n = [printf "%c%02d" xy a | xy <- ['x', 'y'], a <- [0 .. n]]

correctUntilN :: Int -> Map.Map String TreeEntry -> Bool
correctUntilN n zs = unsafePerformIO $ isTheorem $ do
  bools <- (fmap (Map.fromList . uncurry zip) . \(a, b) -> (a,) <$> b) $ liftA2 (,) id sBools (xyStrsUntilN 44)
  let zn = zs Map.! (printf "z%02d" n)
  let control = isOdd $ ((asSint bools 'x' + asSint bools 'y') .>>. n) `sMod` 2
  evalzn <- evalTreeEntry bools zn
  return $ evalzn .<=> control

isOdd :: SInt64 -> SBool
isOdd x = (x `sMod` 2) .== 1

solve :: Map.Map String Entry -> Entry -> Entry
solve _ l@(LitRoot _) = l
solve m (SymWire _ fname a b) = LitRoot ((\(LitRoot la, LitRoot lb) -> f la lb) $ sol a b)
  where
    sol a' b' = both (solve m) (m Map.! a', m Map.! b')
    f = case fname of
      '|' -> (.|.)
      '&' -> (.&.)
      'x' -> xor

buildtree :: Map.Map String Entry -> Entry -> TreeEntry
buildtree _ (SymRoot l) = Root l
buildtree m (SymWire l f a b) = uncurry (Wire l f) $ both (buildtree m) (m Map.! a, m Map.! b)

evalTreeEntry :: Bools -> TreeEntry -> Symbolic SBool
evalTreeEntry bs (Root b) = return $ bs Map.! b
evalTreeEntry bs (Wire _ fname a b) = do
  a' <- evalTreeEntry bs a
  b' <- evalTreeEntry bs b
  return $ f a' b'
  where
    f = case fname of
      '|' -> (.||)
      '&' -> (.&&)
      'x' -> (.<+>)

deps :: TreeEntry -> Set.Set String
deps (Root s) = Set.singleton s
deps (Wire _ _ a b) = Set.union (deps a) (deps b)

wiredeps :: TreeEntry -> Set.Set String
wiredeps (Root _) = Set.empty
wiredeps (Wire l _ a b) = Set.unions [Set.singleton l, wiredeps a, wiredeps b]

data TreeEntry = Root String | Wire String Char TreeEntry TreeEntry

instance Show TreeEntry where
  show :: TreeEntry -> String
  show (Root s) = s
  show (Wire l f a b) = printf "(%s: %s %c %s)" l (show a) f (show b)

data Entry = LitRoot Int | SymRoot String | SymWire String Char String String
  deriving (Show, Eq, Ord)

parseInput :: Part -> [String] -> EntryMap
parseInput p input = foldl (\m (r, g) -> Map.insert r g m) withtop bottom'
  where
    bottom' = map ((\[g, l] -> (l,) . parsegate l $ splitOn " " g) . splitOn " -> ") bottom
    parsegate l [a, gate, b] = SymWire l (tochar gate) a b
    tochar gate = case gate of
      "AND" -> '&'
      "OR" -> '|'
      "XOR" -> 'x'
    insertRoot m [a, b] = case p of
      Part1 -> Map.insert a (LitRoot (read b)) m
      Part2 -> Map.insert a (SymRoot a) m
    withtop = foldl insertRoot Map.empty $ map (splitOn ": ") top
    [top, bottom] = splitOn [""] input
