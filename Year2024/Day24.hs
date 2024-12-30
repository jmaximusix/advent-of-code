{-# LANGUAGE TupleSections #-}

module Day24 (part1, part2) where

import Algorithm.Search (dfs)
import Data.Bits (xor)
import Data.List (intercalate)
import Data.List.Extra (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import qualified Data.SBV as SBV
import qualified Data.Set as Set
import MyLib (Part (..), binarySearch)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

data Label = Named String | Z Int deriving (Eq, Ord)

instance Show Label where
  show (Named s) = s
  show (Z i) = printf "z%02d" i

data Root = Lit Bool | Sym Char Int deriving (Ord, Eq)

instance Show Root where
  show (Lit b) = show b
  show (Sym c i) = printf "%c%02d" c i

data Op = And | Or | Xor deriving (Eq, Ord)

data TreeEntry = Root Root | Wire Label Op TreeEntry TreeEntry

type SymWire = (Op, Either Label Root, Either Label Root)

type SymMap = Map.Map Label SymWire

type Bools = Map.Map Char [SBV.SBool]

part1 :: [String] -> Int
part1 input = result
  where
    result = foldl (\a b -> 2 * a + (if b then 1 else 0)) 0 $ reverse solved
    solved = map (solveLit . buildTree mappings) [Left (Z a) | a <- [0 .. 45]]
    mappings = parseInput Part1 input

part2 :: [String] -> String
part2 input = intercalate "," . map show . Set.toAscList $ searchres
  where
    symmap = parseInput Part2 input
    searchres = snd . last . fromJust $ dfs next isGoal (symmap, Set.empty)

parseInput :: Part -> [String] -> SymMap
parseInput p input = Map.fromList wires
  where
    wires = map ((\[g, l] -> (toLabel l,) . parsegate $ splitOn " " g) . splitOn " -> ") bottom
    parsegate [a, gate, b] = (fromString gate, labelOrRoot a, labelOrRoot b)
    toLabel s@(c1 : _) = if c1 == 'z' then Z (read $ drop 1 s) else Named s
    labelOrRoot s = case Map.lookup s roots of
      Just a -> Right a
      Nothing -> Left $ toLabel s
    insertRoot m [a@(c : r), b] = case p of
      Part1 -> Map.insert a (Lit (b == "1")) m
      Part2 -> Map.insert a (Sym c (read r)) m
    roots = foldl insertRoot Map.empty $ map (splitOn ": ") top
    [top, bottom] = splitOn [""] input

buildTree :: SymMap -> Either Label Root -> TreeEntry
buildTree _ (Right r) = Root r
buildTree m (Left l) = Wire l op (buildTree m a) (buildTree m b)
  where
    (op, a, b) = m Map.! l

solveLit :: TreeEntry -> Bool
solveLit (Root (Lit b)) = b
solveLit (Wire _ op a b) = toFn op (solveLit a) (solveLit b)

solveSym :: Bools -> TreeEntry -> SBV.Symbolic SBV.SBool
solveSym bs (Root (Sym c i)) = return $ bs Map.! c !! i
solveSym bs (Wire _ op a b) = do
  a' <- solveSym bs a
  b' <- solveSym bs b
  return $ (toSymbolicFn op) a' b'

isGoal :: (SymMap, Set.Set Label) -> Bool
isGoal (symmap, s)
  | Set.size s /= 8 = False
  | otherwise = isNothing maybeBreakingAt
  where
    zts = map (buildTree symmap) [Left (Z a) | a <- [0 .. 45]]
    maybeBreakingAt = binarySearch (not . liftA2 zNCorrect id (zts !!)) 0 45

next :: (SymMap, Set.Set Label) -> [(SymMap, Set.Set Label)]
next (symmap, swapped) = newsymmaps
  where
    trees = Map.mapWithKey (\k _ -> buildTree symmap (Left k)) symmap
    zts = map (buildTree symmap) [Left (Z a) | a <- [0 .. 45]]
    (Just br) = binarySearch (not . liftA2 zNCorrect id (zts !!)) 0 45
    toSwap = wiredeps $ zts !! br
    excl1 = wiredeps $ zts !! (br - 1)
    pot = Map.keysSet $ Map.filter (isRelevant br . deps) $ trees
    toSwap' = toSwap `Set.difference` (Set.union swapped excl1)
    candidates = pot `Set.difference` (Set.unions [swapped, excl1])
    newsymmaps =
      filter (\(a, _) -> zNCorrect br (buildTree a (Left (Z br)))) $
        map
          (swapInMap symmap swapped)
          [ (a, b)
          | a <- Set.toList candidates,
            b <- Set.toList toSwap',
            swapPossible trees a b
          ]

swapPossible :: Map.Map Label TreeEntry -> Label -> Label -> Bool
swapPossible trees a b = a /= b && a `Set.notMember` wdb && b `Set.notMember` wda
  where
    wda = wiredeps aTree
    wdb = wiredeps bTree
    aTree = trees Map.! a
    bTree = trees Map.! b

swapInMap :: SymMap -> Set.Set Label -> (Label, Label) -> (SymMap, Set.Set Label)
swapInMap m swapped (a, b) =
  ( Map.insert a (m Map.! b) $
      Map.insert b (m Map.! a) m,
    Set.union (Set.fromList [a, b]) swapped
  )

isRelevant :: Int -> Set.Set Root -> Bool
isRelevant n s =
  (s `Set.isSubsetOf` wanteddeps)
    && (Set.fromList [Sym 'x' n, Sym 'y' n] `Set.isSubsetOf` s)
  where
    wanteddeps = Set.fromList [Sym xy a | xy <- ['x', 'y'], a <- [0 .. n]]

zNCorrect :: Int -> TreeEntry -> Bool
zNCorrect n zn = unsafePerformIO $ SBV.isTheorem $ do
  bools <-
    fmap Map.fromList
      . sequence
      . map (\c -> fmap (c,) $ SBV.sBools [show (Sym c a) | a <- [0 .. 44]])
      $ ['x', 'y']
  let toSInt64 = foldl (\acc a -> 2 * acc + (SBV.oneIf a)) (0 :: SBV.SInt64) . reverse
  let sIntSum = sum $ map toSInt64 $ Map.elems bools
  let control = (sIntSum SBV..>>. n) `SBV.sMod` 2 SBV..== 1
  evalzn <- solveSym bools zn
  return $ evalzn SBV..<=> control

deps :: TreeEntry -> Set.Set Root
deps (Root s) = Set.singleton s
deps (Wire _ _ a b) = Set.union (deps a) (deps b)

wiredeps :: TreeEntry -> Set.Set Label
wiredeps (Root _) = Set.empty
wiredeps (Wire l _ a b) = Set.unions [Set.singleton l, wiredeps a, wiredeps b]

fromString :: String -> Op
fromString s = case s of
  "AND" -> And
  "OR" -> Or
  "XOR" -> Xor
  _ -> error "Invalid operator"

toFn :: Op -> (Bool -> Bool -> Bool)
toFn o = case o of
  And -> (&&)
  Or -> (||)
  Xor -> xor

toSymbolicFn :: Op -> (SBV.SBool -> SBV.SBool -> SBV.SBool)
toSymbolicFn o = case o of
  And -> (SBV..&&)
  Or -> (SBV..||)
  Xor -> (SBV..<+>)
