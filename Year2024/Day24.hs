{-# LANGUAGE TupleSections #-}

module Day24 (part1, part2) where

import Data.Bits (shiftR, xor, (.&.), (.|.))
import Data.List.Extra (splitOn, uncons)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.SBV (SBool, SDivisible (sMod), SInt64, Symbolic, free, isTheorem, oneIf, output, runSMT, sBools, sFalse, (.&&), (.<+>), (.<=>), (.==), (.||))
import qualified Data.Set as Set
import Data.Tuple.Extra (both)
import Debug.Trace (traceShow, traceShowId)
import MyLib (tup2)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

type EMap = Map.Map String Entry

type Bools = Map.Map String SBool

part1 :: [String] -> Int
part1 input = result
  where
    result = foldl1 (\a b -> 2 * a + b) $ reverse solved
    solved = map ((\(Lit l) -> l) . snd) $ Map.toList $ Map.map (solve mappings) zs
    zs = Map.filterWithKey (\k _ -> head k == 'z') mappings
    mappings = parseInput input

asSint :: Bools -> Char -> SInt64
asSint bs c = foldl (\acc a -> 2 * acc + (oneIf a :: SInt64)) 0 $ reverse bs'
  where
    bs' = map snd $ Map.toList $ Map.filterWithKey (\k _ -> head k == c) bs

part2 :: [String] -> Bool
part2 input = unsafePerformIO $ isTheorem $ do
  let mappings = parseInput2 input
  let zs' = Map.filterWithKey (\k _ -> head k == 'z') mappings
  let zs = Map.map (solve3 mappings) zs'
  bools <- (fmap (Map.fromList . uncurry zip) . \(a, b) -> (a,) <$> b) $ liftA2 (,) id sBools [printf "%c%02d" xy a | xy <- ['x', 'y'], a <- [0 .. 44 :: Int]]
  return $ correctUntilN 6 bools zs

correctUntilN :: Int -> Bools -> Map.Map String Entry3 -> Symbolic SBool
correctUntilN n bools zs = do
  let zn = zs Map.! (printf "z%02d" n)
  let control = ((asSint bools 'x' + asSint bools 'y') `shiftR` n) `sMod` 2
  evalzn <- evalEntry3 bools zn
  return $ (oneIf evalzn :: SInt64) .== control

-- wd45 = wds Map.! "z45"
-- wd7 = wds Map.! "z07"
-- -- probwrong2 = foldl Set.difference probwrongpds wds
-- wds = Map.map (wd) zs
-- -- probwrongpds = Set.union wd45 wd7
-- wd = wiredeps mappings
-- first = solved Map.! "z00"
-- solved = Map.map (solve3 mappings) zs
-- zs = Map.filterWithKey (\k _ -> head k == 'z') mappings
-- mappings = parseInput2 input

-- checkEquivalence :: Entry3 -> Entry3 -> Bool
-- Check equivalence of two Entry3 objects
-- checkEquivalence :: Entry3 -> Entry3 -> Symbolic SBool
-- checkEquivalence e1 e2 = do
--   result1 <- evalEntry3 e1
--   result2 <- evalEntry3 e2
--   return $ result1 .<=> result2

-- checkdeps :: String -> (Set.Set String, Entry2) -> Bool
-- -- checkdeps k s | traceShow (k, s) False = undefined
-- checkdeps k (s, e) = expected == s && ()
--   where
--     expected = Set.fromList [printf "%s%02d" xy a :: String | xy <- ["x", "y"], a <- [0 .. numk]]
--     numk = read $ tail k :: Int

solve :: Map.Map String Entry -> Entry -> Entry
solve m l@(Lit _) = l
solve m e = case e of
  Or a b -> Lit ((\(Lit la, Lit lb) -> la .|. lb) $ sol a b)
  And a b -> Lit ((\(Lit la, Lit lb) -> la .&. lb) $ sol a b)
  Xor a b -> Lit ((\(Lit la, Lit lb) -> la `xor` lb) $ sol a b)
  where
    sol a b = both (solve m) (m Map.! a, m Map.! b)

solve2 :: Map.Map String Entry -> Entry -> Entry2
solve2 m (Orig l) = Root l
solve2 m e = case e of
  Or a b -> uncurry Or2 $ sol a b
  And a b -> uncurry And2 $ sol a b
  Xor a b -> uncurry Xor2 $ sol a b
  where
    sol a b = both (solve2 m) (m Map.! a, m Map.! b)

solve3 :: Map.Map String Entry -> Entry -> Entry3
solve3 m (Orig l) = Rt l
solve3 m e = case e of
  Or a b -> sol (.||) a b
  And a b -> sol (.&&) a b
  Xor a b -> sol (.<+>) a b
  where
    sol f a b = uncurry (Fn f) $ both (solve3 m) (m Map.! a, m Map.! b)

evalEntry3 :: Bools -> Entry3 -> Symbolic SBool
evalEntry3 bs (Rt b) = return $ bs Map.! b
evalEntry3 bs (Fn f a b) = do
  a' <- evalEntry3 bs a
  b' <- evalEntry3 bs b
  return $ f a' b'

deps :: Entry2 -> Set.Set String
deps (Root s) = Set.singleton s
deps (Or2 a b) = Set.union (deps a) (deps b)
deps (And2 a b) = Set.union (deps a) (deps b)
deps (Xor2 a b) = Set.union (deps a) (deps b)

wiredeps :: EMap -> Entry -> Set.Set String
wiredeps m (Orig l) = Set.empty
wiredeps m e = case e of
  Or a b -> Set.unions (Set.fromList (f a b) : sol a b)
  And a b -> Set.unions (Set.fromList (f a b) : sol a b)
  Xor a b -> Set.unions (Set.fromList (f a b) : sol a b)
  where
    f a b = filter ((`notElem` ['x', 'y']) . head) [a, b]
    sol a b = map (wiredeps m) [m Map.! a, m Map.! b]

data Entry3 = Rt String | Fn (SBool -> SBool -> SBool) Entry3 Entry3

instance Show Entry3 where
  show (Rt _) = "Rt <symbolic>"
  show (Fn _ a b) = "Fn (" ++ show a ++ ") (" ++ show b ++ ")"

data Entry2 = Root String | Or2 Entry2 Entry2 | And2 Entry2 Entry2 | Xor2 Entry2 Entry2
  deriving (Show, Eq)

data Entry = Lit Int | Or String String | And String String | Xor String String | Orig String
  deriving (Show, Eq)

parseInput2 :: [String] -> Map.Map String Entry
parseInput2 input = foldl (\m (r, g) -> Map.insert r g m) withtop bottom'
  where
    bottom' = map ((\[g, r] -> (r,) . parsegate $ splitOn " " g) . splitOn " -> ") bottom
    parsegate [a, "AND", b] = And a b
    parsegate [a, "OR", b] = Or a b
    parsegate [a, "XOR", b] = Xor a b
    withtop = foldl (\m [a, _] -> Map.insert a (Orig a) m) Map.empty $ map (splitOn ": ") top
    [top, bottom] = splitOn [""] input

parseInput :: [String] -> EMap
parseInput input = foldl (\m (r, g) -> Map.insert r g m) withtop bottom'
  where
    bottom' = map ((\[g, r] -> (r,) . parsegate $ splitOn " " g) . splitOn " -> ") bottom
    parsegate [a, "AND", b] = And a b
    parsegate [a, "OR", b] = Or a b
    parsegate [a, "XOR", b] = Xor a b
    withtop = foldl (\m [a, b] -> Map.insert a (Lit (read b)) m) Map.empty $ map (splitOn ": ") top
    [top, bottom] = splitOn [""] input
