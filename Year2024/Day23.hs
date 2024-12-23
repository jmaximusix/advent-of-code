module Day23 (part1, part2) where

import Data.List (find, intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (second)

type Connections = Map.Map String (Set String)

part1 :: [String] -> Int
part1 = Set.size . allPartiesWithChief . parseInput

part2 :: [String] -> String
part2 = intercalate "," . Set.toAscList . largestParty . parseInput

parseInput :: [String] -> Connections
parseInput = foldl ins Map.empty . map (second tail . splitAt 2)
  where
    ins m (a, b) = ins' a b (ins' b a m)
    ins' a b = Map.insertWith Set.union a (Set.singleton b)

allPartiesWithChief :: Connections -> Set (Set String)
allPartiesWithChief conns =
  Set.unions
    . map (partiesWithMember 3 conns)
    . filter ((== 't') . head)
    . Map.keys
    $ conns

largestParty :: Connections -> Set String
largestParty conns = solve maxsize
  where
    solve n
      | isNothing maybeparties = solve (n - 1)
      | otherwise = Set.findMin $ fromJust maybeparties
      where
        maybeparties = find (not . null) $ map (partiesWithMember n conns) $ Map.keys longest
        longest = Map.filter ((>= n) . length) conns
    maxsize = maximum $ Map.map length conns

partiesWithMember :: Int -> Connections -> String -> Set (Set String)
partiesWithMember 2 conns p = Set.map (\a -> Set.fromList [p, a]) $ conns Map.! p
partiesWithMember n conns p = Set.unions $ Set.map tryAddNeighbors $ partiesWithMember (n - 1) conns p
  where
    tryAddNeighbors party = Set.unions $ Set.map (\m -> tryAdd party m conns) party

tryAdd :: Set String -> String -> Connections -> Set (Set String)
tryAdd party pmember conns = Set.map (`Set.insert` party) successful
  where
    successful = Set.filter (null . Set.difference party . (conns Map.!)) potential
    potential = Set.difference (conns Map.! pmember) party