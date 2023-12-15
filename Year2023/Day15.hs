module Day15 (part1, part2) where

import Data.Char (ord)
import Data.List.Extra (replace, splitOn)
import qualified Data.Map as Map

type Lens = (String, Int)

part1, part2 :: [String] -> Int
part1 = sum . map hash . splitOn "," . head
part2 = sum . Map.mapWithKey focusingPower . foldl operate Map.empty . splitOn "," . head

operate :: Map.Map Int [Lens] -> String -> Map.Map Int [Lens]
operate m s = Map.alter exec (hash label) m
  where
    (label, op : val) = break (\c -> c == '-' || c == '=') s
    exec = case op of
      '-' -> (filter ((/= label) . fst) <$>)
      '=' -> replaceLens (label, read val)

replaceLens :: (String, Int) -> Maybe [Lens] -> Maybe [Lens]
replaceLens new Nothing = Just [new]
replaceLens new (Just l) = case lookup (fst new) l of
  Nothing -> Just $ l ++ [new]
  Just oldv -> Just $ replace [(fst new, oldv)] [new] l

focusingPower :: Int -> [Lens] -> Int
focusingPower box = ((box + 1) *) . sum . zipWith (\i (_, f) -> f * i) [1 ..]

hash :: String -> Int
hash = let h s c = (s + ord c) * 17 `mod` 256 in foldl h 0