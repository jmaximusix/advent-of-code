module Day07 (part1, part2) where

import Data.List (elemIndex)
import Data.List.Split (split, splitOn, whenElt)
import Data.Maybe (fromMaybe)

part1, part2 :: [String] -> Int
part1 = sum . filter (< 100000) . fst . parseInput
part2 input = minimum $ filter (>= (30000000 + total - 70000000)) sizes
  where
    (sizes, total) = parseInput input

parseInput :: [String] -> ([Int], Int)
parseInput ls = (map (size formatted) dirslist, size formatted "dir &/")
  where
    formatted = format (filter (not . null) $ split (whenElt (\x -> take 3 x == "$ c")) ls) ""
    dirslist = map head $ filter (\x -> head x /= "$ ls") formatted

format :: [[String]] -> String -> [[String]]
format [] _ = []
format (x : xs) wd
  | head x == "$ cd .." = format xs $ take (length wd - l) wd
  | take 3 (head x) == "$ c" = ["dir " ++ nwd] : format xs nwd
  | head x == "$ ls" = map (formatEntry wd) x : format xs wd
  where
    nwd = wd ++ "&" ++ drop 5 (head x)
    l = length (last (splitOn "&" wd)) + 1

formatEntry :: String -> String -> String
formatEntry wd entry
  | a == "dir" = a ++ " " ++ wd ++ "&" ++ b
  | otherwise = entry
  where
    [a, b] = words entry

size :: [[String]] -> String -> Int
size _ "$ ls" = 0
size tree dir
  | a == "dir" = sumsize tree (tree !! (fromMaybe (-2) (elemIndex [dir] tree) + 1))
  | otherwise = read a
  where
    a = head $ words dir

sumsize :: [[String]] -> [String] -> Int
sumsize tree folder = sum (map (size tree) folder)
