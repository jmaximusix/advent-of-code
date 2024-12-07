module Day07 (part1, part2) where

import Advent (Part (..))
import Data.List (isSuffixOf)

part1, part2 :: [String] -> Int
part1 = sum . map fst . filter (uncurry (canCreateFrom Part1)) . map parseInput
part2 = sum . map fst . filter (uncurry (canCreateFrom Part2)) . map parseInput

parseInput :: String -> (Int, [Int])
parseInput = (\(r : ns) -> (read (init r), reverse (map read ns))) . words

canCreateFrom :: Part -> Int -> [Int] -> Bool
canCreateFrom _ res [n] = res == n
canCreateFrom p res (n : ns)
  | canCreateFrom p (res - n) ns = True
  | res `mod` n == 0 && canCreateFrom p (res `div` n) ns = True
  | p == Part2 && (n' `isSuffixOf` tail r') = canCreateFrom p unconcat ns
  | otherwise = False
  where
    unconcat = read (take (length r' - length n') r')
    r' = show res
    n' = show n