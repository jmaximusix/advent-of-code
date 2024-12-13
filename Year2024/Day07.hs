module Day07 (part1, part2) where

import MyLib (Part (..))

part1, part2 :: [String] -> Int
part1 = sum . map fst . filter (uncurry (canCreateFrom Part1)) . map parseInput
part2 = sum . map fst . filter (uncurry (canCreateFrom Part2)) . map parseInput

parseInput :: String -> (Int, [Int])
parseInput = (\(r : ns) -> (read (init r), reverse (map read ns))) . words

canCreateFrom :: Part -> Int -> [Int] -> Bool
canCreateFrom _ res [n] = res == n
canCreateFrom p res (n : ns)
  | canCreateFrom p (res - n) ns = True
  | res `rem` n == 0 && canCreateFrom p (res `div` n) ns = True
  | p == Part2 && res `rem` t == n = canCreateFrom p (res `div` t) ns
  | otherwise = False
  where
    t = 10 ^ length (show n)