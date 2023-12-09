module Day03 (part1, part2) where

import Data.Char (ord)

part1, part2 :: [String] -> Int
part1 = sum . map (head . getcommon . split . nums)
part2 = sum . groupbadges . map nums

nums :: String -> [Int]
nums [] = []
nums (x : xs)
  | ord x > 96 = (ord x - 96) : nums xs
  | ord x < 96 = (ord x - 38) : nums xs

split :: [Int] -> ([Int], [Int])
split list =
  let n = length list `div` 2
   in splitAt n list

getcommon :: ([Int], [Int]) -> [Int]
getcommon (x : xs, list2)
  | x `elem` list2 = x : getcommon (xs, list2)
  | otherwise = getcommon (xs, list2)

groupbadges :: [[Int]] -> [Int]
groupbadges [] = []
groupbadges elves = groupbadge (take 3 elves) : groupbadges (drop 3 elves)

groupbadge :: [[Int]] -> Int
groupbadge [a, b, c] = head $ getcommon (getcommon (a, b), c)