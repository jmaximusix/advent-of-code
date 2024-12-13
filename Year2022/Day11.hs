module Day11 (part1, part2) where

import Data.List (sortBy)
import Data.List.Extra (splitOn)
import Data.Ord (Down (Down), comparing)
import MyLib (Part (..), replace)

part1, part2 :: [String] -> Int
part1 = solve Part1 20
part2 = solve Part2 10000

data Monkey = Monkey
  { mid :: Int,
    mop :: String,
    test :: Int,
    tnext :: Int,
    fnext :: Int
  }

solve :: Part -> Int -> [String] -> Int
solve p n =
  product
    . take 2
    . sortBy (comparing Down)
    . last
    . uncurry (playNRounds p n)
    . parseInput

parseInput :: [String] -> ([[Int]], [Monkey])
parseInput input = (items, monkeys)
  where
    (items', monkeys) = unzip (map parseMonkey $ splitOn [""] input)
    lcm' = product $ map test monkeys
    items = items' ++ [[lcm']] ++ [replicate (length monkeys) 0]

parseMonkey :: [String] -> ([Int], Monkey)
parseMonkey [mID, items, op, test', truenext, falsenext] = (newitems, Monkey nid newop ntest tNext fNext)
  where
    newitems = map (read . take 2) (drop 2 (words items))
    newop = last $ splitOn " = " op
    [ntest, tNext, fNext] = map (read . last . words) [test', truenext, falsenext]
    nid = read [mID !! 7]

makeTurn :: Part -> [[Int]] -> Monkey -> [[Int]]
makeTurn p items monkey = incMonkeyCounter mID (length stack) $ foldl (move p monkey) items stack
  where
    mID = mid monkey
    stack = items !! mID

move :: Part -> Monkey -> [[Int]] -> Int -> [[Int]]
move p monkey items i = replace (mid monkey) [] $ replace next ((items !! next) ++ [new]) items
  where
    (new, next) = operation p monkey lcm' i
    lcm' = (head . head . tail . reverse) items

operation :: Part -> Monkey -> Int -> Int -> (Int, Int)
operation part m lcm' i
  | new `mod` test m == 0 = (new, tnext m)
  | otherwise = (new, fnext m)
  where
    new = case part of
      Part1 -> calcNew (mop m) i `div` 3
      Part2 -> calcNew (mop m) i `mod` lcm'

calcNew :: String -> Int -> Int
calcNew instr int
  | op == "*" = select a int * select b int
  | op == "+" = select a int + select b int
  where
    [a, op, b] = words instr

select :: String -> Int -> Int
select x alt
  | x == "old" = alt
  | otherwise = read x

playNRounds :: Part -> Int -> [[Int]] -> [Monkey] -> [[Int]]
playNRounds _ 0 items _ = items
playNRounds p n items monkeys = foldl (makeTurn p) (playNRounds p (n - 1) items monkeys) monkeys

incMonkeyCounter :: Int -> Int -> [[Int]] -> [[Int]]
incMonkeyCounter mID n items = replace (length items - 1) (replace mID i (last items)) items
  where
    i = last items !! mID + n