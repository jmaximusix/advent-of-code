module Day03 (part1, part2) where

import Data.List.Extra (splitOn)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

part1, part2 :: [String] -> Int
part1 = solve "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
part2 = solve "mul\\([0-9]{1,3},[0-9]{1,3}\\)|don't\\(\\)|do\\(\\)"

solve :: String -> [String] -> Int
solve pattern =
  snd
    . foldl process (True, 0)
    . getAllTextMatches
    . ((=~ pattern) :: String -> AllTextMatches [] String)
    . unlines

process :: (Bool, Int) -> String -> (Bool, Int)
process (_, acc) "don't()" = (False, acc)
process (_, acc) "do()" = (True, acc)
process (enabled, acc) s
  | enabled = (enabled, acc + a' * b')
  | otherwise = (enabled, acc)
  where
    [a, b] = splitOn "," s
    a' = read $ drop 4 a
    b' = read $ init b
