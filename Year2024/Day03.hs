module Day03 (part1, part2) where

import Data.List.Extra (splitOn)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

data Instruction = Mul Int Int | Dont | Do deriving (Show)

part1, part2 :: [String] -> Int
part1 = solve "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
part2 = solve "mul\\([0-9]{1,3},[0-9]{1,3}\\)|don't\\(\\)|do\\(\\)"

solve :: String -> [String] -> Int
solve pattern = snd . foldl process (True, 0) . parse pattern . unlines

parse :: String -> String -> [Instruction]
parse pattern = map readInstruction . getAllTextMatches . flip (=~) pattern

readInstruction :: String -> Instruction
readInstruction s
  | s == "don't()" = Dont
  | s == "do()" = Do
  | otherwise = Mul a' b'
  where
    [a, b] = splitOn "," s
    a' = read $ drop 4 a
    b' = read $ init b

process :: (Bool, Int) -> Instruction -> (Bool, Int)
process (_, acc) Dont = (False, acc)
process (_, acc) Do = (True, acc)
process (enabled, acc) (Mul a b)
  | enabled = (enabled, acc + a * b)
  | otherwise = (enabled, acc)