{-# LANGUAGE TupleSections #-}

module Day17 (part1, part2) where

import Data.Bits (xor)
import Data.List (intercalate)
import Data.List.Extra (dropEnd, splitOn)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import MyLib.Utils (readNumbers)

part1 :: [String] -> String
part1 = outToString . executeAll 0 [] . parseInput

part2 :: [String] -> Int
part2 input = minimum laststates
  where
    laststates = map recombine $ allnextStates (map (,[]) firststates') (tail seqtoproduce) programtoexec
    firststates' = firststates (head seqtoproduce) programtoexec
    seqtoproduce = programpart2 input
    programtoexec = dropEnd 2 seqtoproduce

recombine :: (Int, [Int]) -> Int
recombine (i, icum) = foldl (\x c -> x * 8 + c) i icum

allnextStates :: [(Int, [Int])] -> [Int] -> [Int] -> [(Int, [Int])]
allnextStates is os program = foldl (\is' o -> concatMap (\i -> nextStates o i program) is') is os

firststates :: Int -> [Int] -> [Int]
firststates i program = map fst $ filter ((== i) . snd) $ map (\i' -> (i', head $ executeAll 0 [] ([i', 0, 0], program))) [0 .. 1023]

nextStates :: Int -> (Int, [Int]) -> [Int] -> [(Int, [Int])]
nextStates out (i, icum) program = map fst $ filter ((== out) . snd) $ map ((\(i', icum') -> ((i', icum'), head $ executeAll 0 [] ([i', 0, 0], program))) . \i' -> (i' * 128 + (i `div` 8), (i `mod` 8) : icum)) [0 .. 7]

type Instruction = Int

type Operand = Int

type Registers = [Int]

outToString :: [Int] -> String
outToString = intercalate "," . map show

executeAll :: Int -> [Int] -> (Registers, [Int]) -> [Int]
executeAll ptr out (rs, program)
  | ptr > (length program - 2) = out
  | otherwise = executeAll newptr out' (newrs, program)
  where
    newptr = fromMaybe (ptr + 2) maybenewptr
    [i, o] = take 2 $ drop ptr program
    out' = if isNothing maybeout then out else out ++ [fromJust maybeout]
    (newrs, maybeout, maybenewptr) = execute rs (i, o)

execute :: Registers -> (Instruction, Operand) -> (Registers, Maybe Int, Maybe Int)
execute rs@[a, b, c] (instr, op) = case instr of
  0 -> ([a `div` (2 ^ comboopcode rs op), b, c], Nothing, Nothing)
  1 -> ([a, b `xor` op, c], Nothing, Nothing)
  2 -> ([a, comboopcode rs op `mod` 8, c], Nothing, Nothing)
  3 -> if a == 0 then (rs, Nothing, Nothing) else (rs, Nothing, Just op)
  4 -> ([a, b `xor` c, c], Nothing, Nothing)
  5 -> (rs, Just (comboopcode rs op `mod` 8), Nothing)
  6 -> ([a, a `div` (2 ^ comboopcode rs op), c], Nothing, Nothing)
  7 -> ([a, b, a `div` (2 ^ comboopcode rs op)], Nothing, Nothing)

comboopcode :: Registers -> Operand -> Int
comboopcode [a, b, c] op = case op of
  0 -> 0
  1 -> 1
  2 -> 2
  3 -> 3
  4 -> a
  5 -> b
  6 -> c
  7 -> error "Invalid combo opcode"

programpart2 :: [String] -> [Int]
programpart2 = readNumbers . head . last . splitOn [""]

parseInput :: [String] -> (Registers, [Int])
parseInput input = (map (head . readNumbers) registers, instructions)
  where
    instructions = readNumbers $ head program
    [registers, program] = splitOn [""] input