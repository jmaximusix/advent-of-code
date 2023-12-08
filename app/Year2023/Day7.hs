module Year2023.Day7 (part1, part2) where

import Advent (Part (Part1, Part2))
import Data.Bifunctor (bimap)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (Down), comparing)

data Card = Joker | Number Int | Jack | Queen | King | Ace deriving (Eq, Show, Ord)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)

part1, part2 :: [String] -> Int
part1 = solve Part1
part2 = solve Part2

solve :: Part -> [String] -> Int
solve part =
  sum
    . zipWith (*) [1 ..]
    . map snd
    . sortBy (\(a, _) (b, _) -> compareHands a b)
    . map (parseInput part)

parseInput :: Part -> String -> ([Card], Int)
parseInput part input = bimap (map (readCard part)) read (hand, bid)
  where
    [hand, bid] = words input

readCard :: Part -> Char -> Card
readCard _ 'A' = Ace
readCard _ 'K' = King
readCard _ 'Q' = Queen
readCard p 'J' = case p of
  Part1 -> Jack
  Part2 -> Joker
readCard _ 'T' = Number 10
readCard _ x | x `elem` ['2' .. '9'] = Number (read [x])
readCard _ c = error $ "Cant read invalid card: " ++ [c]

compareHands :: [Card] -> [Card] -> Ordering
compareHands hand1 hand2
  | ht1 == ht2 = (head . dropWhile (== EQ) . zipWith compare hand1) hand2
  | otherwise = compare ht1 ht2
  where
    [ht1, ht2] = map handType [hand1, hand2]

handType :: [Card] -> HandType
handType cards
  | mostc == 5 = FiveOfAKind
  | mostc == 4 = FourOfAKind
  | mostc2 == (3, 2) = FullHouse
  | mostc == 3 = ThreeOfAKind
  | mostc2 == (2, 2) = TwoPair
  | mostc == 2 = OnePair
  | otherwise = HighCard
  where
    mostc2@(mostc, _) = mostCommonCards cards

mostCommonCards :: [Card] -> (Int, Int)
mostCommonCards cards
  | jokers > second = (second + jokers, second)
  | otherwise = (first + jokers, second)
  where
    (first : second : _) = sortBy (comparing Down) (map length (group $ sort cards) ++ [0])
    jokers = length $ filter (== Joker) cards
