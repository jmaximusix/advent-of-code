module Year2023.Day7 (part1, part2) where

import Advent (Part (Part1, Part2))
import Data.Bifunctor (bimap)
import Data.List (sort, sortBy)
import Data.List.Extra (group, replace)
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
parseInput part input = bimap (map readCard . maybeReplace) read (hand, bid)
  where
    maybeReplace
      | part == Part1 = id
      | otherwise = replace "J" "*"
    [hand, bid] = words input

readCard :: Char -> Card
readCard 'A' = Ace
readCard 'K' = King
readCard 'Q' = Queen
readCard 'J' = Jack
readCard '*' = Joker
readCard 'T' = Number 10
readCard x | x `elem` ['2' .. '9'] = Number (read [x])
readCard c = error $ "Cant read invalid card: " ++ [c]

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
  | jokers == first && jokers > second = (second + jokers, second)
  | otherwise = (first + jokers, second)
  where
    (first : second') = sortBy (comparing Down) (map length (group $ sort cards))
    second = if null second' then 0 else head second'
    jokers = length $ filter (== Joker) cards
