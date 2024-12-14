module MyLib.Utils (count, countEq, replace, tup2, deleteAt, readNumbers, countAll) where

import Data.Char (isDigit)
import Data.List.Extra (groupOnKey)
import qualified Data.Map.Strict as Map

replace :: Int -> a -> [a] -> [a]
replace i new list = take i list ++ (new : drop (i + 1) list)

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f list = take i list ++ (f (list !! i) : drop (i + 1) list)

deleteAt :: Int -> [a] -> [a]
deleteAt i list = take i list ++ drop (i + 1) list

-- reads continuous digits from strings, e.g. "abc123def456" -> [123, 456]
readNumbers :: String -> [Int]
readNumbers = map (read . snd) . filter fst . groupOnKey (\x -> isDigit x || x == '-')

countEq :: (Eq a) => a -> [a] -> Int
countEq x = count (== x)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

tup2 :: [a] -> (a, a)
tup2 [a, b] = (a, b)

countAll :: (Ord a) => [a] -> Map.Map a Int
countAll = Map.fromListWith (+) . flip zip (repeat 1)