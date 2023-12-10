module Day13 (part1, part2) where

import Data.Function (on)
import Data.List (elemIndex, elemIndices, find, sortBy)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust, isNothing)

data Packet = List [Packet] | Value Int deriving (Show, Eq)

part1, part2 :: [String] -> Int
part1 =
  sum
    . map succ
    . elemIndices LT
    . map (\(x1 : x2 : _) -> (compareTrees `on` readTree) x1 x2)
    . chunksOf 3
part2 input = product $ map (succ . fromJust . \x -> elemIndex x sorted) packages
  where
    packages = ["[[2]]", "[[6]]"]
    sorted = sortBy (compareTrees `on` readTree) $ filter (not . null) (input ++ packages)

readTree :: String -> Packet
readTree s
  | null $ head elems = List []
  | (length elems == 1) && (head elem0 /= '[') = Value (read elem0)
  | otherwise = List $ map readTree elems
  where
    elems = splitElems 0 ([], ([], strip s))
    elem0 = head elems

splitElems :: Int -> ([String], (String, String)) -> [String]
splitElems _ (p, (a, [])) = p ++ [a]
splitElems n (p, (a, x : xs))
  | x == '[' = splitElems (n + 1) new
  | x == ']' = splitElems (n - 1) new
  | n == 0 && x == ',' = splitElems n (p ++ [a], ([], xs))
  | otherwise = splitElems n new
  where
    new = (p, (a ++ [x], xs))

compareTrees :: Packet -> Packet -> Ordering
compareTrees (Value a) (Value b) = compare a b
compareTrees (List as) (List bs)
  | isNothing o = EQ
  | otherwise = fromJust o
  where
    o = find (/= EQ) $ zipWith compareTrees as bs ++ [compare (length as) (length bs)]
compareTrees (Value a) b = compareTrees (List [Value a]) b
compareTrees a (Value b) = compareTrees a (List [Value b])

strip :: String -> String
strip ('[' : s) = (reverse . drop 1 . reverse) s
strip s = s