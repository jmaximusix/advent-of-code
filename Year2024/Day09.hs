module Day09 (part1, part2) where

import Data.Char (digitToInt)
import Data.List (find, partition)
import Data.List.Extra (chunksOf)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import MyLib (count, deleteAt, replace)

part1, part2 :: [String] -> Int
part1 input = sum (map (\(i, Just fid) -> i * fid) untouched) + sum (zipWith (*) (map fst spaces) (reverse (catMaybes right)))
  where
    blocks = concat . zipWith (\fid [l, s] -> replicate l (Just fid) ++ replicate s Nothing) [0 ..] . parseChunks $ input
    splitIdx = count isJust blocks
    (left, right) = splitAt splitIdx blocks
    (untouched, spaces) = partition (isJust . snd) $ zip [0 ..] left
part2 input = fst $ foldl moveAndChecksum (0, spaces) files
  where
    (files, spaces) = fst . foldl f (([], []), 0) . zip [0 ..] . parseChunks $ input
    f ((fs, sp), idx) (fid, [l, s]) = (((idx, (fid, l)) : fs, sp ++ [(idx + l, s)]), idx + l + s)

parseChunks :: [String] -> [[Int]]
parseChunks = chunksOf 2 . (++ [0]) . map digitToInt . head

moveAndChecksum :: (Int, [(Int, Int)]) -> (Int, (Int, Int)) -> (Int, [(Int, Int)])
moveAndChecksum (acc, spaces) (fidx, f@(_, fl))
  | isNothing maybesp || sidx > fidx = (acc + checksum fidx f, spaces)
  | otherwise = (acc + checksum sidx f, adjspaces (sidx + fl, w - fl))
  where
    maybesp = find ((fl <=) . snd . fst) $ zip spaces [0 ..]
    ((sidx, w), spaceidx) = fromJust maybesp
    adjspaces sp'
      | snd sp' == 0 = deleteAt spaceidx spaces
      | otherwise = replace spaceidx sp' spaces
    checksum i' (f', l') = f' * (i' * l' + ((l' * (l' - 1)) `div` 2))