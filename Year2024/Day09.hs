module Day09 (part1, part2) where

import Data.Bifunctor (bimap, second)
import Data.List (find, findIndex, uncons)
import Data.List.Extra (chunksOf, unsnoc)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Debug.Trace (traceShow, traceShowId)
import MyLib (tup2)

-- ((fileid, length of file), free space following)
type File = ((Int, Int), Int)

part1, part2 :: [String] -> Int
part1 input = sum $ zipWith (*) [0 ..] $ fst reordered
  where
    filelist = parseInput (head input ++ "0")
    freeSpace = sum $ map snd filelist
    stream = concatMap blockStream filelist
    splitPoint = length stream - freeSpace
    (before, after) = splitAt splitPoint stream
    reordered = foldl foldFunc ([], reverse (catMaybes after)) before
part2 input = fst $ foldl calcRes (0, 0) reordered
  where
    filelist = parseInput (head input ++ "0")
    reordered = foldl tryFit filelist (reverse [0 .. length filelist - 1])

parseInput :: String -> [File]
parseInput = zipWith (\i [l, s] -> ((i, l), s)) [0 ..] . chunksOf 2 . map (\x -> read [x])

blockStream :: File -> [Maybe Int]
blockStream ((fileid, len), freeSpace) = replicate len (Just fileid) ++ replicate freeSpace Nothing

foldFunc :: ([Int], [Int]) -> Maybe Int -> ([Int], [Int])
foldFunc (merged, backup) Nothing = (merged ++ [head backup], tail backup)
foldFunc (merged, backup) (Just x) = (merged ++ [x], backup)

tryFit :: [File] -> Int -> [File]
tryFit files fid
  | isNothing spaceidx = files
  | fromJust spaceidx == fileidx - 1 = before' ++ [(sp, 0), (f, sps + fs)] ++ after
  | otherwise = before' ++ [(sp, 0), (f, sps - fl)] ++ between' ++ [(bf, bfs + fl + fs)] ++ after
  where
    fileidx = fromJust $ findIndex (\((id', _), _) -> id' == fid) files
    (before, (f@(_, fl), fs) : after) = splitAt fileidx files
    spaceidx = findIndex (\s -> snd s >= fl) before
    (before', (sp, sps) : between) = splitAt (fromJust spaceidx) before
    (between', (bf, bfs)) = fromJust $ unsnoc between

calcRes :: (Int, Int) -> File -> (Int, Int)
calcRes (acc, idx) ((f, l), s) = (acc + f * (idx * l + ((l * (l - 1)) `div` 2)), idx + l + s)