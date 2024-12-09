module Day09 (part1, part2) where

import Data.List (delete, find, findIndex, inits, uncons)
import Data.List.Extra (chunksOf, unsnoc)
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Debug.Trace (traceShow, traceShowId)
import Geometry (Direction, Grid, Pos, getGridElementSafe, getGridElementWithDefault, goNSteps, index2d, replace2d, toPos, toV2, zipPoints)
import MyLib (tup2)

-- fileid, length of file, free space following
type File = (Int, (Int, Int))

part1, part2 :: [String] -> Int
part1 input = sum $ zipWith (*) [0 ..] $ fst reordered
  where
    filelist = parseInput (head input ++ "0")
    freeSpace = sum $ map (snd . snd) filelist
    stream = concatMap blockStream filelist
    splitPoint = length stream - freeSpace
    (before, after) = splitAt splitPoint stream
    reordered = foldl foldFunc ([], catMaybes (reverse after)) before
part2 input = sum $ zipWith (*) [0 ..] $ map (\x -> fromMaybe 0 x) $ concatMap blockStream reordered
  where
    filelist = parseInput (head input ++ "0")
    reordered = foldl tryFit filelist (reverse filelist)

parseInput :: String -> [File]
parseInput = zip [0 ..] . map tup2 . chunksOf 2 . map (\x -> read [x])

blockStream :: File -> [Maybe Int]
blockStream (fileid, (len, freeSpace)) = replicate len (Just fileid) ++ replicate freeSpace Nothing

foldFunc :: ([Int], [Int]) -> Maybe Int -> ([Int], [Int])
foldFunc (merged, backup) Nothing = (merged ++ [head backup], tail backup)
foldFunc (merged, backup) (Just x) = (merged ++ [x], backup)

tryFit :: [File] -> File -> [File]
-- tryFit files file | traceShow (file, files) False = undefined
tryFit files f@(fid, _)
  | succeeded = soos ++ [(id'', (l'', 0)), (id', (l', f'' - l' + offset))] ++ delrest
  | otherwise = files
  where
    file@(id', (l', _)) = fromJust $ find (\(id, _) -> id == fid) files
    soos = takeWhile (\(id, (_, f)) -> f < l' && id' /= id) files
    maybeyeet = uncons $ drop (length soos) files
    (next@(id'', (l'', f'')), rest) = fromJust maybeyeet
    succeeded = isJust maybeyeet && id'' /= id'
    (delrest, offset) = deleteFileFromRest rest file

deleteFileFromRest :: [File] -> File -> ([File], Int)
deleteFileFromRest [] _ = ([], 0)
-- deleteFileFromRest rest f | traceShow (rest, f) False = undefined
deleteFileFromRest rest file@(id, (l, f))
  | isNothing yeet = (rest, 0)
  | isNothing $ unsnoc before = (after, f + l)
  | otherwise = (before' ++ [(did, (dl, df + f + l))] ++ after, 0)
  where
    yeet = findIndex (\(id', _) -> id' == id) rest

    soos = splitWhen (\(id', _) -> id' == id) rest
    [before, after] = soos
    -- [(bid, (bl, bf))] = head soos
    (before', (did, (dl, df))) = fromJust $ unsnoc before