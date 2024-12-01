module Day22 (solution22) where

import Linear (V2(..), distance, V3(..), cross, basis)
import Data.List.Extra(splitOn)
import Data.List.Index(indexed)
import Data.List(elemIndices, partition, transpose, (\\))
import Data.Set(Set)
import qualified Data.Set as Set(fromList, insert, empty, union, findMin, findMax, member, (\\))
import Data.Char (isDigit)
import Debug.Trace(traceShowId, traceShow, trace)
import Data.Tuple.Extra (thd3)

l::Int
l = 4

solution22 :: IO ()
solution22 = do
    mylines <- lines <$> readFile "test22"
    -- putStr $ unlines $ readFace mylines (2,0)
    let [g, instr] = splitOn [[]] mylines
    let [yr, xr] = map (pred . (`div` l)) [length g, maximum $ map length g]
    let grid = toContinuousPath [] [(V2 x y, readFace g (x, y)) | y<-[0..yr], x<-[0..xr], (not . null) (readFace g (x, y))]
    let cube = foldl buildCube [] grid
    mapM_ (print . (\(a, b, c) -> (a, b))) cube
    print $ fst $ foldl followInstruct ((V2 0 0, cube), directions) (parseInstructions ('R':head instr))
    -- let grid = map parseLine g
    -- let instructions = parseInstructions ('R':head instr)
    -- print instructionso
    -- print $ eval (foldl (followInstruct grid) (50, 0, directions) instructions)
    --print $ eval (followInstruct grid (57, 7, D:L:directions) ('R', 4))

-- eval :: Loc -> (Loc, Int)
-- eval (x, y, d:ds) = ((x, y, [d]), 1000*(y+1)+4*(x+1) + (if d == R then 0 else if d == D then 1 else if d == L then 2 else 3))

type Face = [[Char]]
type Corners = (V3 Int, V3 Int, V3 Int, V3 Int) -- ecken sind   1 2     a b 
                                                --              4 3     d c
type Side = (V2 Int, Corners, Face)
type Location = (V2 Int, [Side])
type Instruct = (Char, Int)
type Direction = V2 Int

directions :: [Direction]
directions = drop 3 $ cycle $ basis ++ map negate basis

buildCube:: [Side] -> (V2 Int, Face) -> [Side]
buildCube [] (p, f) = [(p, (V3 0 0 0, V3 1 0 0, V3 1 1 0, V3 0 1 0), f)]
buildCube ss (p@(V2 x y), f) = (p, corners, f):ss
    where
        corners = buildCorners (p-p') corners'
        (p', corners', _) = head $ filter (\(V2 x' y', _, _) -> (abs (y' - y) + abs (x' - x)) == 1) ss

buildCorners:: V2 Int -> Corners ->  Corners
buildCorners v (a, b, c, d) = case v of
        V2 1 0 -> (b, cr b c a, c, cr c b d)
        V2 (-1) 0 -> (cr a d b, a, cr d a c, d)
        V2 0 1 -> (d, c, cr d c a, cr c d b)
        V2 0 (-1) -> (cr a b d, cr b a c, a, b)
    where   cr v1 v2 v3 = (`mod` 2) <$> v1 + ((v2 - v1) `cross` (v3 - v1))

toContinuousPath:: [(V2 Int, Face)] -> [(V2 Int, Face)] -> [(V2 Int, Face)]
toContinuousPath [] (l:ls) = toContinuousPath [l] ls
toContinuousPath connected [] = connected
toContinuousPath connected remaining = toContinuousPath (connected ++ done) rem'
    where (done, rem') = partition (\(V2 x y, _) -> any (\(V2 x' y', _) -> abs (y' - y) + abs (x' - x) == 1) connected) remaining

readFace:: [[Char]] -> (Int, Int) -> Face
readFace input (x, y)
    | hasFace = (take l . map (take l) . drop y' . map (drop x')) input
    | otherwise = []
    where 
        hasFace = y' < length input && x' < length (input !! y') && (input !! y' !! x') `elem` ['#', '.']
        (y', x') = (y*l, x*l)

followInstruct:: (Location, [Direction]) -> Instruct -> (Location, [Direction])
followInstruct (l, dirs) (turn, n) = (p', dirs')
    where   p' = move l (head dirs') n
            dirs' = if turn == 'R' then drop 1 dirs else drop 3 dirs

move:: Location -> Direction -> Int -> Location
move l _ 0 = l
move l dir n
    | isBlocked p' = l
    | otherwise = move (p', s') dir (n-1)
    where   (p', s') = newCoords l dir
            isBlocked (V2 x y) = thd3 (head s') !! x !! y == '#'

newCoords:: Location -> Direction -> Location
newCoords (p, s:ss) d
    | inRange (p+d) = (p+d, s:ss)
    | otherwise = ((`mod` l) <$> (p+d), turned)
    where   inRange (V2 x y) = x >= 0 && x <l && y >= 0 && y < l
            turned = turnCube (s:ss) edge
            edge = findEdge s (p+d)
            
turnCube:: [Side] -> (V3 Int, V3 Int, Int) -> [Side]
turnCube sides edge | traceShow (sides, edge) False = undefined
turnCube sides (c1, c2, i) = rotated : sides'
    where 
        (s:sides') = take (length sides) $ dropWhile (not . matches) $ cycle (tail sides ++ [head sides])
        aligned = rotateFace s (c1, c2)
        rotated = case i of 
                0 -> aligned
                1 -> rotateCCW aligned
                2 -> rotateCCW $ rotateCCW aligned
                3 -> rotateCCW $ rotateCCW $ rotateCCW aligned
        matches (_, (a, b, c, d), _) = length ([a, b, c, d] \\ [c1, c2]) == 2
        
rotateFace:: Side -> (V3 Int, V3 Int) -> Side
rotateFace s@(_, (a, b, _, _), _) (c1, c2)
    | c1 == a && c2 == b = s
    | otherwise = rotateFace (rotateCCW s) (c1, c2)

rotateCCW:: Side -> Side
rotateCCW (v, (a, b, c, d), f) = (v, (b, c, d, a), reverse $ transpose f)

findEdge:: Side -> V2 Int -> (V3 Int, V3 Int, Int)
findEdge (_, (a, b, c, d), _) (V2 x y)
    | x >= l = (b, c, 1)
    | x < 0 = (d, a, 3)
    | y >= l = (c, d, 0)
    | y < 0 = (a, b, 2)

parseInstructions:: String -> [Instruct]
parseInstructions [] = []
parseInstructions s = (turn, read n) : parseInstructions (drop (length n + 1) s)
    where   n = takeWhile isDigit $ tail s
            turn = head s