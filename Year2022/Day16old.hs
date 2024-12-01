module Day16 (solution16) where

-- -- import Data.List.Index(indexed)
-- import Data.Char (isDigit)
-- import Data.List.Extra (splitOn, maximumOn)
-- import Data.Maybe (fromJust, isNothing)
-- import Data.List(find, partition, nub, sort)
-- import Algorithm.Search (bfs)
-- import Debug.Trace(traceShowId, trace, traceShow)
-- import Data.Map (Map, findWithDefault, empty)
-- import qualified Data.Map as Map ((!), fromList, filter, keys, toList, insert, insertWith, empty, lookup, filterWithKey)
-- import Data.Set (Set, member, notMember)
-- import qualified Data.Set as Set(fromList, insert, empty)
-- import Data.Tuple.Extra (thd3, snd3)
-- import Lib(replace)

solution16 :: IO ()
solution16 = do
    mylines <- lines <$> readFile "input16"
    print "hello"
--     let (adj, vs, lAA) = parseValves mylines
--     print $ maximum $ map (\(Done p) -> p) $ nextMoves adj vs Map.empty [State lAA Set.empty 0 30]

-- type Records = Map (Set Int) [(Int, Int, Int)]
-- type Valves = Map Int Int
-- type AdjMatrix = Map (Set Int) Int
-- data State = State {posMe:: Int, posElephant::Int, open::Set Int, pressure::Int, time::Int, cooldownMe::Int, cooldownElephant::Int} | Done Int deriving (Show, Eq)


-- nextMove:: AdjMatrix -> Valves -> State -> [State]
-- nextMove adj vs state
--     | length (open state) == length vs = [Done (pressure state)]
--     | otherwise = [move adj state v | v<-Map.toList vs, fst v `notMember` open state]

-- -- move:: AdjMatrix -> State -> (Int, Int) -> State
-- -- move adj (State l o p t) (l', p')
-- --         | t' > 0 = State l' (Set.insert l' o) (p + p'*t') t'
-- --         | otherwise = Done p
-- --         where
-- --             t' = t - (dist adj l l' + 1)

-- dist:: AdjMatrix -> Int -> Int -> Int
-- dist adj l l' = adj Map.! Set.fromList [l, l']

-- nextMoves:: AdjMatrix -> Valves -> Records -> [State] -> [State]
-- nextMoves _ _ _ [] = []
-- nextMoves adj vs rs s = done' ++  nextMoves adj vs rs' new'
--     where   (done, new) = partition doneFilter $ concatMap (nextMove adj vs) s
--             done' = [maximumOn (\(Done p) -> p) done | not $ null done]
--             new' = filter (\s@(State l le o p t c) -> (l, p, t) `elem` rs' Map.! o) new
--             rs' = foldl (updateRecords adj) rs new

-- doneFilter :: State -> Bool
-- doneFilter (Done _) = True
-- doneFilter (State {}) = False

-- compareStats:: AdjMatrix -> (Int, Int, Int) -> (Int, Int, Int) -> Ordering
-- compareStats adj (l1, p1, t1) (l2, p2, t2)
--     | p1 > p2 && t1 - dt >= t2 = GT
--     | p2 > p1 && t2 - dt >= t1 = LT
--     | p1 == p2 && t1 - dt > t2 = GT
--     | p1 == p2 && t2 - dt > t1 = LT
--     | otherwise = EQ
--     where dt = dist adj l1 l2

-- updateRecords:: AdjMatrix -> Records -> State -> Records
-- updateRecords adj rs (State l o p t) = Map.insertWith update o [(l, p, t)] rs
--     where update = foldl (\(a:as) b -> if compareStats adj a b == GT then a:as else a:b:as)

-- parseValves:: [String] -> (AdjMatrix, Valves, Int)
-- parseValves s = (Map.fromList adjmatrix, valves, lAA)
--     where
--         indices = [0..length s - 1]
--         lAA = namemap Map.! "AA"
--         namemap = Map.fromList $ zip names indices
--         names = map (\x -> words x !! 1) s
--         flow = map (\x -> read $ takeWhile isDigit $ splitOn "=" (words x !! 4) !! 1) s
--         adjacent = Map.fromList $ zip indices $ map (\x -> map ((namemap Map.!) . take 2) (drop 9 $ words x)) s
--         valves = Map.filter (>0) $ Map.fromList (zip indices flow)
--         relevant = lAA : Map.keys valves
--         adjmatrix = [(Set.fromList [a, b], length $ fromJust (bfs (adjacent Map.!) (==b) a)) | a <- relevant, b<-relevant]


