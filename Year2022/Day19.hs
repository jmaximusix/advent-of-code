module Day19 (solution19) where

import Data.Map (Map)
import qualified Data.Map as Map ((!), empty, insertWith)
import Data.Maybe (mapMaybe)
import Data.Set (Set, member)
import qualified Data.Set as Set (elemAt, filter, fromList, map, singleton, union, unions)
import Linear.Matrix (M44, (!*))
import Linear.Metric (dot)
import Linear.V4 (V4 (..))
import Linear.Vector (basis)
import Text.Read (readMaybe)

solution19 :: IO ()
solution19 = do
    mylines <- lines <$> readFile "input19"
    let blueprints = map readBlueprint mylines
    let f n blueprint = maximum
            $ Set.map (\x -> V4 0 0 0 1 `dot` inventory x) 
            $ nextMinutes n 
            (Map.empty, Set.singleton (State blueprint (V4 1 0 0 0) (V4 0 0 0 0)))
    let indices = [1 .. length blueprints]
    print $ sum $ zipWith (*) indices $ map (f 24) blueprints
    print $ product $ map (f 32) $ take 3 blueprints

type Records = Map (V4 Int) (Set (V4 Int))
data State = State {cost :: M44 Int, robots :: V4 Int, inventory :: V4 Int} deriving (Show, Eq, Ord)

nextFactoryMinute :: State -> Set State
nextFactoryMinute s@(State c r i) = Set.fromList $ if length new < 4 then s : new else new
  where
    new = [s{robots = r + t, inventory = i' t - t} | t <- basis, isUseful t, canafford t]
    canafford t = all (>= 0) (i' t)
    isUseful t = (r `dot` t) < (m `dot` t)
    i' t = i - (c !* t)
    m = maximum <$> c + V4 0 0 0 32

collectOres :: State -> State
collectOres state = state{inventory = inventory state + robots state}

compareVec :: V4 Int -> V4 Int -> Ordering
compareVec v1 v2
    | v2 == v1 = EQ
    | all (>= 0) (v1 - v2) = GT
    | all (>= 0) (v2 - v1) = LT
    | otherwise = EQ

nextMinutes :: Int -> (Records, Set State) -> Set State
nextMinutes 0 (_, states) = states
nextMinutes n states = nextMinutes (n - 1) (nextMinute states)

nextMinute :: (Records, Set State) -> (Records, Set State)
nextMinute (rs, states) = (rs', newstates'')
  where
    newstates = Set.unions $ Set.map nextFactoryMinute states
    newstates' = Set.map collectOres newstates
    rs' = foldl updateRecords rs newstates'
    newstates'' = Set.filter (\s -> inventory s `member` (rs' Map.! robots s)) newstates'

updateRecords :: Records -> State -> Records
updateRecords rs state = Map.insertWith update (robots state) (Set.singleton (inventory state)) rs
  where
    update new old
        | all (\x -> compareVec (Set.elemAt 0 new) x == LT) old = old
        | otherwise = Set.filter (\x -> compareVec x (Set.elemAt 0 new) /= LT) $ Set.union new old

readBlueprint :: String -> M44 Int
readBlueprint s = V4 (V4 a b c e) (V4 0 0 d 0) (V4 0 0 0 f) (V4 0 0 0 0)
  where
    [a, b, c, d, e, f] = mapMaybe readMaybe $ words s