module Day23 (part1, part2) where

import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tuple.Extra (both, dupe, second)
import Debug.Trace (traceShow, traceShowId)
import MyLib (count, countAll, tup2)

part1, part2 :: [String] -> Int
part1 input = traceShow (parties2) sol
  where
    sol = count (any ((== 't') . head) . Set.toList) $ Set.toList parties2
    diffa = Set.difference parties1 parties2
    diffb = Set.difference parties2 parties1
    parties2 = allLanPartiesOfSize 13 conns Set.empty
    parties1 = Set.fromList $ allLanParties conns []
    estmax = maximum $ Map.unionWith (+) estmaxl estmaxr
    estmaxl = countAll $ map fst conns
    estmaxr = countAll $ map snd conns
    conns = map parseInput input
part2 = undefined

password :: Set.Set String -> String
password = intercalate "," . Set.toAscList

parseInput :: String -> (String, String)
parseInput = second tail . splitAt 2

partners :: [(String, String)] -> String -> [String]
partners conns target = left ++ right
  where
    left = map snd $ filter ((== target) . fst) conns
    right = map fst $ filter ((== target) . snd) conns

allLanParties :: [(String, String)] -> [Set.Set String] -> [Set.Set String]
allLanParties [] acc = acc
allLanParties conns acc = allLanParties remaining (party ++ acc)
  where
    remaining = filter (\(a, b) -> not (a == c0 || b == c0)) conns
    party = findLanParty conns c0
    c0 = fst $ head conns

findLanParty :: [(String, String)] -> String -> [Set.Set String]
findLanParty conns p = lans
  where
    lans = nub $ [Set.fromList [p, a, b] | (a, bs) <- soos, b <- bs]
    soos = zip next $ map (filter (`elem` next) . partners conns) next
    next = partners conns p

allLanPartiesOfSize :: Int -> [(String, String)] -> Set.Set (Set.Set String) -> Set.Set (Set.Set String)
allLanPartiesOfSize n conn acc | traceShow (n, length conn, acc) False = undefined
allLanPartiesOfSize _ [] acc = acc
allLanPartiesOfSize n conns acc = allLanPartiesOfSize n remaining (acc `Set.union` Set.fromList party)
  where
    remaining = filter (\(a, b) -> not (a == c0 || b == c0)) conns
    party = lanPartiesOfSize n conns c0
    c0 = fst $ head conns

lanPartiesOfSize :: Int -> [(String, String)] -> String -> [Set.Set String]
lanPartiesOfSize 2 conns p = [Set.fromList [p, a] | a <- partners conns p]
lanPartiesOfSize n conns p = this
  where
    this = nub $ concatMap (`addOne` conns) onesmaller
    onesmaller = lanPartiesOfSize (n - 1) conns p

addOne :: Set.Set String -> [(String, String)] -> [Set.Set String]
-- addOne p c | traceShow (p, c) False = undefined
addOne party conns = soos
  where
    soos = nub $ concatMap (\p -> addOne' party p conns) ps
    ps = Set.toList party

addOne' :: Set.Set String -> String -> [(String, String)] -> [Set.Set String]
addOne' party pmember conns = ps
  where
    ps = map (`Set.insert` party) soos
    soos = filter (isConnectedToAll party conns) next
    next = filter (`Set.notMember` party) $ partners conns pmember

isConnectedToAll :: Set.Set String -> [(String, String)] -> String -> Bool
isConnectedToAll party conns p = all (`elem` ps) party
  where
    ps = partners conns p