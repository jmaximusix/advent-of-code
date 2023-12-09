module MyLib (mostRecentChallenge, readPart, Date, replace, replace2d) where

import Advent (Day, Part (Part1, Part2), mkDay_)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime
  ( LocalTime (localDay),
    hoursToTimeZone,
    utcToLocalTime,
  )

type Date = (Day, Integer)

mostRecentChallenge :: IO Date
mostRecentChallenge = do
  now <- getCurrentTime
  let localTime = utcToLocalTime (hoursToTimeZone (-5)) now
  let (year, month, day) = toGregorian $ localDay localTime
  let year' = if month /= 12 then year - 1 else year
  let day' = if month /= 12 || day > 25 then 25 else fromIntegral day
  return (mkDay_ day', year')

readPart :: String -> Part
readPart "1" = Part1
readPart "2" = Part2
readPart "a" = Part1
readPart "b" = Part2
readPart _ = error "Invalid part. Use 1, 2, a, or b."

replace2d :: (Int, Int) -> a -> [[a]] -> [[a]]
replace2d (x, y) new grid = replace x (replace y new (grid !! x)) grid

replace :: Int -> a -> [a] -> [a]
replace i new list = take i list ++ [new] ++ drop (i + 1) list