module Day02 (part1, part2) where

part1, part2 :: [String] -> Int
part1 = solve evaluateSafety
part2 =
  let removeAny l = [take i l ++ drop (i + 1) l | i <- [0 .. length l - 1]]
   in solve (\l -> evaluateSafety l || any evaluateSafety (removeAny l))

solve :: ([Int] -> Bool) -> [String] -> Int
solve evalfunc = length . filter id . map (evalfunc . map read . words)

evaluateSafety :: [Int] -> Bool
evaluateSafety list =
  let gaps = zipWith (-) (tail list) list
   in all (\x -> abs x >= 1 && abs x <= 3) gaps && (all (> 0) gaps || all (< 0) gaps)
