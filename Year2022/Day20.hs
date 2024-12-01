module Day20 (solution20) where

import Data.List.Index(deleteAt, insertAt, indexed)
import Data.Maybe(fromJust)
import Data.List(elemIndex)

solution20 :: IO ()
solution20 = do
    mylines <- lines <$> readFile "input20"
    let input = map (\x -> read x ::Int) mylines
    let decryptionKey = 811589153
    let mixed n xs  = map snd $ mixN n xs (indexed xs)
    let g xs x = xs !! ((x + fromJust (elemIndex 0 xs)) `mod` length xs)
    let f xs n = sum $ map (g (mixed n xs)) [1000,2000,3000]
    print $ f input 1
    print $ f (map (*decryptionKey) input) 10

mixN:: Int -> [Int] -> [(Int, Int)] -> [(Int, Int)]
mixN 0 _ input = input
mixN n original input = mixN (n-1) original $ foldl mix input (indexed original)

mix :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
mix input (_, 0) = input
mix input next = insertAt i' next input'
    where   i = fromJust $ elemIndex next input
            input' = deleteAt i input
            i' = (i + snd next) `mod` length input'
