{-# LANGUAGE TemplateHaskell #-}

module Main where

import Advent
  ( AoC (AoCInput, AoCPrompt, AoCSubmit),
    AoCOpts
      ( AoCOpts,
        _aCache,
        _aForce,
        _aSessionKey,
        _aThrottle,
        _aYear
      ),
    Day,
    Part (Part1, Part2),
    dayInt,
    mkDay_,
    runAoC_,
    showSubmitRes,
  )
import Data.Text (unpack)
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day2
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import LoadEnv (loadEnv)
import System.Environment (getEnv)
import THUtils (solutions)

getSolution :: Day -> Part -> String -> Int
getSolution day part = case part of
  Part1 -> fst sol
  Part2 -> snd sol
  where
    sol = $(solutions) !! (fromIntegral (dayInt day) - 1)

main :: IO ()
main = do
  putStrLn "--- Advent of Code ---"
  loadEnv
  session_key <- getEnv "AOC_SESSION_KEY"
  let options =
        AoCOpts
          { _aSessionKey = session_key,
            _aYear = 2023,
            _aCache = Just "./aoc_cache",
            _aForce = False,
            _aThrottle = 3000000
          }
  let day = mkDay_ 1
  let part = Part1

  -- prompt <- runAoC_ options $ AoCPrompt day
  input <- runAoC_ options $ AoCInput day
  let result = getSolution day part (unpack input)
  print result
  response <- runAoC_ options $ AoCSubmit day part (show result)
  print $ showSubmitRes (snd response)