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
    runAoC_,
    showSubmitRes,
  )
import Clap (Submit (Ask, Direct, No), parseClap)
import qualified Data.Map as Map (keys)
import Data.Maybe (fromMaybe, isNothing)
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
import System.IO (hFlush, stdout)
import THUtils (solutions)
import Text.Printf (printf)

main :: IO ()
main = do
  loadEnv
  session_key <- getEnv "AOC_SESSION_KEY"
  cache_dir <- getEnv "AOC_CACHE_DIR"

  (day, part', year, submit, test) <- parseClap

  let options =
        AoCOpts
          { _aSessionKey = session_key,
            _aYear = year,
            _aCache = Just cache_dir,
            _aForce = False,
            _aThrottle = 1000000
          }

  prompt <- runAoC_ options $ AoCPrompt day
  let part = fromMaybe (last $ Map.keys prompt) part'
  let l = '\n' : (concat (replicate 13 "*✻") ++ "\n")
  printf "%sAdvent of Code %d Day %d%s\n" l year (dayInt day) l
  input <-
    if test
      then do lines <$> readFile (printf "%stest/%d/test%d.txt" cache_dir year (dayInt day))
      else do lines . unpack <$> runAoC_ options (AoCInput day)
  result <-
    if submit == No && isNothing part'
      then do return $ unlines $ map (\x -> printf "%s:\t%d" (show x) (getSolution day input x)) [Part1, Part2]
      else do (return . show . getSolution day input) part
  putStrLn result
  submit' <-
    if submit == Ask
      then do askAnswer
      else do return submit
  case submit' of
    Direct -> do
      response <- runAoC_ options $ AoCSubmit day part result
      print $ showSubmitRes (snd response)
    _ -> do return ()

askAnswer :: IO Submit
askAnswer = do
  putStr "\nSubmit this solution? [y/N]: "
  hFlush stdout
  response <- getLine
  let sub' = case response of
        "y" -> Direct
        "Y" -> Direct
        _ -> No
  return sub'

getSolution :: Day -> [String] -> Part -> Int
getSolution day input part = case part of
  Part1 -> fst sol input
  Part2 -> snd sol input
  where
    sol = $(solutions) !! (fromIntegral (dayInt day) - 1)