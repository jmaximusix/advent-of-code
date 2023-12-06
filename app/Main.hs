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
import Data.Char (toLower)
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
import THUtils (solutionsTemplate)
import Text.Printf (printf)

main :: IO ()
main = do
  loadEnv
  cache_dir <- getEnv "AOC_CACHE_DIR"
  sessionKey <- getEnv "AOC_SESSION_KEY"
  (day, part', year, submit, test) <- parseClap

  let options =
        AoCOpts
          { _aSessionKey = sessionKey,
            _aYear = year,
            _aCache = Just cache_dir,
            _aForce = False,
            _aThrottle = 1000000
          }

  let l = '\n' : (concat (replicate 13 "✻*") ++ "\n")
  printf "%sAdvent of Code %d Day %d%s\n" l year (dayInt day) l
  input <- getInput (day, year) cache_dir options test
  if submit == No && isNothing part'
    then do
      mapM_ (getResult day input) [Part1, Part2]
    else do
      prompt <- runAoC_ options $ AoCPrompt day
      let part = fromMaybe (last $ Map.keys prompt) part'
      submitResult submit options day part =<< getResult day input part

getInput :: (Day, Integer) -> String -> AoCOpts -> Bool -> IO [String]
getInput (day, year) cache _ True = do lines <$> readFile (printf "%stest/%d/test%d.txt" cache year (dayInt day))
getInput (day, _) _ options False = do lines . unpack <$> runAoC_ options (AoCInput day)

submitResult :: Submit -> AoCOpts -> Day -> Part -> Int -> IO ()
submitResult No _ _ _ _ = do return ()
submitResult Ask o d p r = do
  answer <- askAnswer
  submitResult answer o d p r
submitResult Direct opts day part result = do
  putStrLn "Submitting...\n"
  response <- runAoC_ opts $ AoCSubmit day part (show result)
  putStrLn $ showSubmitRes (snd response)

askAnswer :: IO Submit
askAnswer = do
  putStr "\nSubmit this solution? [y/N]: "
  hFlush stdout
  response <- toLower . head <$> getLine
  let sub' = case response of
        'y' -> Direct
        _ -> No
  return sub'

getResult :: Day -> [String] -> Part -> IO Int
getResult day input part = do
  let mbSolution = lookup (dayInt day) $(solutionsTemplate)
  let (p1, p2) = fromMaybe (error "Solution not defined") mbSolution
  result <- do
    case part of
      Part1 -> return $ p1 input
      Part2 -> return $ p2 input
  putStrLn $ printf "%s: %d" (show part) result
  return result
