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
import LoadEnv (loadEnv)
import MyLib (Date)
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import TemplateHS (solutionsForYear)
import Text.Printf (printf)
import qualified Year2022
import qualified Year2023

main :: IO ()
main = do
  loadEnv
  cache_dir <- getEnv "AOC_CACHE_DIR"
  sessionKey <- getEnv "AOC_SESSION_KEY"
  (date@(day, year), part', submit, test) <- parseClap
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
  input <- getInput date cache_dir options test
  if submit == No && isNothing part'
    then do
      mapM_ (getResult date input) [Part1, Part2]
    else do
      prompt <- runAoC_ options $ AoCPrompt day
      let part = fromMaybe (last $ Map.keys prompt) part'
      submitResult submit options day part =<< getResult date input part

getInput :: Date -> String -> AoCOpts -> Bool -> IO [String]
getInput (day, year) cache _ True = lines <$> readFile (printf "%stest/%d/test%d.txt" cache year (dayInt day))
getInput (day, _) _ options False = lines . unpack <$> runAoC_ options (AoCInput day)

submitResult :: Submit -> AoCOpts -> Day -> Part -> String -> IO ()
submitResult No _ _ _ _ = do return ()
submitResult Ask o d p r = do
  answer <- askAnswer
  submitResult answer o d p r
submitResult Direct opts day part result = do
  putStrLn "Submitting...\n"
  response <- runAoC_ opts $ AoCSubmit day part result
  putStrLn $ showSubmitRes (snd response)

askAnswer :: IO Submit
askAnswer = do
  putStr "\nSubmit this solution? [y/N]: "
  hFlush stdout
  response <- map toLower <$> getLine
  case response of
    "y" -> return Direct
    _ -> return No

getResult :: Date -> [String] -> Part -> IO String
getResult (day, year) input part = do
  let solutions = fromMaybe (error "No solutions for year") $ lookup year $(solutionsForYear)
  let (p1, p2) = fromMaybe (error "Solution not defined") $ lookup (dayInt day) solutions
  let result = case part of
        Part1 -> p1 input :: String
        Part2 -> p2 input
  putStrLn $ printf "%s: %s" (show part) result
  return result