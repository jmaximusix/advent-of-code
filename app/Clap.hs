module Clap (Submit (No, Ask, Direct), parseClap) where

import Advent
import qualified Control.Monad (when)
import Data.Char (toLower)
import MyLib (Date, capitalise, mostRecentChallenge, readPart)
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    optional,
    progDesc,
    short,
    strOption,
    switch,
    value,
    (<**>),
  )
import System.Exit (exitFailure)

data Submit = Direct | Ask | No deriving (Show, Read, Eq)

data Clap = Clap Day (Maybe Part) Integer Submit Bool

parseClap :: IO (Date, Maybe Part, Submit, Bool)
parseClap = do
  recent <- mostRecentChallenge
  Clap day part year submit test <-
    execParser $
      info
        (clapOptions recent <**> helper)
        ( fullDesc <> progDesc "Advent of Code" <> header "Advent of Code solutions in Haskell"
        )
  Control.Monad.when (submit /= No && test) $ do
    putStrLn "Cannot submit a solution using a test input.\nPlease adjust command line options."
    exitFailure
  return ((day, year), part, submit, test)

clapOptions :: Date -> Parser Clap
clapOptions (recentDay, recentYear) =
  Clap
    <$> ( mkDay_
            <$> option
              auto
              ( long "day"
                  <> short 'd'
                  <> value (dayInt recentDay)
                  <> help "Day of the month"
              )
        )
    <*> optional
      ( readPart
          <$> strOption
            ( long "part"
                <> short 'p'
                <> help "Part of the problem"
            )
      )
    <*> option
      auto
      ( long "year"
          <> short 'y'
          <> value recentYear
          <> help "Year"
      )
    <*> ( read . capitalise . map toLower
            <$> strOption
              ( long "submit"
                  <> short 's'
                  <> value "no"
                  <> metavar "[direct|ask|no]"
                  <> help "Submission method"
              )
        )
    <*> switch
      ( long "test"
          <> short 't'
          <> help "Test mode"
      )
