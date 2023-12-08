{-# LANGUAGE TupleSections #-}

module TemplateHS (solutionsTemplate, solutionsForYear) where

import Control.Monad (liftM2)
import Control.Monad.Extra (mapMaybeM)
import Data.Bifunctor (bimap)
import Data.Tuple.Extra (both)
import Debug.Trace (traceShowId)
import Language.Haskell.TH.Lib (integerL, listE, litE, tupE, varE)
import Language.Haskell.TH.Syntax (Exp, Q, lookupValueName)
import Text.Printf (printf)

solutionsTemplate :: Q Exp
solutionsTemplate = do
  let name = printf "Day%02d.part%d"
  let names day = both (lookupValueName . name day) (1 :: Integer, 2)
  let lookupDay d = uncurry (liftM2 (liftM2 (curry (d,)))) $ names d
  let tuptup (a, b) = tupE [a, b]
  exist <- mapMaybeM lookupDay [1 .. 25]
  listE $ map (tuptup . bimap (litE . integerL) (tuptup . both varE)) exist

solutionsForYear :: Q Exp
solutionsForYear = do
  let name = printf "Year%d.solutions"
  let lookupYear y = fmap (y,) <$> lookupValueName (name y)
  exist <- mapMaybeM lookupYear [2015 .. 2023]
  listE $ map (\(a, b) -> tupE [litE $ integerL a, varE b]) exist