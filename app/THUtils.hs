{-# LANGUAGE TupleSections #-}

module THUtils (solutionsTemplate) where

import Control.Monad (liftM2)
import Control.Monad.Extra (mapMaybeM)
import Data.Bifunctor (bimap)
import Data.Tuple.Extra (both)
import Language.Haskell.TH.Lib (integerL, listE, litE, tupE, varE)
import Language.Haskell.TH.Syntax (Exp, Q, lookupValueName)
import Text.Printf (printf)

solutionsTemplate :: Q Exp
solutionsTemplate = do
  let name = printf "Year2023.Day%d.part%d"
  let names day = both (lookupValueName . name day) (1 :: Integer, 2)
  let lookupDay d = uncurry (liftM2 (liftM2 (curry (d,)))) $ names d
  let tuptup (a, b) = tupE [a, b]
  exist <- mapMaybeM lookupDay [1 .. 25]
  listE $ map (tuptup . bimap (litE . integerL) (tuptup . both varE)) exist