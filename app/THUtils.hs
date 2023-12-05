{-# LANGUAGE TemplateHaskell #-}

module THUtils where

import Language.Haskell.TH.Lib (listE, varE)
import Language.Haskell.TH.Syntax (Exp, Q, mkName)

solutions :: Q Exp
solutions = listE $ map dayParts [1 :: Int .. 25]
  where
    dayParts n =
      [|
        ( $(varE $ mkName $ "Day" ++ show n ++ "." ++ "part1"),
          $(varE $ mkName $ "Day" ++ show n ++ "." ++ "part2")
        )
        |]