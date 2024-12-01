{-# LANGUAGE TemplateHaskell #-}

module Year2024 (solutions) where

import qualified Day01
-- import qualified Day02
-- import qualified Day03
-- import qualified Day04
-- import qualified Day05
-- import qualified Day06
-- import qualified Day07
-- import qualified Day08
-- import qualified Day09
-- import qualified Day10
-- import qualified Day11
-- import qualified Day12
-- import qualified Day13
-- import qualified Day14
-- import qualified Day15
-- import qualified Day16
-- import qualified Day17
-- import qualified Day18
-- import qualified Day19
-- import qualified Day20
-- import qualified Day21
-- import qualified Day22
-- import qualified Day23
-- import qualified Day24
-- import qualified Day25

import TemplateHS (Solutions, solutionsTemplate)

solutions :: Solutions
solutions = $(solutionsTemplate)