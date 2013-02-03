{-# LANGUAGE TemplateHaskell #-}
module Main( main ) where

import Data.Tree.FenwickTree

import Test.QuickCheck
import Test.QuickCheck.All

emptyFT :: FTree (Double, Double)
emptyFT = empty (\(pos, freq) -> freq) (\(pos1, _) (pos2, _) -> pos1 `compare` pos2)

prop_insert_toList ls = toList (foldr insert emptyFT ls) == ls

main = $quickCheckAll
