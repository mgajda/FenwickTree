{-# LANGUAGE TemplateHaskell #-}
module Main( main ) where

import Data.Tree.FenwickTree

import Test.QuickCheck
import Test.QuickCheck.All

emptyFT :: FTree (Double, Double)
emptyFT = empty (\(pos, freq) -> freq)

prop_insert_toList ls = toList (foldr insert emptyFT ls) == ls

main = $quickCheckAll
