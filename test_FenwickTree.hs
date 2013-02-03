{-# LANGUAGE TemplateHaskell #-}
module Main( main ) where

import Data.Tree.FenwickTree
import Data.List(sort)

import Test.QuickCheck
import Test.QuickCheck.All

emptyFT :: FTree (Double, Double)
emptyFT = empty (\(pos, freq) -> freq) (\(pos1, _) (pos2, _) -> pos1 `compare` pos2)

uniq (a:b:cs) | a == b = uniq $ b:cs
uniq (a:cs)            = a:uniq cs
uniq []                = []

prop_insert_toList ls = toList (foldr insert emptyFT sls) == sls
  where
    sls = uniq $ sort ls 

main = $quickCheckAll
