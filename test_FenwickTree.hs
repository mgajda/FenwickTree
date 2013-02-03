{-# LANGUAGE TemplateHaskell #-}
module Main( main ) where

import Data.Tree.FenwickTree
import Data.List(sort)

import Test.QuickCheck
import Test.QuickCheck.All

emptyFT :: FTree (Double, Double)
emptyFT = empty (\(pos, freq) -> freq) (\(pos1, _) (pos2, _) -> pos1 `compare` pos2)


-- Prepare a list of unique values

uniq []     = []
uniq (e:es) = (e:) . uniq . filter (/= e) $ es

mkTree = foldr insert emptyFT

prop_insert_toList ls = toList (mkTree uls) == sort uls
  where
    uls = uniq ls

prop_insert_query_non_zero l ls = query l (insert l ft) == snd l + query l ft
  where
    ft = mkTree $ filter (/=l) ls

prop_freqList ls = toFreqList (mkTree uls) == zip (scanr ((+) . snd) 0.0 uls) uls
  where
    uls = uniq ls

prop_freqList_query l ls = query l ft == lookupFL l (toFreqList ft)
  where
    uls = uniq ls
    ft  = insert l (mkTree uls)

lookupFL a ((f, b):_ ) | a == b = f
lookupFL a ((f, b):cs)          = lookupFL a cs
lookupFL a []                   = 0.0
-- prop_insert_freqList

main = $quickCheckAll

