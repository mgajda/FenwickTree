{-# LANGUAGE TemplateHaskell #-}
module Main( main ) where

import Test.QuickCheck
import Test.QuickCheck.All

main = $quickCheckAll
