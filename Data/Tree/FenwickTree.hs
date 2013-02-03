module Data.Tree.FenwickTree(FTree, empty, insert, query, invQuery, toList, toFreqList) where

type Val = Double

data FTree a = FTree { root   :: FNode a
                     , assess :: a -> Val
                     }

data FNode a = Node { psum        :: Val,
                      left, right :: FTree a
                    }
             | Leaf { v, psum :: Val,
                      content :: a
                    }

empty :: (a -> Double) -> FTree a
empty asfun = FTree { root   = undefined
                    , assess = asfun
                    }

insert :: a -> FTree a -> FTree a
insert = undefined

query :: a -> FTree a -> Double
query = undefined

invQuery :: Val -> FTree a -> a
invQuery = undefined

toList :: FTree a -> [a]
toList = undefined

toFreqList :: FTree a -> [(Double, a)]
toFreqList = undefined
