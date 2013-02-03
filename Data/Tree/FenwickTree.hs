module Data.Tree.FenwickTree(FTree,
                             empty, insert,
                             query, invQuery,
                             toList, toFreqList, fromList) where

type Val = Double

data FTree a = FTree { root :: Maybe (FNode a)
                     , val  :: a -> Val
                     , cmp  :: a -> a -> Ordering
                     }

data FNode a = Node { psum        :: Val,
                      left, right :: FNode a
                    }
             | Leaf { v, psum :: Val,
                      content :: a
                    }

empty :: (a -> Double) -> (a -> a -> Ordering) -> FTree a
empty v c = FTree { root   = Nothing
                  , val    = v
                  , cmp    = c
                  }

insert :: a -> FTree a -> FTree a
insert a ft@(FTree { root = Nothing }) = ft { root = Just $ Leaf { v       = v
                                                                 , psum    = v
                                                                 , content = a
                                                                 }
                                            }
  where
    v = val ft $ a
insert a ft                            = ft { root = insert' a (val ft) (cmp ft) (root ft) }

insert' a ass cmp = undefined

query :: a -> FTree a -> Double
query = undefined

invQuery :: Val -> FTree a -> a
invQuery = undefined

toList :: FTree a -> [a]
toList (FTree { root=Nothing   }) = []
toList (FTree { root=Just node }) = toList' node []

toList' (Leaf { content = c }) cont = c:cont
toList' (Node { left  = l
              , right = r })   cont = toList' l $ toList' r cont

toFreqList :: FTree a -> [(Double, a)]
toFreqList = undefined

fromList = undefined
