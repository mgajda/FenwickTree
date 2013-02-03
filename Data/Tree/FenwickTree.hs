{-# LANGUAGE BangPatterns #-}
module Data.Tree.FenwickTree(FTree,
                             empty, insert,
                             query, invQuery,
                             toList, toFreqList,
                             fromList) where

type Val = Double

data FTree a = FTree { root :: FNode a
                     , val  :: a -> Val
                     , cmp  :: a -> a -> Ordering
                     }

instance (Show a) => Show (FTree a) where
  showsPrec _ ft = ("FTree " ++) . shows (root ft)

data FNode a = Node { psum        :: Val,
                      split       :: a,
                      left, right :: FNode a
                    }
             | Leaf
  deriving (Show)

empty :: (a -> Double) -> (a -> a -> Ordering) -> FTree a
empty v c = FTree { root   = Leaf
                  , val    = v
                  , cmp    = c
                  }

insert :: a -> FTree a -> FTree a
insert a ft = ft { root = insert' a (val ft) (cmp ft) (root ft) }

insert' a val cmp Leaf = Node { psum  = val a
                              , split = a
                              , left  = Leaf
                              , right = Leaf
                              }
insert' a val cmp n@(Node { psum  = p
                          , split = s
                          , left  = l
                          , right = r
                          }) = case a `cmp` s of
                                 GT -> n { right = insert' a val cmp r }
                                 LT -> n { psum = p + val a
                                         , left = insert' a val cmp l }
                                 EQ -> n { psum = p + val a } -- just adjust frequency

query :: a -> FTree a -> Val
query a ft = query' (cmp ft) a (root ft)

query' cmp a Leaf                 = 0.0
query' cmp a (Node { psum  = p
                   , split = s
                   , left  = l
                   , right = r }) = case a `cmp` s of
                                      GT -> p + query' cmp a r
                                      LT ->     query' cmp a l
                                      EQ -> p

invQuery :: Val -> FTree a -> a
invQuery = undefined

toList :: FTree a -> [a]
toList ft = toList' (root ft) []

toList'  Leaf                  cont = cont
toList' (Node { split = s
              , left  = l
              , right = r })   cont = toList' l $ s:toList' r cont

toFreqList :: FTree a -> [(Double, a)]
toFreqList ft = toFreqList' 0.0 (root ft) []

toFreqList' cSum Leaf cont = cont
toFreqList' cSum (Node { split = s
                       , psum  = p
                       , left  = l
                       , right = r }) cont = toFreqList' cSum l $
                                             (nSum, s):toFreqList' nSum r []
  where
    nSum = p+cSum

fromList = undefined

