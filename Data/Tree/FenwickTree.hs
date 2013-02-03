{-# LANGUAGE BangPatterns #-}
module Data.Tree.FenwickTree(FTree,
                             empty, insert,
                             query, invQuery,
                             toList, toFreqList,
                             fromList) where

import Control.Exception(assert) -- DEBUG
import Data.List(sortBy)

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

invQuery :: Val -> FTree a -> Maybe a
invQuery v ft = invQuery' v (root ft)

invQuery' :: Val -> FNode a -> Maybe a 
invQuery' v Leaf = Nothing
invQuery' v (Node { psum  = p
                  , split = s
                  , left  = l
                  , right = r }) = case v `compare` p of
                                     EQ -> Just s
                                     GT -> invQuery' v r
                                     LT -> case invQuery' v l of
                                             Just r  -> Just r
                                             Nothing -> Just s

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
                       , right = r
                       }) cont = toFreqList' cSum l $
                                   (nSum, s):toFreqList' nSum r cont
  where
    nSum = p+cSum

fromList cmp val ls = FTree { cmp  = cmp
                            , val  = val
                            , root = fromList' cmp val l $ sortBy cmp ls
                            }
  where
    l = length ls

fromList' cmp val 0 [ ] = Leaf
fromList' cmp val 1 [a] = Node { split = a
                               , psum  = val a
                               , left  = Leaf
                               , right = Leaf
                               }
fromList' cmp val n ls = assertions $
                           Node { split = a
                                , psum  = val a
                                , left  = fromList' cmp val n'  lsLeft
                                , right = fromList' cmp val n'' lsRight
                                }
  where
    a       = head rest
    lsRight = tail rest
    (lsLeft, rest) = splitAt n' ls
    n'  = n `div` 2
    n'' = n - n' - 1
    assertions r = assert (n' + n'' + 1 == n) $
                   assert (length lsRight == n'') $
                   assert (length lsLeft  == n' ) $
                   r

