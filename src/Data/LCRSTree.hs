-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LCRSTree
-- Copyright   :  (c) Pedro Rodriguez Tavarez <pedro@pjrt.co>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Pedro Rodriguez Tavarez <pedro@pjrt.co>
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Data.LCRSTree where

import Data.Tree (Tree)
import qualified Data.Tree as T

-- | A Left-child-right-sibling tree. <https://en.wikipedia.org/wiki/Left-child_right-sibling_binary_tree>
data LCRSTree n = Empty
    | Leaf n (LCRSTree n)
    | Node n (LCRSTree n) (LCRSTree n)
  deriving (Show, Eq)

-- | Functor instance
instance Functor LCRSTree where
  fmap _ Empty = Empty
  fmap f (Leaf a s) = Leaf (f a) (fmap f s)
  fmap f (Node n c s) = Node (f n) (fmap f c) (fmap f s)

instance Foldable LCRSTree where
  foldr _ z Empty = z
  foldr f z (Leaf n s) = foldr f (f n z) s
  foldr f z (Node n c s) =
    let v = foldr f (f n z) c
    in foldr f v s

-- | Return the depth of the tree. This means the depth of the longest
-- branch
lcrsDepth :: Integral i => LCRSTree n -> i
lcrsDepth = depth 0
  where
    depth i Empty = i
    depth i (Leaf _ s) = depth i s
    depth i (Node _ c s) =
      let lDepth = depth (i + 1) c
          rDepth = depth i s
      in max lDepth rDepth

-- | Given a path, determine if it exists. Partial paths that match
-- return "True".
pathExists :: Eq n => [n] -> LCRSTree n -> Bool
pathExists _ Empty = False
pathExists [] _ = True
pathExists (p:ps) (Leaf n s)
  | p == n = True
  | otherwise = pathExists (p:ps) s
pathExists (p:ps) (Node n c s)
  | p == n = pathExists ps c
  | otherwise = pathExists (p:ps) s

-- | Convert a 'Tree' into a 'LCRSTree'
fromRoseTree :: Tree n -> LCRSTree n
fromRoseTree t = mkWithS t []
  where
    mkWithS (T.Node n []) ss = Leaf n $ siblings ss
    mkWithS (T.Node n ch) ss =
      let mkN = case ch of
                 [] -> Leaf n
                 (c:cs) -> Node n (mkWithS c cs)
      in  mkN $ siblings ss

    siblings [] = Empty
    siblings (c:cs) = mkWithS c cs

-- | Convert a 'LCRSTree' into a 'Tree'
--
-- This function fails if a non-top 'Node' is passed. A non-top node is a node
-- @Node n c s@ where @s /= Empty@.
toRoseTree :: LCRSTree n -> Tree n
toRoseTree (Node topN topC Empty) = T.Node topN (collectS topC)
  where
    collectS  Empty   = []
    collectS (Leaf a s) = T.Node a [] : collectS s
    collectS (Node n c s) = T.Node n (collectS c) : collectS s
toRoseTree _ = error "fromLCRSTree: non-top node passed"
