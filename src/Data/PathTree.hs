-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PathTree
-- Copyright   :  (c) Pedro Rodriguez Tavarez <pedro@pjrt.co>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Pedro Rodriguez Tavarez <pedro@pjrt.co>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module implements multiple functions using a 'LCRSTree' to create a
-- tree where the mode of insertion are paths.
-------------------------------------------------------------------------------
module Data.PathTree
( PathTree
, LCRSTree(..)
, insert
, insertWith
, insertReplace
, fromPath
, fromPaths
, fromPathsWith
, fromPathsReplace
, toPaths
, pathExists
) where

import Data.List (foldl')
import Data.LCRSTree

-- | A path tree is simply a 'LCRSTree'
type PathTree n = LCRSTree n

-- | Insert a value /a/ into the path /[n]/ into a tree.
insert :: (Eq n) => [n] -> PathTree n -> PathTree n
insert t Empty = fromPath t
insert [] t  = t
insert [a] t =
  case t of
    Empty -> Leaf a Empty
    Leaf a' s -> Leaf a (Leaf a' s)
    Node n c s -> Node n c (insert [a] s)
insert (h:t) l@(Leaf _ _) = Node h (insert t Empty) l
insert (h:t) (Node n c s)
  | h == n = Node n (insert t c) s
  | otherwise = Node n c (insert (h:t) s)

-- | Like 'insert', but will use /f/ to decide what to do if an existing
-- value already exists at the path.
insertWith :: (Eq n) => (n -> n -> n) -> [n] -> PathTree n -> PathTree n
insertWith _ t Empty = fromPath t
insertWith _ [] t = t
insertWith f [a] t =
  case t of
    Empty -> Leaf a Empty
    Leaf a' s -> if a == a'
                 then Leaf (f a' a) s
                 else Leaf a (insertWith f [a] s)
    Node n c s -> if n == a
                  then Node (f n a) c s
                  else Node n c (insertWith f [a] s)
insertWith f (h:t) l@(Leaf _ _) = Node h (insertWith f t Empty) l
insertWith f (h:t) (Node n c s)
  | h == n = Node n (insertWith f t c) s
  | otherwise = Node n c (insertWith f (h:t) s)

-- | Like 'insert', but replaces the value at the path. May seem odd to
-- replace a value that is equal to itself, but this can be used with
-- partially-equal types for some flexibility.
insertReplace :: (Eq n) => [n] -> PathTree n -> PathTree n
insertReplace = insertWith const


-- | Given a single path, create a tree from it.
fromPath :: [n] -> PathTree n
fromPath [] = Empty
fromPath [a] = Leaf a Empty
fromPath (h:t) = Node h (fromPath t) Empty

-- | Like 'fromPath', but for multiple paths.
fromPaths :: Eq n => [[n]] -> PathTree n
fromPaths [] = Empty
fromPaths (h:t) = foldl' (flip insert) (fromPath h) t

-- | Like 'fromPaths' but applies /f/ if a give path already exists.
fromPathsWith :: Eq n => (n -> n -> n) -> [[n]] -> PathTree n
fromPathsWith _ [] = Empty
fromPathsWith f (h:t) = foldl' (flip (insertWith f)) (fromPath h) t

-- | Like 'fromPaths' but if two equal paths are passed, the former one
-- will be replaced.
fromPathsReplace :: Eq n => [[n]] -> PathTree n
fromPathsReplace = fromPathsWith const

-- | Returns all paths from the root node(s).
-- Note that @toPaths . fromPaths@ may NOT return the same tree back due to
-- some reordering of siblings.
toPaths :: PathTree n -> [[n]]
toPaths = trackPath []
  where
    trackPath _ Empty = []
    trackPath ns (Leaf a sib) = (ns ++ [a]) : trackPath ns sib
    trackPath ns (Node n' c' s') =
      let newPath = ns ++ [n']
      in  trackPath newPath c' ++ trackPath ns s'

-- | Given a path, determine if it exists fully. For a path to "exists fully"
-- means that it ends on a level that contains a leaf.
pathExists :: Eq n => [n] -> LCRSTree n -> Bool
pathExists _ Empty = False
pathExists paths (Leaf n s) =
  case paths of
    [] -> False
    [p] -> n == p || pathExists [p] s
    (p:ps) -> if p == n then pathExists ps s
                        else pathExists (p:ps) s
pathExists paths (Node n c s) =
  case paths of
    [] -> False
    [p] -> pathExists [p] s
    (p:ps) -> if p == n then pathExists ps c
                        else pathExists (p:ps) s
