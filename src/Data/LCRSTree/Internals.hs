module Data.LCRSTree.Internals where

import Data.List (foldl')

-- | A Left-child-right-sibling tree. <https://en.wikipedia.org/wiki/Left-child_right-sibling_binary_tree>
data LCRSTree n a = Empty
    | Leaf a (LCRSTree n a)
    | Node n (LCRSTree n a) (LCRSTree n a)
  deriving (Eq, Ord, Show)

instance Functor (LCRSTree n) where
  fmap _ Empty = Empty
  fmap f (Leaf a s) = Leaf (f a) (fmap f s)
  fmap f (Node n c s) = Node n (fmap f c) (fmap f s)

-- | Map the nodes of the tree, leaving the leafs unchanged. To change
-- the leafs, use 'fmap'.
mapNodes :: (n -> c) -> LCRSTree n a -> LCRSTree c a
mapNodes _ Empty = Empty
mapNodes f (Leaf a s) = Leaf a (mapNodes f s)
mapNodes f (Node n c s) = Node (f n) (mapNodes f c) (mapNodes f s)

-- | Insert a value /a/ into the path /[n]/ into a tree.
insert :: (Eq n) => ([n], a) -> LCRSTree n a -> LCRSTree n a
insert t Empty = fromPath t
insert ([], a) t =
  case t of
    Empty  -> Leaf a Empty
    Leaf a' s -> Leaf a (Leaf a' s)
    Node n c s -> Node n c (insert ([], a) s)
insert (h:t, a) l@(Leaf _ _) = Node h (insert (t, a) Empty) l
insert (h:t, a) (Node n c s)
  | h == n = Node n (insert (t, a) c) s
  | otherwise = Node n c (insert (h:t, a) s)


-- | Given a single path and value, create a tree from it.
fromPath :: ([n], a) -> LCRSTree n a
fromPath ([], a) = Leaf a Empty
fromPath ([l], a) = Node l (Leaf a Empty) Empty
fromPath (h:t, a) = Node h (fromPath (t, a)) Empty

-- | Like 'fromPaths', but for multiple paths.
fromPaths :: Eq n => [([n], a)] -> LCRSTree n a
fromPaths = foldl' (flip insert) Empty

-- | Reverse of 'fromPaths'. Returns all paths from the root node.
-- Note that `toPaths . fromPaths` may NOT return the same tree back due to
-- some reordering of siblings.
toPaths :: LCRSTree n a -> [([n], a)]
toPaths Empty = []
toPaths (Node n c s) = trackPath [n] c ++ toPaths s
  where
    trackPath _ Empty = []
    trackPath ns (Leaf a sib) = (ns, a) : trackPath ns sib
    trackPath ns (Node n' c' s') =
      let newPath = ns ++ [n']
      in  trackPath newPath c' ++ trackPath ns s'
toPaths (Leaf _ _) = error "toPaths: Cannot build paths from a single leaf"

-- | Return the depth of the tree. This means, the depth of the longest
-- branch
lcrsDepth :: Integral i => LCRSTree n a -> i
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
pathExists :: Eq n => [n] -> LCRSTree n a -> Bool
pathExists _ Empty = False
pathExists [] _ = True
pathExists ps (Leaf _ s) = pathExists ps s
pathExists (p:ps) (Node n c s)
  | p == n = pathExists ps c
  | otherwise = pathExists (p:ps) s

-- RoseTree -------------------------------------------------------------------

-- | A RoseTree, also known as a "multi-way tree" but with leafs of a different
-- type than the nodes.
data RoseTree n a = RLeaf a | RNode n [RoseTree n a]
  deriving Show

-- | Convert a 'LCRSTree' into a 'RoseTree'
toRose :: LCRSTree n a -> RoseTree n a
toRose (Node n c Empty) = RNode n (collectS c)
  where
    collectS  Empty   = []
    collectS (Leaf a s) = RLeaf a : collectS s
    collectS (Node n' c' s') = RNode n' (collectS c') : collectS s'
toRose Empty = error "toRose: Cannot convert an empty tree to a rose tree"
toRose (Leaf _ _) = error "toRose: Cannot convert a single leaf into a rose tree"
toRose Node {} = error "toRose: Tree has no top"

-- | Convert a 'RoseTree' into a 'LCRSTree'.
fromRose :: RoseTree n a -> LCRSTree n a
fromRose (RLeaf a) = Leaf a Empty
fromRose (RNode n []) = Node n Empty Empty -- todo:pjrt this should never happen...
fromRose (RNode n (c:cs)) = Node n (mkWithS c cs) Empty
  where
    mkWithS (RLeaf a) ss =
      let siblings = case ss of
                      [] -> Empty
                      (sib:sibs) -> mkWithS sib sibs
      in Leaf a siblings
    mkWithS (RNode a children) ss =
      let childs   = case children of
                      [] -> Node a Empty
                      (ch:ct) -> Node a (mkWithS ch ct)
          siblings = case ss of
                      [] -> Empty
                      (sib:sibs) -> mkWithS sib sibs
      in childs siblings
