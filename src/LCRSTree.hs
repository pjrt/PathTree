module LCRSTree where

import Data.List (foldl')

-- Left-child-right-sibling tree
data LCRSTree n a = Empty
    | Leaf a (LCRSTree n a)
    | Node n (LCRSTree n a) (LCRSTree n a)
  deriving (Eq, Ord, Show)

instance Functor (LCRSTree n) where
  fmap _ Empty = Empty
  fmap f (Leaf a s) = Leaf (f a) (fmap f s)
  fmap f (Node n c s) = Node n (fmap f c) (fmap f s)

mapNodes :: (n -> c) -> LCRSTree n a -> LCRSTree c a
mapNodes _ Empty = Empty
mapNodes f (Leaf a s) = Leaf a (mapNodes f s)
mapNodes f (Node n c s) = Node (f n) (mapNodes f c) (mapNodes f s)

fromPath :: ([n], a) -> LCRSTree n a
fromPath ([], _) = Empty
fromPath ([l], a) = Node l (Leaf a Empty) Empty
fromPath (h:t, a) = Node h (fromPath (t, a)) Empty

insert :: (Eq n) => ([n], a) -> LCRSTree n a -> LCRSTree n a
insert t Empty = fromPath t

insert ([], a) t =
  case t of
    Empty  -> Leaf a Empty -- insert
    Leaf a' s -> Leaf a (Leaf a' s)
    Node n c s -> Node n c (insert ([], a) s)
insert (h:t, a) l@(Leaf _ _) = Node h (insert (t, a) Empty) l
insert (h:t, a) (Node n c s)
  | h == n = Node n (insert (t, a) c) s
  | otherwise = Node n c (insert (h:t, a) s)

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

fromPaths :: Eq n => [([n], a)] -> LCRSTree n a
fromPaths = foldl' (flip insert) Empty

lcrsDepth :: Integral i => LCRSTree n a -> i
lcrsDepth = depth 0
  where
    depth i Empty = i
    depth i (Leaf _ s) = depth i s
    depth i (Node _ c s) =
      let lDepth = depth (i + 1) c
          rDepth = depth i s
      in max lDepth rDepth

pathExists :: Eq n => [n] -> LCRSTree n a -> Bool
pathExists _ Empty = False
pathExists [] _ = True
pathExists ps (Leaf _ s) = pathExists ps s
pathExists (p:ps) (Node n c s)
  | p == n = pathExists ps c
  | otherwise = pathExists (p:ps) s

-- RoseTree -------------------------------------------------------------------
data RoseTree n a = RLeaf a | RNode n [RoseTree n a]
  deriving Show

toRose :: LCRSTree n a -> RoseTree n a
toRose (Node n c Empty) = RNode n (collectS c)
  where
    collectS  Empty   = []
    collectS (Leaf a s) = RLeaf a : collectS s
    collectS (Node n' c' s') = RNode n' (collectS c') : collectS s'
toRose Empty = error "toRose: Cannot convert an empty tree to a rose tree"
toRose (Leaf _ _) = error "toRose: Cannot convert a single leaf into a rose tree"
toRose Node {} = error "toRose: Tree has no top"

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
