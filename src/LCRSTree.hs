module LCRSTree where

import Data.List (foldl')
{-import qualified Data.Map as M-}

data LCRSTree n a = Empty
    | Leaf a
    | Node n (LCRSTree n a) (LCRSTree n a)
  deriving (Eq, Ord, Show)

instance Functor (LCRSTree n) where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node n c s) = Node n (fmap f c) (fmap f s)

mapNodes :: (n -> c) -> LCRSTree n a -> LCRSTree c a
mapNodes _ Empty = Empty
mapNodes _ (Leaf a) = Leaf a
mapNodes f (Node n c s) = Node (f n) (mapNodes f c) (mapNodes f s)

fromEmpty :: ([n], a) -> LCRSTree n a
fromEmpty ([], _) = Empty
fromEmpty ([l], a) = Node l (Leaf a) Empty
fromEmpty (h:t, a) = Node h (fromEmpty (t, a)) Empty

insert :: (Eq n) => ([n], a) -> LCRSTree n a -> LCRSTree n a
insert ([], a) t =
  case t of
    Empty  -> Leaf a
    Leaf _ -> Leaf a
    Node n c s -> Node n c (insert ([], a) s)
insert t Empty = fromEmpty t
insert (h:t, a) l@(Leaf _) = Node h (insert (t, a) Empty) l
insert (h:t, a) (Node n c s)
  | h == n = Node n (insert (t, a) c) s
  | otherwise = Node n c (insert (h:t, a) s)

toPaths :: LCRSTree n a -> [([n], a)]
toPaths (Node n c Empty) = trackPath [n] c
  where
    trackPath _ Empty = []
    trackPath ns (Leaf a) = [(ns, a)]
    trackPath ns (Node n' c' s) =
      let newPath = ns ++ [n']
      in  trackPath newPath c' ++ trackPath ns s
toPaths _ = error "toPaths: Top was not passed"

fromPaths :: (Eq n) => [([n], a)] -> LCRSTree n a
fromPaths = foldl' (flip insert) Empty


toRose :: LCRSTree n a -> RoseTree n a
toRose Empty = error "Empty tree"
toRose (Leaf _) = error "Single leaf"
toRose (Node n c Empty) = RNode n (collectS c)
  where
    collectS Empty = []
    collectS (Leaf a) = [RLeaf a]
    collectS (Node n' c' s) = RNode n' (collectS c') : collectS s
toRose (Node _ _ _) = error "toRose: Top node has siblings!"

{-fromRose :: RoseTree n a -> LCRSTree n a-}
{-fromRose (RLeaf a) = Leaf a-}
{-fromRose (RNode n (c:s)) = Node n (fromParts c s) Empty-}
  {-where-}
    {-fromParts (RNode n' (tc:ts)) (sb:sbs) =-}
      {-Node n' (fromParts tc ts) (fromParts sb sbs)-}
    {-fromParts (RLeaf a) (sb:sbs) = Node        fromParts sb sbs-}

data RoseTree n a = RLeaf a | RNode n [RoseTree n a]
  deriving Show
