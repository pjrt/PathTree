{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.QuickCheck
import Data.List (foldl')
import Control.Arrow (first)
import Control.Monad (liftM2, liftM3)
import Data.LCRSTree
import Data.PathTree

import Test.Framework
import Test.Framework.Providers.QuickCheck2


main :: IO ()
main = defaultMain runTests

runTests :: [Test]
runTests =
  [ prop_fromPath
  , prop_insert
  , prop_pathExistance
  , prop_roseIdentity
  -- , testProperty "Tree Identity" $ noShrinking prop_identity
  ]

prop_fromPath :: Test
prop_fromPath =
    testGroup "fromPath"
      [ testProperty "identity" idendity_test
      , testProperty "depth" depth_test
      ]
  where
    idendity_test :: Property
    idendity_test =
      forAll pathOf2OrMore $ \path ->
        (head . toPaths . fromPath) path === path
    depth_test =
      forAll pathOf2OrMore $ \path ->
        let depth = length path - 1
        in lcrsDepth (fromPath path) === depth

    pathOf2OrMore = nonEmptyPath `suchThat` ((2 <) . length)

prop_insert :: Test
prop_insert =
  testGroup "Insert"
    [ testProperty "all values inserted should exist in the tree" insertExists
    , testProperty "inserting multiple paths with the same head should return a top node" topNodeInsert
    , testProperty "two paths inserted in any order should both exist" insertOrderExist
    , testProperty "inserting two path that diverge on a node should create a tree with one node diverging" insertDiverge
    ]
  where
    insertExists =
      forAll nonEmptyPathAndTree $ \(path, tree) ->
        let newT = insert path tree
        in  pathExistsE path newT
      where
        nonEmptyPathAndTree = liftM2 (,) nonEmptyPath arbitrary

    insertOrderExist =
      forAll twoNonEmptyPathsAndTree $ \(p1, p2, tree) ->
        let newTree1 = insert p1 $ insert p2 tree
            newTree2 = insert p2 $ insert p1 tree
        in conjoin [ pathExistsE p1 newTree1, pathExistsE p2 newTree1
                   , pathExistsE p1 newTree2, pathExistsE p2 newTree2 ]
        where
          twoNonEmptyPathsAndTree :: Gen ([AlphaChar], [AlphaChar], LCRSTree AlphaChar)
          twoNonEmptyPathsAndTree =
            liftM3 (,,) (listOf1 arbitrary) (listOf1 arbitrary) arbitrary

    topNodeInsert =
      forAll nonEmptyPathAndArb $ \(paths, top) ->
        let newPaths = map (top:) paths
            tree =  foldl' (flip insert) Empty newPaths
        in siblings tree == Empty
        where
          siblings (Node _ _ s) = s
          siblings (Leaf _ s) = s
          sibling Empty = error "No siblings for Empty"
          nonEmptyPathAndArb = liftM2 (,) (listOf1 nonEmptyPath) arbitrary

    insertDiverge =
      forAll (zipM3 nonEmptyPath nonEmptyPath nonEmptyPath) $ \(root, p1, p2) ->
        let paths = [root ++ p1, root ++ p2]
            lenOfInter = lenMin p1 p2
            tree = foldl' (flip insert) Empty paths
            actual = nodeCount tree
            expectedNumOfLeaf = 2
            expectedNumOfNode = lenOfInter - expectedNumOfLeaf + length root
        in counterexample
            (show tree ++ " contains " ++ show actual ++ " node-leaf count but expected "
                       ++ show (expectedNumOfNode, expectedNumOfLeaf))
            (actual == (expectedNumOfNode, expectedNumOfLeaf))
        where
          lenMin [l] a = 1 + length a
          lenMin a [l] = 1 + length a
          lenMin l1@(h1:t1) l2@(h2:t2)
            | h1 == h2 = 1 + lenMin t1 t2
            | otherwise = length $ l1 ++ l2
          intersectFromStart a [] = a
          intersectFromStart [] a = a
          intersectFromStart l1@(h1:t1) l2@(h2:t2)
            | h1 == h2 = h1 : intersectFromStart t1 t2
            | otherwise = l1 ++ l2


prop_pathExistance :: Test
prop_pathExistance =
  testGroup "Path integrity"
    [ testProperty "paths should exist in a tree they make" prop_existance
    , testProperty "pathExists should consider partial paths" prop_partial
    , testProperty "countPathExistances should return n for n non-uniquily inserted paths" prop_cpeNonUnique
    , testProperty "countPathExistances should return 1 for n uniquily inserted paths" prop_cpeUnique
    ]

  where
    nonZero :: Gen Int
    nonZero = arbitrary `suchThat` (>0)
    prop_partial =
      forAll (listOf1 nonEmptyPath) $ \paths ->
        let tr = fromPaths paths
            flatPaths = map cutInHalf paths
        in conjoin $ map (`pathExistsE` tr) flatPaths
        where
          cutInHalf l = let half = length l `div` 2 in take half l


    prop_existance =
      forAll (listOf1 nonEmptyPath) $ \paths ->
        let tr = fromPaths paths
        in conjoin $ map (`pathExistsE` tr) paths

    prop_cpeNonUnique =
      forAll (zipM nonEmptyPath nonZero) $ \(path, n) ->
        let tr = foldl' (flip insert) Empty $ map (const path) [1..n]
        in  countPathExistances path tr === n

    prop_cpeUnique =
      forAll (zipM nonEmptyPath nonZero) $ \(path, n) ->
        let tr = foldl' (flip insertReplace) Empty $ map (const path) [1..n]
        in  countPathExistances path tr === 1


prop_roseIdentity :: Test
prop_roseIdentity =
    testProperty "fromRoseTree . toRoseTree should be identity" roseIdent
  where
    roseIdent :: LCRSTree AlphaChar -> Property
    roseIdent tree = (fromRoseTree . toRoseTree) tree === tree


-- I would like to test this, but at the moment, I can't guarantee the
-- order in which the tree is built from the path will be the same
-- other the tree had before. Semantically speaking, however, the tree
-- doesn't change.
--
-- I could make the equality if the tree be order independent on
-- sibling nodes, but that sounds like work :\
-- We could use the path as the "identity" of a tree (a tree is indentified
-- by its paths). This makes sense, I think.
prop_identity :: LCRSTree AlphaChar -> Property
prop_identity tree = (fromPaths . toPaths) tree === tree

instance (Eq n, Arbitrary n) => Arbitrary (LCRSTree n) where
  shrink Empty = []
  shrink (Leaf a s) =
    [Empty] ++ [s] ++ [Leaf a' s' | (a', s') <- shrink (a, s)]
  shrink (Node n c s) =
    [Empty] ++ [c, s] ++ [Node n' c' s' | (n', c', s') <- shrink (n, c, s)]

  arbitrary = do
      let empty = return Empty
          leaf = do n <- arbitrary
                    s <- freq [empty, node, leaf]
                    return $ Leaf n s
          node = do n <- arbitrary
                    c <- freq [leaf, node]
                    s <- freq [empty, leaf, node]
                    return $ Node n c s
      n <- arbitrary
      c <- node
      return $ Node n c Empty
    where
      freq = frequency . freq' 60
        where
          freq' _ [] = []
          freq' n (h:t)
            | n <= 1 = (1, h) : freq' 1 t
            | otherwise = (n, h) : freq' (div n 2) t


-- | A smaller set of characters (a-zA-Z)
newtype AlphaChar = AlphaChar Char
  deriving (Eq, Ord)

instance Show AlphaChar where
  show (AlphaChar c) = "'" ++ [c] ++ "'"


instance Arbitrary AlphaChar where
  arbitrary =
    let es = elements $ ['A'..'Z'] ++ ['a'..'z']
    in AlphaChar <$> es


zipM = liftM2 (,)
zipM3 = liftM3 (,,)

pathExistsE x y =
  counterexample (show x ++ " does not exist in " ++ show y) (pathExists x y)

nonEmptyPath :: Gen [AlphaChar]
nonEmptyPath = arbitrary `suchThat` (not . null)

countPathExistances :: (Integral i, Eq n) => [n] -> PathTree n -> i
countPathExistances [] _ = 1 -- The empty path exists once, in any tree
countPathExistances _ Empty = 0
countPathExistances [h] (Leaf n s)
  | h == n = 1 + countPathExistances [h] s
  | otherwise = countPathExistances [h] s
countPathExistances (h:t) tree =
  case tree of
    Empty -> 0
    Leaf _ s -> countPathExistances (h:t) s
    Node n c s -> if n == h
                  then countPathExistances t c
                  else countPathExistances (h:t) s

nodeCount :: Integral i => PathTree n -> (i, i)
nodeCount = nodeC (0,0)
  where
    nodeC :: Integral i => (i, i) -> PathTree n -> (i, i)
    nodeC t Empty = t
    nodeC (cn, cl) (Leaf _ s) = nodeC (cn, cl + 1) s
    nodeC (cn, cl) (Node _ c s) =
      let (cnc, clc) = nodeC (cn + 1, cl) c
          (snc, slc) = nodeC (0, 0) s
      in (cnc + snc, clc + slc)
