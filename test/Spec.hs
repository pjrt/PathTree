module Main where

import Test.QuickCheck
import Data.List (foldl')
import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.LCRSTree
import qualified Data.LCRSTree.Internals as I

import Test.Framework
import Test.Framework.Providers.QuickCheck2


main :: IO ()
main = defaultMain runTests

runTests :: [Test]
runTests =
  [ prop_fromPath
  , prop_insert
  , prop_pathExistance
  {-, testProperty "Tree Identity" prop_identity-}
  ]

prop_fromPath :: Test
prop_fromPath =
    testGroup "fromPath"
      [ testProperty "identity" idendity_test
      , testProperty "depth" depth_test
      ]
  where
    idendity_test :: Property
    idendity_test = forAll nonEmptyPath $ \path ->
        (head . toPaths . fromPath) path === path
    depth_test :: ([AlphaChar], Int) -> Property
    depth_test p@(path, _) =
      let depth = length path
      in lcrsDepth (fromPath p) === depth

prop_insert :: Test
prop_insert =
  testGroup "Insert"
    [ testProperty "all values inserted should exist in the tree" insertExists
    ]
  where
    insertExists =
      forAll (nonEmptyPathAndTree) $ \(path, tree) ->
        let newT = insert path tree
        in  pathExistsE (fst path) newT
      where
        nonEmptyPathAndTree = liftM2 (,) nonEmptyPath arbitrary

prop_pathExistance :: Test
prop_pathExistance =
  testGroup "Path integrity"
    [ testProperty "paths should exist in a tree they make" prop_existance
    , testProperty "pathExists should consider partial paths" prop_partial
    ]

  where
    prop_partial =
      forAll (listOf1 nonEmptyPath) $ \paths ->
        let tr = fromPaths paths
            flatPaths = map (cutInHalf . fst) paths
        in conjoin $ map (`pathExistsE` tr) flatPaths
        where
          cutInHalf l = let half = length l `div` 2 in take half l


    prop_existance =
      forAll (listOf1 nonEmptyPath) $ \paths ->
        let tr = fromPaths paths
            flatPaths = map fst paths
        in conjoin $ map (`pathExistsE` tr) flatPaths


-- I would like to test this, but at the moment, I can't guarantee the
-- order in which the tree is built from the path will be the same
-- other the tree had before. Semantically speaking, however, the tree
-- doesn't change.
--
-- I could make the equality if the tree be order independent on
-- sibling nodes, but that sounds like work :\
prop_identity :: LCRSTree AlphaChar Int -> Property
prop_identity tree = (fromPaths . toPaths) tree === tree

instance (Eq n, Arbitrary n, Arbitrary a) => Arbitrary (LCRSTree n a) where
  arbitrary = do
    top <- arbitrary
    cs <- listOf1 arbitrary `suchThat` (\v -> length v > 4)
    return $ foldl' (flip insert) I.Empty $ map (first ((:) top)) cs


-- | A smaller set of characters (a-zA-Z)
newtype AlphaChar = AlphaChar Char
  deriving (Eq, Ord)

instance Show AlphaChar where
  show (AlphaChar c) = "'" ++ [c] ++ "'"


instance Arbitrary AlphaChar where
  arbitrary =
    let es = elements $ ['A'..'Z'] ++ ['a'..'z']
    in AlphaChar <$> es


pathExistsE x y =
  counterexample (show x ++ " does not exist in " ++ show y) (pathExists x y)

nonEmptyPath :: Gen ([AlphaChar], Int)
nonEmptyPath = arbitrary `suchThat` (\(p, _) -> not (null p))
