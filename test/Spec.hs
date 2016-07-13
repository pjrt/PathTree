module Spec where

import Test.QuickCheck
import LCRSTree


main :: IO ()
main = runTests

runTests = quickCheck prop_identity

prop_identity :: LCRSTree AlphaChar Int -> Property
prop_identity tree = (fromPaths . toPaths) tree === tree

instance (Eq n, Arbitrary n, Arbitrary a) => Arbitrary (LCRSTree n a) where
  arbitrary = do
    let leaf = Leaf <$> arbitrary
        empty = return Empty
        cNode = frequency [(5, node), (5, leaf)]
        node = do
          n <- arbitrary
          c <- cNode
          s <- frequency [(1, empty), (15, leaf), (5, node `suchThat` notSameN n)]
          return (Node n c s)
    n <- arbitrary
    c <- cNode
    return $ Node n c Empty
    where
      notSameN n (Node n2 _ _) = n /= n2
      notSameN _ _ = True


newtype AlphaChar = AlphaChar Char
  deriving (Eq, Ord)

instance Show AlphaChar where
  show (AlphaChar c) = [c]


instance Arbitrary AlphaChar where
  arbitrary =
    AlphaChar <$> choose ('A', 'z')
