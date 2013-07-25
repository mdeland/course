module Structure.List.Arbitrary where

import Structure.List
import Test.QuickCheck

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    fmap (foldr (:.) Nil) arbitrary