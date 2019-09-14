module MusicaTesting where

import Harmoniask as HSK
import Test.QuickCheck as QCK
import Control.Monad

instance Arbitrary HSK.Distance where
  arbitrary = do
    x <- choose (0, 15)
    return $ (toEnum x)

instance Arbitrary HSK.Interval where
  arbitrary = fmap Interval arbitrary

instance Arbitrary HSK.Chord where
  arbitrary = fmap Chord arbitrary
