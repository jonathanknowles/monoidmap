-- |
-- Copyright: © 2025 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.QuickCheck.Instances.Arbitrary ()
where

import Data.Functor
    ( (<$>)
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.Ord
    ( Ord
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary, shrink)
    , shrinkMap
    )

import qualified Data.MonoidMap as MonoidMap

instance
    ( Arbitrary k
    , Arbitrary v
    , Ord k
    , MonoidNull v
    )
    => (Arbitrary (MonoidMap k v))
  where
    arbitrary = MonoidMap.fromMap <$> arbitrary
    shrink = shrinkMap MonoidMap.fromMap MonoidMap.toMap
