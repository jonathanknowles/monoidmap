{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Test.Common
    ( Key
    , TestConstraints
    , property
    ) where

import Prelude

import Data.Monoid.Null
    ( MonoidNull )
import Data.Text
    ( Text )
import Data.Total.MonoidMap
    ( MonoidMap )
import Data.Typeable
    ( Typeable )
import GHC.Exts
    ( IsList (..) )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Function (..)
    , Property
    , Testable
    , checkCoverage
    , choose
    , coarbitraryIntegral
    , coarbitraryShow
    , functionIntegral
    , functionShow
    , listOf
    , scale
    , shrinkMapBy
    )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Data.Text as Text
import qualified Data.Total.MonoidMap as MonoidMap
import qualified Test.QuickCheck as QC

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance (Arbitrary k, Ord k, Arbitrary v, MonoidNull v) =>
    Arbitrary (MonoidMap k v)
  where
    arbitrary =
        fromList <$> scale (`mod` 16) (listOf ((,) <$> arbitrary <*> arbitrary))
    shrink =
        shrinkMapBy MonoidMap.fromMap MonoidMap.toMap shrink

--------------------------------------------------------------------------------
-- Test types
--------------------------------------------------------------------------------

type TestConstraints k v = (TestKey k, TestValue v)

type TestKey k =
    ( Arbitrary k
    , CoArbitrary k
    , Function k
    , Ord k
    , Show k
    , Typeable k
    )

type TestValue v =
    ( Arbitrary v
    , CoArbitrary v
    , Eq v
    , Function v
    , MonoidNull v
    , Show v
    , Typeable v
    )

newtype Key = Key Int
    deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Arbitrary Key where
    arbitrary = Key <$> choose (0, 15)
    shrink (Key k) = Key <$> shrink k

instance CoArbitrary Key where
    coarbitrary = coarbitraryIntegral

instance Function Key where
    function = functionIntegral

instance Arbitrary Text where
    arbitrary = Text.pack <$> listOf (choose ('a', 'd'))

instance CoArbitrary Text where
    coarbitrary = coarbitraryShow

instance Function Text where
    function = functionShow

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

property :: Testable t => t -> Property
property = checkCoverage . QC.property
