{-# LANGUAGE ExistentialQuantification #-}
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
    , Test
    , TestType (..)
    , testTypesMonoidNull
    , testTypesGroup
    , testTypesMonus
    , testTypesLeftReductive
    , testTypesRightReductive
    , testTypesReductive
    , testTypesLeftGCDMonoid
    , testTypesRightGCDMonoid
    , testTypesOverlappingGCDMonoid
    , testTypesGCDMonoid
    , testTypesLCMMonoid
    , TestValue
    , makeSpec
    , property
    ) where

import Prelude

import Data.Group
    ( Group )
import Data.Kind
    ( Constraint, Type )
import Data.Monoid
    ( Dual, Sum (..) )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, OverlappingGCDMonoid, RightGCDMonoid )
import Data.Monoid.LCM
    ( LCMMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Proxy
    ( Proxy (Proxy) )
import Data.Semigroup.Cancellative
    ( LeftReductive, Reductive, RightReductive )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Total.MonoidMap
    ( MonoidMap )
import Data.Typeable
    ( Typeable, typeRep )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe )
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
    , frequency
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

instance Arbitrary Text where
    arbitrary = Text.pack <$> listOf genChar
      where
        genChar = frequency
            [ (64, pure 'a')
            , (16, pure 'b')
            , ( 4, pure 'c')
            , ( 1, pure 'd')
            ]

instance CoArbitrary Text where
    coarbitrary = coarbitraryShow

instance Function Text where
    function = functionShow

--------------------------------------------------------------------------------
-- Test keys
--------------------------------------------------------------------------------

newtype Key = Key Int
    deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Arbitrary Key where
    arbitrary = Key <$> choose (0, 15)
    shrink (Key k) = Key <$> shrink k

instance CoArbitrary Key where
    coarbitrary = coarbitraryIntegral

instance Function Key where
    function = functionIntegral

--------------------------------------------------------------------------------
-- Test constraints
--------------------------------------------------------------------------------

type Test k v = (TestKey k, TestValue v)

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

--------------------------------------------------------------------------------
-- Test instances
--------------------------------------------------------------------------------

data TestType (c :: Type -> Constraint) =
    forall v. (TestValue v, c v) => TestType (Proxy v)

testTypesMonoidNull :: [TestType MonoidNull]
testTypesMonoidNull =
    [ TestType (Proxy @(Dual Text))
    , TestType (Proxy @(Dual [Int]))
    , TestType (Proxy @(Dual [Natural]))
    , TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Int))
    , TestType (Proxy @(Sum Natural))
    , TestType (Proxy @(Text))
    , TestType (Proxy @[Int])
    , TestType (Proxy @[Natural])
    ]

testTypesGroup :: [TestType Group]
testTypesGroup =
    [ TestType (Proxy @(Sum Int))
    ]

testTypesMonus :: [TestType Monus]
testTypesMonus =
    [ TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Natural))
    ]

testTypesLeftReductive :: [TestType LeftReductive]
testTypesLeftReductive =
    [ TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Int))
    , TestType (Proxy @(Sum Natural))
    , TestType (Proxy @[Int])
    , TestType (Proxy @[Natural])
    , TestType (Proxy @(Text))
    , TestType (Proxy @(Dual [Int]))
    , TestType (Proxy @(Dual [Natural]))
    , TestType (Proxy @(Dual Text))
    ]

testTypesRightReductive :: [TestType RightReductive]
testTypesRightReductive =
    [ TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Int))
    , TestType (Proxy @(Sum Natural))
    , TestType (Proxy @[Int])
    , TestType (Proxy @[Natural])
    , TestType (Proxy @(Text))
    , TestType (Proxy @(Dual [Int]))
    , TestType (Proxy @(Dual [Natural]))
    , TestType (Proxy @(Dual Text))
    ]

testTypesReductive :: [TestType Reductive]
testTypesReductive =
    [ TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Int))
    , TestType (Proxy @(Sum Natural))
    ]

testTypesLeftGCDMonoid :: [TestType LeftGCDMonoid]
testTypesLeftGCDMonoid =
    [ TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Natural))
    , TestType (Proxy @(Text))
    , TestType (Proxy @(Dual Text))
    ]

testTypesRightGCDMonoid :: [TestType RightGCDMonoid]
testTypesRightGCDMonoid =
    [ TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Natural))
    , TestType (Proxy @(Text))
    , TestType (Proxy @(Dual Text))
    ]

testTypesOverlappingGCDMonoid :: [TestType OverlappingGCDMonoid]
testTypesOverlappingGCDMonoid =
    [ TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Natural))
    , TestType (Proxy @(Text))
    , TestType (Proxy @(Dual Text))
    ]

testTypesGCDMonoid :: [TestType GCDMonoid]
testTypesGCDMonoid =
    [ TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Natural))
    ]

testTypesLCMMonoid :: [TestType LCMMonoid]
testTypesLCMMonoid =
    [ TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Natural))
    ]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

makeSpec :: forall k v. Test k v => Spec -> Proxy k -> Proxy v -> Spec
makeSpec spec _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) spec

property :: Testable t => t -> Property
property = checkCoverage . QC.property
