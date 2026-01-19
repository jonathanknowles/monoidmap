{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Test.Common
    ( Key
    , Test
    , TestKey
    , TestValueType (..)
    , testValueTypesAll
    , testValueTypesGroup
    , testValueTypesMonus
    , testValueTypesLeftReductive
    , testValueTypesRightReductive
    , testValueTypesReductive
    , testValueTypesLeftGCDMonoid
    , testValueTypesRightGCDMonoid
    , testValueTypesOverlappingGCDMonoid
    , testValueTypesGCDMonoid
    , testValueTypesLCMMonoid
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
    ( Dual, Product, Sum )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, OverlappingGCDMonoid, RightGCDMonoid )
import Data.Monoid.LCM
    ( LCMMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal
    ( MonoidMap )
import Data.Proxy
    ( Proxy (Proxy) )
import Data.Semigroup.Cancellative
    ( LeftReductive, Reductive, RightReductive )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable, typeRep )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe )
import Test.Key
    ( Key2, Key4 )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Function (..)
    , Property
    , Testable
    , arbitrarySizedNatural
    , checkCoverage
    , coarbitraryIntegral
    , coarbitraryShow
    , frequency
    , functionIntegral
    , functionMap
    , functionShow
    , listOf
    , scale
    , shrinkIntegral
    , shrinkMapBy
    )

import qualified Data.MonoidMap.Internal as MonoidMap
import qualified Data.Text as Text
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

instance (CoArbitrary k, CoArbitrary v) =>
    CoArbitrary (MonoidMap k v)
  where
    coarbitrary = coarbitrary . MonoidMap.toMap

instance (Function k, Function v, Ord k, MonoidNull v) =>
    Function (MonoidMap k v)
  where
    function = functionMap MonoidMap.toMap MonoidMap.fromMap

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral

instance CoArbitrary Natural where
    coarbitrary = coarbitraryIntegral

instance Function Natural where
    function = functionIntegral

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

type SmallKey = Key2
type Key = Key4

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
-- Test value types (for different type class constraints)
--------------------------------------------------------------------------------

data TestValueType (c :: Type -> Constraint) =
    forall v. (TestValue v, c v) => TestValueType (Proxy v)

testValueTypesAll :: [TestValueType MonoidNull]
testValueTypesAll =
    [ TestValueType (Proxy @(Dual Text))
    , TestValueType (Proxy @(Dual [Int]))
    , TestValueType (Proxy @(Dual [Natural]))
    , TestValueType (Proxy @(Product Int))
    , TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Int))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(Text))
    , TestValueType (Proxy @[Int])
    , TestValueType (Proxy @[Natural])
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Int)))
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

testValueTypesGroup :: [TestValueType Group]
testValueTypesGroup =
    [ TestValueType (Proxy @(Sum Int))
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Int)))
    ]

testValueTypesMonus :: [TestValueType Monus]
testValueTypesMonus =
    [ TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

testValueTypesLeftReductive :: [TestValueType LeftReductive]
testValueTypesLeftReductive =
    [ TestValueType (Proxy @(Dual Text))
    , TestValueType (Proxy @(Dual [Int]))
    , TestValueType (Proxy @(Dual [Natural]))
    , TestValueType (Proxy @(Product Int))
    , TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Int))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(Text))
    , TestValueType (Proxy @[Int])
    , TestValueType (Proxy @[Natural])
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

testValueTypesRightReductive :: [TestValueType RightReductive]
testValueTypesRightReductive =
    [ TestValueType (Proxy @(Dual Text))
    , TestValueType (Proxy @(Dual [Int]))
    , TestValueType (Proxy @(Dual [Natural]))
    , TestValueType (Proxy @(Product Int))
    , TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Int))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(Text))
    , TestValueType (Proxy @[Int])
    , TestValueType (Proxy @[Natural])
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

testValueTypesReductive :: [TestValueType Reductive]
testValueTypesReductive =
    [ TestValueType (Proxy @(Product Int))
    , TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Int))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

testValueTypesLeftGCDMonoid :: [TestValueType LeftGCDMonoid]
testValueTypesLeftGCDMonoid =
    [ TestValueType (Proxy @(Dual Text))
    , TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(Text))
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

testValueTypesRightGCDMonoid :: [TestValueType RightGCDMonoid]
testValueTypesRightGCDMonoid =
    [ TestValueType (Proxy @(Dual Text))
    , TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(Text))
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

testValueTypesOverlappingGCDMonoid :: [TestValueType OverlappingGCDMonoid]
testValueTypesOverlappingGCDMonoid =
    [ TestValueType (Proxy @(Dual Text))
    , TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(Text))
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

testValueTypesGCDMonoid :: [TestValueType GCDMonoid]
testValueTypesGCDMonoid =
    [ TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

testValueTypesLCMMonoid :: [TestValueType LCMMonoid]
testValueTypesLCMMonoid =
    [ TestValueType (Proxy @(Product Natural))
    , TestValueType (Proxy @(Set Int))
    , TestValueType (Proxy @(Set Natural))
    , TestValueType (Proxy @(Sum Natural))
    , TestValueType (Proxy @(MonoidMap SmallKey (Sum Natural)))
    ]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

makeSpec :: forall k v. Test k v => Spec -> Proxy k -> Proxy v -> Spec
makeSpec spec _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) spec

property :: Testable t => t -> Property
property = checkCoverage . QC.property
