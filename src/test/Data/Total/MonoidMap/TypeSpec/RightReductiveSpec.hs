{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.TypeSpec.RightReductiveSpec
    ( spec
    ) where

import Prelude

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Monoid
    ( Dual, Sum (..) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( RightReductive (..) )
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
    ( Spec, describe, it )
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
    , cover
    , functionIntegral
    , functionShow
    , listOf
    , scale
    , shrinkMapBy
    , (===)
    )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Data.Text as Text
import qualified Data.Total.MonoidMap as MonoidMap
import qualified Test.QuickCheck as QC

spec :: Spec
spec = describe "Operations requiring a RightReductive constraint" $ do

    specFor (Proxy @Key) (Proxy @(Set Int))
    specFor (Proxy @Key) (Proxy @(Set Natural))
    specFor (Proxy @Key) (Proxy @(Sum Int))
    specFor (Proxy @Key) (Proxy @(Sum Natural))
    specFor (Proxy @Key) (Proxy @[Int])
    specFor (Proxy @Key) (Proxy @[Natural])
    specFor (Proxy @Key) (Proxy @(Text))
    specFor (Proxy @Key) (Proxy @(Dual [Int]))
    specFor (Proxy @Key) (Proxy @(Dual [Natural]))
    specFor (Proxy @Key) (Proxy @(Dual Text))

type TestConstraints k v =
    ( Arbitrary k
    , Arbitrary v
    , CoArbitrary k
    , CoArbitrary v
    , Eq v
    , Function k
    , Function v
    , MonoidNull v
    , Ord k
    , Show k
    , Show v
    , Typeable k
    , Typeable v
    )

specFor
    :: forall k v. (TestConstraints k v, RightReductive v)
    => Proxy k
    -> Proxy v
    -> Spec
specFor _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do

    it "prop_stripSuffix_isJust" $
        prop_stripSuffix_isJust
            @k @v & property
    it "prop_stripSuffix_get" $
        prop_stripSuffix_get
            @k @v & property
    it "prop_stripSuffix_mappend" $
        prop_stripSuffix_mappend
            @k @v & property

prop_stripSuffix_isJust
    :: (Ord k, MonoidNull v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_stripSuffix_isJust m1 m2 =
    isJust (stripSuffix m1 m2) === m1 `isSuffixOf` m2
    & cover 1
        (m1 `isSuffixOf` m2)
        "m1 `isSuffixOf` m2"

prop_stripSuffix_get
    :: (Ord k, Eq v, MonoidNull v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_stripSuffix_get m1 m2 k = QC.property $
    all
        (\r ->
            Just (MonoidMap.get k r)
            ==
            stripSuffix (MonoidMap.get k m1) (MonoidMap.get k m2)
        )
        (stripSuffix m1 m2)
    & cover 1
        (isJust (stripSuffix m1 m2))
        "isJust (stripSuffix m1 m2)"

prop_stripSuffix_mappend
    :: (Ord k, Eq v, MonoidNull v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_stripSuffix_mappend m1 m2 = QC.property $
    all
        (\r -> r <> m1 == m2)
        (stripSuffix m1 m2)
    & cover 1
        (isJust (stripSuffix m1 m2))
        "isJust (stripSuffix m1 m2)"

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
