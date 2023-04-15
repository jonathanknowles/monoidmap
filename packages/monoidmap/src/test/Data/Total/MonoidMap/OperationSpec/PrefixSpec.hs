{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.PrefixSpec
    ( spec
    ) where

import Prelude

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Monoid
    ( Dual, Sum (..) )
import Data.Monoid.GCD
    ( LeftGCDMonoid (..) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( LeftReductive (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Total.MonoidMap
    ( MonoidMap )
import Data.Typeable
    ( typeRep )
import Numeric.Natural
    ( Natural )
import Test.Common
    ( Key, TestConstraints, property )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, cover, (===) )

import qualified Data.Total.MonoidMap as MonoidMap
import qualified Test.QuickCheck as QC

spec :: Spec
spec = describe "Prefixes" $ do

    specLeftReductive (Proxy @Key) (Proxy @(Set Int))
    specLeftReductive (Proxy @Key) (Proxy @(Set Natural))
    specLeftReductive (Proxy @Key) (Proxy @(Sum Int))
    specLeftReductive (Proxy @Key) (Proxy @(Sum Natural))
    specLeftReductive (Proxy @Key) (Proxy @[Int])
    specLeftReductive (Proxy @Key) (Proxy @[Natural])
    specLeftReductive (Proxy @Key) (Proxy @(Text))
    specLeftReductive (Proxy @Key) (Proxy @(Dual [Int]))
    specLeftReductive (Proxy @Key) (Proxy @(Dual [Natural]))
    specLeftReductive (Proxy @Key) (Proxy @(Dual Text))

    specLeftGCDMonoid (Proxy @Key) (Proxy @(Set Int))
    specLeftGCDMonoid (Proxy @Key) (Proxy @(Set Natural))
    specLeftGCDMonoid (Proxy @Key) (Proxy @(Sum Natural))
    specLeftGCDMonoid (Proxy @Key) (Proxy @[Int])
    specLeftGCDMonoid (Proxy @Key) (Proxy @[Natural])
    specLeftGCDMonoid (Proxy @Key) (Proxy @(Text))
    specLeftGCDMonoid (Proxy @Key) (Proxy @(Dual [Int]))
    specLeftGCDMonoid (Proxy @Key) (Proxy @(Dual [Natural]))
    specLeftGCDMonoid (Proxy @Key) (Proxy @(Dual Text))

specLeftReductive
    :: forall k v. (TestConstraints k v, LeftReductive v)
    => Proxy k
    -> Proxy v
    -> Spec
specLeftReductive _k _v =
    describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do
        it "prop_stripPrefix_isJust" $
            prop_stripPrefix_isJust
                @k @v & property
        it "prop_stripPrefix_get" $
            prop_stripPrefix_get
                @k @v & property
        it "prop_stripPrefix_mappend" $
            prop_stripPrefix_mappend
                @k @v & property

specLeftGCDMonoid
    :: forall k v. (TestConstraints k v, LeftGCDMonoid v)
    => Proxy k
    -> Proxy v
    -> Spec
specLeftGCDMonoid _k _v =
    describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do
        it "prop_commonPrefix_get" $
            prop_commonPrefix_get
                @k @v & property

prop_stripPrefix_isJust
    :: (Ord k, MonoidNull v, LeftReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_stripPrefix_isJust m1 m2 =
    isJust (stripPrefix m1 m2) === m1 `isPrefixOf` m2
    & cover 1
        (m1 `isPrefixOf` m2)
        "m1 `isPrefixOf` m2"

prop_stripPrefix_get
    :: (Ord k, Eq v, MonoidNull v, LeftReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_stripPrefix_get m1 m2 k = QC.property $
    all
        (\r ->
            Just (MonoidMap.get k r)
            ==
            stripPrefix (MonoidMap.get k m1) (MonoidMap.get k m2)
        )
        (stripPrefix m1 m2)
    & cover 1
        (isJust (stripPrefix m1 m2))
        "isJust (stripPrefix m1 m2)"

prop_stripPrefix_mappend
    :: (Ord k, Eq v, MonoidNull v, LeftReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_stripPrefix_mappend m1 m2 = QC.property $
    all
        (\r -> m1 <> r == m2)
        (stripPrefix m1 m2)
    & cover 1
        (isJust (stripPrefix m1 m2))
        "isJust (stripPrefix m1 m2)"

prop_commonPrefix_get
    :: (Ord k, Eq v, Show v, MonoidNull v, LeftGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_commonPrefix_get m1 m2 k =
    MonoidMap.get k (commonPrefix m1 m2)
    ===
    commonPrefix (MonoidMap.get k m1) (MonoidMap.get k m2)
    & cover 1
        (MonoidMap.get k (commonPrefix m1 m2) == mempty)
        "MonoidMap.get k (commonPrefix m1 m2) == mempty"
    & cover 0.1
        (MonoidMap.get k (commonPrefix m1 m2) /= mempty)
        "MonoidMap.get k (commonPrefix m1 m2) /= mempty"
