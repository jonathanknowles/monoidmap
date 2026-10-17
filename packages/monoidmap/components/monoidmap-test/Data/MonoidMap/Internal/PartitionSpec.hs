{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.PartitionSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.MonoidMap.Internal
    ( MonoidMap )
import Data.Proxy
    ( Proxy (..) )
import Test.Common
    ( Key
    , Test
    , TestValueType (TestValueType)
    , makeSpec
    , property
    , testValueTypesAll
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..), Property, applyFun, applyFun2, cover, (===) )

import qualified Data.MonoidMap.Internal as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Partitioning" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    it "prop_partition_filter" $
        prop_partition_filter
            @k @v & property
    it "prop_partition_append" $
        prop_partition_append
            @k @v & property
    it "prop_partition_disjoint" $
        prop_partition_disjoint
            @k @v & property
    it "prop_partitionKeys_filterKeys" $
        prop_partitionKeys_filterKeys
            @k @v & property
    it "prop_partitionKeys_append" $
        prop_partitionKeys_append
            @k @v & property
    it "prop_partitionKeys_disjoint" $
        prop_partitionKeys_disjoint
            @k @v & property
    it "prop_partitionWithKey_filterWithKey" $
        prop_partitionWithKey_filterWithKey
            @k @v & property
    it "prop_partitionWithKey_append" $
        prop_partitionWithKey_append
            @k @v & property
    it "prop_partitionWithKey_disjoint" $
        prop_partitionWithKey_disjoint
            @k @v & property

prop_partition_filter
    :: Test k v => Fun v Bool -> MonoidMap k v -> Property
prop_partition_filter (applyFun -> f) m =
    MonoidMap.partition f m === (m1, m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    m1 = MonoidMap.filter f m
    m2 = MonoidMap.filter (not . f) m

prop_partition_append
    :: Test k v => Fun v Bool -> MonoidMap k v -> Property
prop_partition_append (applyFun -> f) m =
    m1 <> m2 === m
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partition f m

prop_partition_disjoint
    :: Test k v => Fun v Bool -> MonoidMap k v -> Property
prop_partition_disjoint (applyFun -> f) m =
    Set.disjoint
        (MonoidMap.nonNullKeys m1)
        (MonoidMap.nonNullKeys m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partition f m

prop_partitionKeys_filterKeys
    :: Test k v => Fun k Bool -> MonoidMap k v -> Property
prop_partitionKeys_filterKeys (applyFun -> f) m =
    MonoidMap.partitionKeys f m === (m1, m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    m1 = MonoidMap.filterKeys f m
    m2 = MonoidMap.filterKeys (not . f) m

prop_partitionKeys_append
    :: Test k v => Fun k Bool -> MonoidMap k v -> Property
prop_partitionKeys_append (applyFun -> f) m =
    m1 <> m2 === m
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partitionKeys f m

prop_partitionKeys_disjoint
    :: Test k v => Fun k Bool -> MonoidMap k v -> Property
prop_partitionKeys_disjoint (applyFun -> f) m =
    Set.disjoint
        (MonoidMap.nonNullKeys m1)
        (MonoidMap.nonNullKeys m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partitionKeys f m

prop_partitionWithKey_filterWithKey
    :: Test k v => Fun (k, v) Bool -> MonoidMap k v -> Property
prop_partitionWithKey_filterWithKey (applyFun2 -> f) m =
    MonoidMap.partitionWithKey f m === (m1, m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    m1 = MonoidMap.filterWithKey f m
    m2 = MonoidMap.filterWithKey ((fmap . fmap) not f) m

prop_partitionWithKey_append
    :: Test k v => Fun (k, v) Bool -> MonoidMap k v -> Property
prop_partitionWithKey_append (applyFun2 -> f) m =
    m1 <> m2 === m
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partitionWithKey f m

prop_partitionWithKey_disjoint
    :: Test k v => Fun (k, v) Bool -> MonoidMap k v -> Property
prop_partitionWithKey_disjoint (applyFun2 -> f) m =
    Set.disjoint
        (MonoidMap.nonNullKeys m1)
        (MonoidMap.nonNullKeys m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partitionWithKey f m
