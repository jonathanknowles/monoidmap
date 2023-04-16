{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.PartitionSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Total.MonoidMap
    ( MonoidMap )
import Data.Typeable
    ( typeRep )
import Test.Common
    ( Key
    , TestConstraints
    , TestInstance (TestInstance)
    , property
    , testInstancesMonoidNull
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..), Property, applyFun, applyFun2, cover, (===) )

import qualified Data.Set as Set
import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Partitioning" $ do

    forM_ testInstancesMonoidNull $ \(TestInstance p) -> specFor (Proxy @Key) p

specFor :: forall k v. TestConstraints k v => Proxy k -> Proxy v -> Spec
specFor _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do

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
    :: TestConstraints k v => Fun v Bool -> MonoidMap k v -> Property
prop_partition_filter (applyFun -> f) m =
    MonoidMap.partition f m === (m1, m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    m1 = MonoidMap.filter f m
    m2 = MonoidMap.filter (not . f) m

prop_partition_append
    :: TestConstraints k v => Fun v Bool -> MonoidMap k v -> Property
prop_partition_append (applyFun -> f) m =
    m1 <> m2 === m
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partition f m

prop_partition_disjoint
    :: TestConstraints k v => Fun v Bool -> MonoidMap k v -> Property
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
    :: TestConstraints k v => Fun k Bool -> MonoidMap k v -> Property
prop_partitionKeys_filterKeys (applyFun -> f) m =
    MonoidMap.partitionKeys f m === (m1, m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    m1 = MonoidMap.filterKeys f m
    m2 = MonoidMap.filterKeys (not . f) m

prop_partitionKeys_append
    :: TestConstraints k v => Fun k Bool -> MonoidMap k v -> Property
prop_partitionKeys_append (applyFun -> f) m =
    m1 <> m2 === m
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partitionKeys f m

prop_partitionKeys_disjoint
    :: TestConstraints k v => Fun k Bool -> MonoidMap k v -> Property
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
    :: TestConstraints k v => Fun (k, v) Bool -> MonoidMap k v -> Property
prop_partitionWithKey_filterWithKey (applyFun2 -> f) m =
    MonoidMap.partitionWithKey f m === (m1, m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    m1 = MonoidMap.filterWithKey f m
    m2 = MonoidMap.filterWithKey ((fmap . fmap) not f) m

prop_partitionWithKey_append
    :: TestConstraints k v => Fun (k, v) Bool -> MonoidMap k v -> Property
prop_partitionWithKey_append (applyFun2 -> f) m =
    m1 <> m2 === m
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partitionWithKey f m

prop_partitionWithKey_disjoint
    :: TestConstraints k v => Fun (k, v) Bool -> MonoidMap k v -> Property
prop_partitionWithKey_disjoint (applyFun2 -> f) m =
    Set.disjoint
        (MonoidMap.nonNullKeys m1)
        (MonoidMap.nonNullKeys m2)
    & cover 2
        (MonoidMap.nonNull m1 && MonoidMap.nonNull m2)
        "MonoidMap.nonNull m1 && MonoidMap.nonNull m2"
  where
    (m1, m2) = MonoidMap.partitionWithKey f m
