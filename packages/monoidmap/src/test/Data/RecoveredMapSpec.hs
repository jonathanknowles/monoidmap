{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use null" #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Data.RecoveredMapSpec
    where

import Prelude

import Data.Function
    ( on, (&) )
import Data.List
    ( nubBy )
import Data.Monoid
    ( Sum (..) )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , cover
    , listOf
    , property
    , shrinkMapBy
    , (===)
    )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Data.Map.Strict as OMap
import qualified Data.MonoidMap.Internal.RecoveredMap as RMap
import qualified Data.Set as Set

spec :: Spec
spec = specFor (Proxy @Key) (Proxy @Value)

specFor
    :: forall k v. () =>
        ( Arbitrary k
        , Arbitrary v
        , Eq v
        , Ord k
        , Show k
        , Show v
        )
    => Proxy k
    -> Proxy v
    -> Spec
specFor _keyType _valueType = do

    describe "Conversion to and from lists" $ do
        it "prop_fromList_toList" $
            prop_fromList_toList
                @k @v & property

    describe "Empty" $ do
        it "prop_empty_keysSet" $
            prop_empty_keysSet
                @k & property
        it "prop_empty_lookup" $
            prop_empty_lookup
                @k @v & property
        it "prop_empty_show" $
            prop_empty_show
                @k @v & property
        it "prop_empty_toList" $
            prop_empty_toList
                @k @v & property

    describe "Singleton" $ do
        it "prop_singleton_keysSet" $
            prop_singleton_keysSet
                @k @v & property
        it "prop_singleton_lookup" $
            prop_singleton_lookup
                @k @v & property
        it "prop_singleton_show" $
            prop_singleton_show
                @k @v & property
        it "prop_singleton_toList" $
            prop_singleton_toList
                @k @v & property

    describe "Append" $ do
        it "prop_append_toList" $
            prop_append_toList
                @k @v & property

    describe "Delete" $ do
        it "prop_delete_lookup" $
            prop_delete_lookup
                @k @v & property
        it "prop_delete_member" $
            prop_delete_member
                @k @v & property
        it "prop_delete_toList" $
            prop_delete_toList
                @k @v & property

    describe "Insert" $ do
        it "prop_insert_lookup" $
            prop_insert_lookup
                @k @v & property
        it "prop_insert_member" $
            prop_insert_member
                @k @v & property
        it "prop_insert_toList" $
            prop_insert_toList
                @k @v & property

--------------------------------------------------------------------------------
-- Test types
--------------------------------------------------------------------------------

type Key = Int
type Value = Sum Int

--------------------------------------------------------------------------------
-- Conversion to and from lists
--------------------------------------------------------------------------------

prop_fromList_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => [(k, v)]
    -> Property
prop_fromList_toList kvs =
    (===)
        (RMap.toList (RMap.fromList kvs))
        (OMap.toList (OMap.fromList kvs))
    & cover 10
        (length kvs > 1 && length (nubBy ((==) `on` fst) kvs) /= length kvs)
        "length kvs > 1 && length (nubBy ((==) `on` fst) kvs) /= length kvs"
    & cover 10
        (length kvs > 1 && length (nubBy ((==) `on` fst) kvs) == length kvs)
        "length kvs > 1 && length (nubBy ((==) `on` fst) kvs) == length kvs"
    & checkCoverage

--------------------------------------------------------------------------------
-- Empty
--------------------------------------------------------------------------------

prop_empty_keysSet
    :: forall k. (Eq k, Show k)
    => Property
prop_empty_keysSet =
    (===)
        (RMap.keysSet (RMap.empty @k))
        (OMap.keysSet (OMap.empty @k))

prop_empty_lookup
    :: forall k v. (Ord k, Eq v, Show v)
    => k
    -> Property
prop_empty_lookup k =
    (===)
        (RMap.lookup k (RMap.empty @k @v))
        (OMap.lookup k (OMap.empty @k @v))

prop_empty_show
    :: forall k v. (Show k, Show v)
    => Property
prop_empty_show =
    (===)
        (show (RMap.empty @k @v))
        (show (OMap.empty @k @v))

prop_empty_toList
    :: forall k v. (Eq k, Show k, Eq v, Show v)
    => Property
prop_empty_toList =
    (===)
        (RMap.toList (RMap.empty @k @v))
        (OMap.toList (OMap.empty @k @v))

--------------------------------------------------------------------------------
-- Singleton
--------------------------------------------------------------------------------

prop_singleton_keysSet
    :: forall k v. (Ord k, Show k)
    => k
    -> v
    -> Property
prop_singleton_keysSet k v =
    (===)
        (RMap.keysSet (RMap.singleton k v))
        (OMap.keysSet (OMap.singleton k v))

prop_singleton_lookup
    :: forall k v. (Ord k, Eq v, Show v)
    => k
    -> v
    -> Property
prop_singleton_lookup k v =
    (===)
        (RMap.lookup k (RMap.singleton k v))
        (OMap.lookup k (OMap.singleton k v))

prop_singleton_show
    :: forall k v. (Ord k, Show k, Show v)
    => k
    -> v
    -> Property
prop_singleton_show k v =
    (===)
        (show (RMap.singleton k v))
        (show (OMap.singleton k v))

prop_singleton_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => k
    -> v
    -> Property
prop_singleton_toList k v =
    (===)
        (RMap.toList (RMap.singleton k v))
        (OMap.toList (OMap.singleton k v))

--------------------------------------------------------------------------------
-- Append
--------------------------------------------------------------------------------

prop_append_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => [(k, v)]
    -> [(k, v)]
    -> Property
prop_append_toList kvs1 kvs2 =
    (===)
        (RMap.toList (RMap.fromList kvs1 <> RMap.fromList kvs2))
        (OMap.toList (OMap.fromList kvs1 <> OMap.fromList kvs2))
    & cover 10
        (ks1 `Set.disjoint` ks2)
        "ks1 `Set.disjoint` ks2"
    & cover 10
        (not (ks1 `Set.disjoint` ks2))
        "not (ks1 `Set.disjoint` ks2)"
    & checkCoverage
  where
    ks1 = Set.fromList (fst <$> kvs1)
    ks2 = Set.fromList (fst <$> kvs2)

--------------------------------------------------------------------------------
-- Delete
--------------------------------------------------------------------------------

prop_delete_lookup
    :: forall k v. (Ord k, Eq v, Show v)
    => [(k, v)]
    -> k
    -> Property
prop_delete_lookup kvs k =
    (===)
        (RMap.lookup k (RMap.delete k (RMap.fromList kvs)))
        (OMap.lookup k (OMap.delete k (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"
    & checkCoverage

prop_delete_member
    :: forall k v. (Ord k, Eq v)
    => [(k, v)]
    -> k
    -> Property
prop_delete_member kvs k =
    (===)
        (RMap.member k (RMap.delete k (RMap.fromList kvs)))
        (OMap.member k (OMap.delete k (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"
    & checkCoverage

prop_delete_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => [(k, v)]
    -> k
    -> Property
prop_delete_toList kvs k =
    (===)
        (RMap.toList (RMap.delete k (RMap.fromList kvs)))
        (OMap.toList (OMap.delete k (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"
    & checkCoverage

--------------------------------------------------------------------------------
-- Insert
--------------------------------------------------------------------------------

prop_insert_lookup
    :: forall k v. (Ord k, Eq v, Show v)
    => [(k, v)]
    -> k
    -> v
    -> Property
prop_insert_lookup kvs k v =
    (===)
        (RMap.lookup k (RMap.insert k v (RMap.fromList kvs)))
        (OMap.lookup k (OMap.insert k v (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"
    & checkCoverage

prop_insert_member
    :: forall k v. (Ord k, Eq v)
    => [(k, v)]
    -> k
    -> v
    -> Property
prop_insert_member kvs k v =
    (===)
        (RMap.member k (RMap.insert k v (RMap.fromList kvs)))
        (OMap.member k (OMap.insert k v (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"
    & checkCoverage

prop_insert_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => [(k, v)]
    -> k
    -> v
    -> Property
prop_insert_toList kvs k v =
    (===)
        (RMap.toList (RMap.insert k v (RMap.fromList kvs)))
        (OMap.toList (OMap.insert k v (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"
    & checkCoverage

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance (Arbitrary k, Ord k, Arbitrary v) =>
    Arbitrary (RMap.Map k v)
  where
    arbitrary = RMap.fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)
    shrink = shrinkMapBy RMap.fromList RMap.toList shrink
