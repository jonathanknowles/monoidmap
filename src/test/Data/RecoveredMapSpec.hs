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
import Test.Hspec
    ( Spec, describe, it, parallel )
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

import qualified Data.MonoidMap.Internal.RecoveredMap as RMap
import qualified Data.Strict.Map as OMap
import qualified Data.Strict.Set as Set

spec :: Spec
spec = do

    parallel $ describe "Conversion to and from lists" $ do
        it "prop_fromList_toList" $
            prop_fromList_toList & property

    parallel $ describe "Empty" $ do
        it "prop_empty_keysSet" $
            prop_empty_keysSet & property
        it "prop_empty_lookup" $
            prop_empty_lookup & property
        it "prop_empty_show" $
            prop_empty_show & property
        it "prop_empty_toList" $
            prop_empty_toList & property

    parallel $ describe "Singleton" $ do
        it "prop_singleton_keysSet" $
            prop_singleton_keysSet & property
        it "prop_singleton_lookup" $
            prop_singleton_lookup & property
        it "prop_singleton_show" $
            prop_singleton_show & property
        it "prop_singleton_toList" $
            prop_singleton_toList & property

    parallel $ describe "Append" $ do
        it "prop_append_toList" $
            prop_append_toList & property

    parallel $ describe "Delete" $ do
        it "prop_delete_lookup" $
            prop_delete_lookup & property
        it "prop_delete_member" $
            prop_delete_member & property
        it "prop_delete_toList" $
            prop_delete_toList & property

    parallel $ describe "Insert" $ do
        it "prop_insert_lookup" $
            prop_insert_lookup & property
        it "prop_insert_member" $
            prop_insert_member & property
        it "prop_insert_toList" $
            prop_insert_toList & property

--------------------------------------------------------------------------------
-- Test types
--------------------------------------------------------------------------------

type Key = Int
type Value = Sum Int

--------------------------------------------------------------------------------
-- Conversion to and from lists
--------------------------------------------------------------------------------

prop_fromList_toList :: [(Key, Value)] -> Property
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

prop_empty_keysSet :: Property
prop_empty_keysSet =
    (===)
        (RMap.keysSet (RMap.empty @Key @Value))
        (OMap.keysSet (OMap.empty @Key @Value))

prop_empty_lookup :: Key -> Property
prop_empty_lookup k =
    (===)
        (RMap.lookup k (RMap.empty @Key @Value))
        (OMap.lookup k (OMap.empty @Key @Value))

prop_empty_show :: Property
prop_empty_show =
    (===)
        (show (RMap.empty @Key @Value))
        (show (OMap.empty @Key @Value))

prop_empty_toList :: Property
prop_empty_toList =
    (===)
        (RMap.toList (RMap.empty @Key @Value))
        (OMap.toList (OMap.empty @Key @Value))

--------------------------------------------------------------------------------
-- Singleton
--------------------------------------------------------------------------------

prop_singleton_keysSet :: Key -> Value -> Property
prop_singleton_keysSet k v =
    (===)
        (RMap.keysSet (RMap.singleton k v))
        (OMap.keysSet (OMap.singleton k v))

prop_singleton_lookup :: Key -> Value -> Property
prop_singleton_lookup k v =
    (===)
        (RMap.lookup k (RMap.singleton k v))
        (OMap.lookup k (OMap.singleton k v))

prop_singleton_show :: Key -> Value -> Property
prop_singleton_show k v =
    (===)
        (show (RMap.singleton k v))
        (show (OMap.singleton k v))

prop_singleton_toList :: Key -> Value -> Property
prop_singleton_toList k v =
    (===)
        (RMap.toList (RMap.singleton k v))
        (OMap.toList (OMap.singleton k v))

--------------------------------------------------------------------------------
-- Append
--------------------------------------------------------------------------------

prop_append_toList :: [(Key, Value)] -> [(Key, Value)] -> Property
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

prop_delete_lookup :: [(Key, Value)] -> Key -> Property
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

prop_delete_member :: [(Key, Value)] -> Key -> Property
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

prop_delete_toList :: [(Key, Value)] -> Key -> Property
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

prop_insert_lookup :: [(Key, Value)] -> Key -> Value -> Property
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

prop_insert_member :: [(Key, Value)] -> Key -> Value -> Property
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

prop_insert_toList :: [(Key, Value)] -> Key -> Value -> Property
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
