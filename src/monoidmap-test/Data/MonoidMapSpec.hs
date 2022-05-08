{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMapSpec
    where

import Prelude

import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Sum (..) )
import Data.MonoidMap
    ( MonoidMap )
import Data.Set
    ( Set )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
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
import Test.QuickCheck.Classes
    ( eqLaws
    , isListLaws
    , monoidLaws
    , semigroupLaws
    , semigroupMonoidLaws
    , showReadLaws
    )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Classes.Semigroup
    ( cancellativeGCDMonoidLaws
    , cancellativeLaws
    , commutativeLaws
    , gcdMonoidLaws
    , leftCancellativeLaws
    , leftGCDMonoidLaws
    , leftReductiveLaws
    , monoidNullLaws
    , monusLaws
    , overlappingGCDMonoidLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightGCDMonoidLaws
    , rightReductiveLaws
    )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = do
    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(MonoidMap Int String)
            [ eqLaws
            , isListLaws
            , leftCancellativeLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , overlappingGCDMonoidLaws
            , rightCancellativeLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap Int (Sum Natural))
            [ cancellativeGCDMonoidLaws
            , cancellativeLaws
            , commutativeLaws
            , eqLaws
            , gcdMonoidLaws
            , isListLaws
            , leftCancellativeLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap Int (Set Int))
            [ commutativeLaws
            , eqLaws
            , gcdMonoidLaws
            , isListLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , reductiveLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap Int (MonoidMap Int (Sum Natural)))
            [ cancellativeGCDMonoidLaws
            , cancellativeLaws
            , commutativeLaws
            , eqLaws
            , gcdMonoidLaws
            , isListLaws
            , leftCancellativeLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]

    parallel $ describe "Conversion to and from lists" $ do
        it "prop_fromList_toList" $
            prop_fromList_toList & property
        it "prop_toList_fromList" $
            prop_toList_fromList & property

    parallel $ describe "Conversion to and from ordinary maps" $ do
        it "prop_fromMap_toMap" $
            prop_fromMap_toMap & property
        it "prop_toMap_fromMap" $
            prop_toMap_fromMap & property

    parallel $ describe "Deletions" $ do
        it "prop_delete_keysSet" $
            prop_delete_keysSet & property
        it "prop_delete_lookup" $
            prop_delete_lookup & property
        it "prop_delete_member" $
            prop_delete_member & property

    parallel $ describe "Insertions" $ do
        it "prop_insert_keysSet" $
            prop_insert_keysSet & property
        it "prop_insert_lookup" $
            prop_insert_lookup & property
        it "prop_insert_member" $
            prop_insert_member & property
        it "prop_insert_toList" $
            prop_insert_toList & property

    parallel $ describe "Lookups" $ do
        it "prop_lookup_keysSet" $
            prop_lookup_keysSet & property
        it "prop_lookup_member" $
            prop_lookup_member & property

    parallel $ describe "Singletons" $ do
        it "prop_singleton_delete" $
            prop_singleton_delete & property
        it "prop_singleton_keysSet" $
            prop_singleton_keysSet & property
        it "prop_singleton_lookup" $
            prop_singleton_lookup & property
        it "prop_singleton_member" $
            prop_singleton_member & property
        it "prop_singleton_size" $
            prop_singleton_size & property
        it "prop_singleton_toList" $
            prop_singleton_toList & property

--------------------------------------------------------------------------------
-- Conversion to and from lists
--------------------------------------------------------------------------------

prop_fromList_toList :: [(Int, Sum Int)] -> Property
prop_fromList_toList xs =
    MonoidMap.toList (MonoidMap.fromList xs) ===
    Map.toList (Map.filter (/= mempty) (Map.fromListWith (<>) xs))

prop_toList_fromList :: MonoidMap Int (Sum Int) -> Property
prop_toList_fromList xs =
    MonoidMap.fromList (MonoidMap.toList xs) === xs

--------------------------------------------------------------------------------
-- Conversion to and from ordinary maps
--------------------------------------------------------------------------------

prop_fromMap_toMap :: Map Int (Sum Int) -> Property
prop_fromMap_toMap m =
    MonoidMap.toMap (MonoidMap.fromMap m) === Map.filter (/= mempty) m

prop_toMap_fromMap :: MonoidMap Int (Sum Int) -> Property
prop_toMap_fromMap m =
    MonoidMap.fromMap (MonoidMap.toMap m) === m

--------------------------------------------------------------------------------
-- Deletions
--------------------------------------------------------------------------------

prop_delete_keysSet :: MonoidMap Int (Sum Int) -> Int -> Property
prop_delete_keysSet m k =
    Set.member k (MonoidMap.keysSet (MonoidMap.delete k m)) === False
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & checkCoverage

prop_delete_lookup :: MonoidMap Int (Sum Int) -> Int -> Property
prop_delete_lookup m k =
    MonoidMap.lookup k (MonoidMap.delete k m) === mempty
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & checkCoverage

prop_delete_member :: MonoidMap Int (Sum Int) -> Int -> Property
prop_delete_member m k =
    MonoidMap.member k (MonoidMap.delete k m) === False
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & checkCoverage

--------------------------------------------------------------------------------
-- Insertions
--------------------------------------------------------------------------------

prop_insert_keysSet :: MonoidMap Int (Sum Int) -> Int -> Sum Int -> Property
prop_insert_keysSet m k v =
    Set.member k (MonoidMap.keysSet (MonoidMap.insert k v m)) ===
        (v /= mempty)

prop_insert_lookup :: MonoidMap Int (Sum Int) -> Int -> Sum Int -> Property
prop_insert_lookup m k v =
    MonoidMap.lookup k (MonoidMap.insert k v m) === v
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 10
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"
    & checkCoverage

prop_insert_member :: MonoidMap Int (Sum Int) -> Int -> Sum Int -> Property
prop_insert_member m k v =
    MonoidMap.member k (MonoidMap.insert k v m) ===
        (v /= mempty)

prop_insert_toList :: MonoidMap Int (Sum Int) -> Int -> Sum Int -> Property
prop_insert_toList m k v =
    filter ((== k) . fst) (MonoidMap.toList (MonoidMap.insert k v m)) ===
        if v == mempty
        then []
        else [(k, v)]

--------------------------------------------------------------------------------
-- Lookups
--------------------------------------------------------------------------------

prop_lookup_keysSet :: MonoidMap Int (Sum Int) -> Int -> Property
prop_lookup_keysSet m k =
    Set.member k (MonoidMap.keysSet m) === (MonoidMap.lookup k m /= mempty)
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 10
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"
    & checkCoverage

prop_lookup_member :: MonoidMap Int (Sum Int) -> Int -> Property
prop_lookup_member m k =
    MonoidMap.member k m === (MonoidMap.lookup k m /= mempty)
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 10
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"
    & checkCoverage

--------------------------------------------------------------------------------
-- Singletons
--------------------------------------------------------------------------------

prop_singleton_delete :: Int -> Sum Int -> Property
prop_singleton_delete k v =
    MonoidMap.delete k (MonoidMap.singleton k v) === mempty

prop_singleton_keysSet :: Int -> Sum Int -> Property
prop_singleton_keysSet k v =
    MonoidMap.keysSet (MonoidMap.singleton k v) ===
        if v == mempty
        then Set.empty
        else Set.singleton k

prop_singleton_lookup :: Int -> Sum Int -> Property
prop_singleton_lookup k v =
    MonoidMap.lookup k (MonoidMap.singleton k v) === v

prop_singleton_member :: Int -> Sum Int -> Property
prop_singleton_member k v =
    MonoidMap.member k (MonoidMap.singleton k v) === (v /= mempty)

prop_singleton_size :: Int -> Sum Int -> Property
prop_singleton_size k v =
    MonoidMap.size (MonoidMap.singleton k v) ===
        if v == mempty
        then 0
        else 1

prop_singleton_toList :: Int -> Sum Int -> Property
prop_singleton_toList k v =
    MonoidMap.toList (MonoidMap.singleton k v) ===
        if v == mempty
        then []
        else [(k, v)]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance (Arbitrary k, Ord k, Arbitrary v, Eq v, Monoid v) =>
    Arbitrary (MonoidMap k v)
  where
    arbitrary = fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)
    shrink = shrinkMapBy MonoidMap.fromMap MonoidMap.toMap shrink
