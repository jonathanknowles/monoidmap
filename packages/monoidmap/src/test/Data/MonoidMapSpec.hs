{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMapSpec
    where

import Prelude

import Data.Bifunctor
    ( bimap, first, second )
import Data.Function
    ( (&) )
import Data.Group
    ( Group (..) )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Product (..), Sum (..) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap
    ( MonoidMap )
import Data.Proxy
    ( Proxy (..) )
import Data.Ratio
    ( (%) )
import Data.Semigroup.Cancellative
    ( LeftReductive (..), RightReductive (..) )
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
    ( Spec, describe, it )
import Test.Hspec.Unit
    ( UnitTestData1
    , UnitTestData2
    , unitTestData1
    , unitTestData2
    , unitTestSpec
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Fun (..)
    , Function (..)
    , Gen
    , Property
    , Testable
    , applyFun
    , applyFun2
    , checkCoverage
    , choose
    , cover
    , listOf
    , oneof
    , scale
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
import Test.QuickCheck.Classes.Group
    ( groupLaws )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Classes.Monoid.GCD
    ( cancellativeGCDMonoidLaws
    , gcdMonoidLaws
    , leftGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , rightGCDMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.Monus
    ( monusLaws )
import Test.QuickCheck.Classes.Monoid.Null
    ( monoidNullLaws, positiveMonoidLaws )
import Test.QuickCheck.Classes.Semigroup.Cancellative
    ( cancellativeLaws
    , commutativeLaws
    , leftCancellativeLaws
    , leftReductiveLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    )
import Test.QuickCheck.Instances.Natural
    ()
import Test.QuickCheck.Instances.Text
    ()

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set
import qualified Test.QuickCheck as QC

spec :: Spec
spec = do
    specLaws
    specProperties
    specUnit

specLaws :: Spec
specLaws = describe "Laws" $ do

    testLawsMany @(MonoidMap Int String)
        [ eqLaws
        , isListLaws
        , leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Product Integer))
        [ commutativeLaws
        , eqLaws
        , isListLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , reductiveLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Product Natural))
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
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Product Rational))
        [ commutativeLaws
        , eqLaws
        , groupLaws
        , isListLaws
        , monoidLaws
        , monoidNullLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Sum Integer))
        [ cancellativeLaws
        , commutativeLaws
        , eqLaws
        , groupLaws
        , isListLaws
        , leftCancellativeLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , reductiveLaws
        , rightCancellativeLaws
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
        , positiveMonoidLaws
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
        , positiveMonoidLaws
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
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]

specProperties :: Spec
specProperties = describe "Properties" $ do
    specPropertiesFor (Proxy @Int) (Proxy @(Set Int))
    specPropertiesFor (Proxy @Int) (Proxy @(Set Natural))
    specPropertiesFor (Proxy @Int) (Proxy @(Sum Int))
    specPropertiesFor (Proxy @Int) (Proxy @(Sum Natural))
    specPropertiesFor (Proxy @Int) (Proxy @Text)

specPropertiesFor
    :: forall k v. () =>
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
    => Proxy k
    -> Proxy v
    -> Spec
specPropertiesFor keyType valueType = do

    let description = mconcat
            [ "MonoidMap ("
            , show (typeRep keyType)
            , ") ("
            , show (typeRep valueType)
            , ")"
            ]

    let property :: Testable t => t -> Property
        property = checkCoverage . QC.property

    describe description $ do

        describe "Conversion to and from lists" $ do
            it "prop_fromList_toMap" $
                prop_fromList_toMap
                    @k @v & property
            it "prop_fromList_toList" $
                prop_fromList_toList
                    @k @v & property
            it "prop_toList_fromList" $
                prop_toList_fromList
                    @k @v & property

        describe "Conversion to and from ordinary maps" $ do
            it "prop_fromMap_toMap" $
                prop_fromMap_toMap
                    @k @v & property
            it "prop_toMap_fromMap" $
                prop_toMap_fromMap
                    @k @v & property

        describe "Singleton" $ do
            it "prop_singleton_get" $
                prop_singleton_get
                    @k @v & property
            it "prop_singleton_member" $
                prop_singleton_member
                    @k @v & property
            it "prop_singleton_keys" $
                prop_singleton_keys
                    @k @v & property
            it "prop_singleton_null" $
                prop_singleton_null
                    @k @v & property
            it "prop_singleton_delete" $
                prop_singleton_delete
                    @k @v & property
            it "prop_singleton_size" $
                prop_singleton_size
                    @k @v & property
            it "prop_singleton_toList" $
                prop_singleton_toList
                    @k @v & property

        describe "Get" $ do
            it "prop_get_member" $
                prop_get_member
                    @k @v & property
            it "prop_get_keys" $
                prop_get_keys
                    @k @v & property

        describe "Set" $ do
            it "prop_set_get" $
                prop_set_get
                    @k @v & property
            it "prop_set_member" $
                prop_set_member
                    @k @v & property
            it "prop_set_keys" $
                prop_set_keys
                    @k @v & property
            it "prop_set_toList" $
                prop_set_toList
                    @k @v & property

        describe "Nullify" $ do
            it "prop_delete_get" $
                prop_delete_get
                    @k @v & property
            it "prop_delete_member" $
                prop_delete_member
                    @k @v & property
            it "prop_delete_keys" $
                prop_delete_keys
                    @k @v & property

        describe "Keys" $ do
            it "prop_keys_get" $
                prop_keys_get
                    @k @v & property

        describe "Filtering" $ do
            it "prop_filter_toList" $
                prop_filter_toList
                    @k @v & property
            it "prop_filterKeys_filter" $
                prop_filterKeys_filter
                    @k @v & property
            it "prop_filterKeys_toList" $
                prop_filterKeys_toList
                    @k @v & property
            it "prop_filterValues_filter" $
                prop_filterValues_filter
                    @k @v & property
            it "prop_filterValues_toList" $
                prop_filterValues_toList
                    @k @v & property

        describe "Partitioning" $ do
            it "prop_partition_filter" $
                prop_partition_filter
                    @k @v & property
            it "prop_partitionKeys_filterKeys" $
                prop_partitionKeys_filterKeys
                    @k @v & property
            it "prop_partitionValues_filterValues" $
                prop_partitionValues_filterValues
                    @k @v & property

        describe "Slicing" $ do
            it "prop_take_toList_fromList" $
                prop_take_toList_fromList
                    @k @v & property
            it "prop_drop_toList_fromList" $
                prop_drop_toList_fromList
                    @k @v & property
            it "prop_splitAt_toList_fromList" $
                prop_splitAt_toList_fromList
                    @k @v & property

        describe "Mapping" $ do
            it "prop_map_asList" $
                prop_map_asList
                    @k @v & property
            it "prop_mapWith_asList" $
                prop_mapWith_asList
                    @k @v & property
            it "prop_mapKeys_asList" $
                prop_mapKeys_asList
                    @k @v & property
            it "prop_mapKeysWith_asList" $
                prop_mapKeysWith_asList
                    @k @v & property
            it "prop_mapValues_asList" $
                prop_mapValues_asList
                    @k @v & property

specUnit :: Spec
specUnit = describe "Unit tests" $ do

    describe "Group" $ do

        unitTestSpec_Group_invert_Product_Rational
        unitTestSpec_Group_invert_Sum_Integer
        unitTestSpec_Group_pow_Product_Rational
        unitTestSpec_Group_pow_Sum_Integer
        unitTestSpec_Group_subtract_Product_Rational
        unitTestSpec_Group_subtract_Sum_Integer

    describe "Reductive" $ do

        unitTestSpec_Reductive_isPrefixOf_String
        unitTestSpec_Reductive_isPrefixOf_Sum_Natural
        unitTestSpec_Reductive_isSuffixOf_String
        unitTestSpec_Reductive_isSuffixOf_Sum_Natural

--------------------------------------------------------------------------------
-- Conversion to and from lists
--------------------------------------------------------------------------------

prop_fromList_toMap
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => [(k, v)]
    -> Property
prop_fromList_toMap kvs =
    MonoidMap.toMap m === Map.filter (/= mempty) o
    & cover 1
        (MonoidMap.notNull m && MonoidMap.size m /= Map.size o)
        "MonoidMap.notNull m && MonoidMap.size m /= Map.size o"
    & cover 1
        (MonoidMap.notNull m && MonoidMap.size m == Map.size o)
        "MonoidMap.notNull m && MonoidMap.size m == Map.size o"
  where
    m = MonoidMap.fromList kvs
    o = Map.fromListWith (<>) kvs

prop_fromList_toList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => [(k, v)]
    -> Property
prop_fromList_toList kvs =
    MonoidMap.toList m === Map.toList (Map.filter (/= mempty) o)
    & cover 1
        (MonoidMap.notNull m && MonoidMap.size m /= Map.size o)
        "MonoidMap.notNull m && MonoidMap.size m /= Map.size o"
    & cover 1
        (MonoidMap.notNull m && MonoidMap.size m == Map.size o)
        "MonoidMap.notNull m && MonoidMap.size m == Map.size o"
  where
    m = MonoidMap.fromList kvs
    o = Map.fromListWith (<>) kvs

prop_toList_fromList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => MonoidMap k v
    -> Property
prop_toList_fromList m =
    MonoidMap.fromList (MonoidMap.toList m) === m
    & cover 1
        (MonoidMap.notNull m)
        "MonoidMap.notNull m"

--------------------------------------------------------------------------------
-- Conversion to and from ordinary maps
--------------------------------------------------------------------------------

prop_fromMap_toMap
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Map k v
    -> Property
prop_fromMap_toMap o =
    MonoidMap.toMap m === Map.filter (/= mempty) o
    & cover 1
        (MonoidMap.notNull m && MonoidMap.size m /= Map.size o)
        "MonoidMap.notNull m && MonoidMap.size m /= Map.size o"
    & cover 1
        (MonoidMap.notNull m && MonoidMap.size m == Map.size o)
        "MonoidMap.notNull m && MonoidMap.size m == Map.size o"
  where
    m = MonoidMap.fromMap o

prop_toMap_fromMap
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => MonoidMap k v
    -> Property
prop_toMap_fromMap m =
    MonoidMap.fromMap (MonoidMap.toMap m) === m

--------------------------------------------------------------------------------
-- Singleton
--------------------------------------------------------------------------------

prop_singleton_get
    :: (Ord k, Eq v, MonoidNull v, Show v)
    => k
    -> v
    -> Property
prop_singleton_get k v =
    MonoidMap.get k (MonoidMap.singleton k v) === v
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

prop_singleton_member
    :: (Ord k, Eq v, MonoidNull v)
    => k
    -> v
    -> Property
prop_singleton_member k v =
    MonoidMap.member k (MonoidMap.singleton k v) === (v /= mempty)
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

prop_singleton_keys
    :: (Ord k, Show k, Eq v, MonoidNull v)
    => k
    -> v
    -> Property
prop_singleton_keys k v =
    MonoidMap.keys (MonoidMap.singleton k v) ===
        (if v == mempty then Set.empty else Set.singleton k)
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

prop_singleton_null
    :: (Ord k, Eq v, MonoidNull v)
    => k
    -> v
    -> Property
prop_singleton_null k v =
    MonoidMap.null (MonoidMap.singleton k v) === (v == mempty)
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

prop_singleton_delete
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => k
    -> v
    -> Property
prop_singleton_delete k v =
    MonoidMap.delete k (MonoidMap.singleton k v) === mempty
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

prop_singleton_size
    :: (Ord k, Eq v, MonoidNull v)
    => k
    -> v
    -> Property
prop_singleton_size k v =
    MonoidMap.size (MonoidMap.singleton k v) ===
        (if v == mempty then 0 else 1)
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

prop_singleton_toList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => k
    -> v
    -> Property
prop_singleton_toList k v =
    MonoidMap.toList (MonoidMap.singleton k v) ===
        [(k, v) | v /= mempty]
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

--------------------------------------------------------------------------------
-- Get
--------------------------------------------------------------------------------

prop_get_member
    :: (Ord k, Eq v, MonoidNull v)
    => MonoidMap k v
    -> k
    -> Property
prop_get_member m k =
    MonoidMap.member k m === (MonoidMap.get k m /= mempty)
    & cover 1
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 1
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"

prop_get_keys
    :: (Ord k, Eq v, MonoidNull v)
    => MonoidMap k v
    -> k
    -> Property
prop_get_keys m k =
    Set.member k (MonoidMap.keys m) === (MonoidMap.get k m /= mempty)
    & cover 1
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 1
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"

--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

prop_set_get
    :: (Ord k, Eq v, MonoidNull v, Show v)
    => MonoidMap k v
    -> k
    -> v
    -> Property
prop_set_get m k v =
    MonoidMap.get k (MonoidMap.set k v m) === v
    & cover 1
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 1
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"

prop_set_member
    :: (Ord k, Eq v, MonoidNull v)
    => MonoidMap k v
    -> k
    -> v
    -> Property
prop_set_member m k v =
    MonoidMap.member k (MonoidMap.set k v m) ===
        (v /= mempty)
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

prop_set_keys
    :: (Ord k, Eq v, MonoidNull v)
    => MonoidMap k v
    -> k
    -> v
    -> Property
prop_set_keys m k v =
    Set.member k (MonoidMap.keys (MonoidMap.set k v m)) ===
        (v /= mempty)
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

prop_set_toList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => MonoidMap k v
    -> k
    -> v
    -> Property
prop_set_toList m k v =
    filter ((== k) . fst) (MonoidMap.toList (MonoidMap.set k v m)) ===
        [(k, v) | v /= mempty]
    & cover 1
        (v == mempty)
        "v == mempty"
    & cover 1
        (v /= mempty)
        "v /= mempty"

--------------------------------------------------------------------------------
-- Nullify
--------------------------------------------------------------------------------

prop_delete_get
    :: (Ord k, Eq v, Monoid v, Show v)
    => MonoidMap k v
    -> k
    -> Property
prop_delete_get m k =
    MonoidMap.get k (MonoidMap.delete k m) === mempty
    & cover 1
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 1
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"

prop_delete_member
    :: Ord k
    => MonoidMap k v
    -> k
    -> Property
prop_delete_member m k =
    MonoidMap.member k (MonoidMap.delete k m) === False
    & cover 1
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 1
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"

prop_delete_keys
    :: Ord k
    => MonoidMap k v
    -> k
    -> Property
prop_delete_keys m k =
    Set.member k (MonoidMap.keys (MonoidMap.delete k m)) === False
    & cover 1
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 1
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"

--------------------------------------------------------------------------------
-- Keys
--------------------------------------------------------------------------------

prop_keys_get
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => MonoidMap k v
    -> Property
prop_keys_get m =
    fmap
        (\k -> (k, MonoidMap.get k m))
        (Set.toList (MonoidMap.keys m))
        === MonoidMap.toList m
    & cover 1
        (MonoidMap.null m)
        "MonoidMap.null m"
    & cover 1
        (not (MonoidMap.null m))
        "not (MonoidMap.null m)"

--------------------------------------------------------------------------------
-- Filtering
--------------------------------------------------------------------------------

prop_filter_toList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun (k, v) Bool
    -> MonoidMap k v
    -> Property
prop_filter_toList (applyFun2 -> f) m =
    toList n === List.filter (uncurry f) (toList m)
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m"
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m"
  where
    n = MonoidMap.filter f m

prop_filterKeys_filter
    :: (Ord k, Show k, Eq v, Show v)
    => Fun k Bool
    -> MonoidMap k v
    -> Property
prop_filterKeys_filter (applyFun -> f) m =
    n === MonoidMap.filter (\k _ -> f k) m
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m"
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m"
  where
    n = MonoidMap.filterKeys f m

prop_filterKeys_toList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun k Bool
    -> MonoidMap k v
    -> Property
prop_filterKeys_toList (applyFun -> f) m =
    toList n === List.filter (f . fst) (toList m)
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m"
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m"
  where
    n = MonoidMap.filterKeys f m

prop_filterValues_filter
    :: (Ord k, Show k, Eq v, Show v)
    => Fun v Bool
    -> MonoidMap k v
    -> Property
prop_filterValues_filter (applyFun -> f) m =
    n === MonoidMap.filter (\_ v -> f v) m
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m"
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m"
  where
    n = MonoidMap.filterValues f m

prop_filterValues_toList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun v Bool
    -> MonoidMap k v
    -> Property
prop_filterValues_toList (applyFun -> f) m =
    toList n === List.filter (f . snd) (toList m)
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n == MonoidMap.size m"
    & cover 1
        (MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m)
        "MonoidMap.notNull n && MonoidMap.size n /= MonoidMap.size m"
  where
    n = MonoidMap.filterValues f m

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

prop_partition_filter
    :: (Ord k, Show k, Eq v, Show v)
    => Fun (k, v) Bool
    -> MonoidMap k v
    -> Property
prop_partition_filter (applyFun2 -> f) m =
    MonoidMap.partition f m === (x, y)
    & cover 1
        (MonoidMap.size x /= 0 && MonoidMap.size y /= 0)
        "MonoidMap.size x /= 0 && MonoidMap.size y /= 0"
  where
    x = MonoidMap.filter f m
    y = MonoidMap.filter ((fmap . fmap) not f) m

prop_partitionKeys_filterKeys
    :: (Ord k, Show k, Eq v, Show v)
    => Fun k Bool
    -> MonoidMap k v
    -> Property
prop_partitionKeys_filterKeys (applyFun -> f) m =
    MonoidMap.partitionKeys f m === (x, y)
    & cover 1
        (MonoidMap.size x /= 0 && MonoidMap.size y /= 0)
        "MonoidMap.size x /= 0 && MonoidMap.size y /= 0"
  where
    x = MonoidMap.filterKeys f m
    y = MonoidMap.filterKeys (not . f) m

prop_partitionValues_filterValues
    :: (Ord k, Show k, Eq v, Show v)
    => Fun v Bool
    -> MonoidMap k v
    -> Property
prop_partitionValues_filterValues (applyFun -> f) m =
    MonoidMap.partitionValues f m === (x, y)
    & cover 1
        (MonoidMap.size x /= 0 && MonoidMap.size y /= 0)
        "MonoidMap.size x /= 0 && MonoidMap.size y /= 0"
  where
    x = MonoidMap.filterValues f m
    y = MonoidMap.filterValues (not . f) m

--------------------------------------------------------------------------------
-- Slicing
--------------------------------------------------------------------------------

data Slice k v = Slice Int (MonoidMap k v)
    deriving (Eq, Show)

instance (Arbitrary k, Arbitrary v, MonoidNull v, Ord k) =>
    Arbitrary (Slice k v)
  where
    arbitrary = do
        m <- genMap
        i <- genIndex m
        pure $ Slice i m
      where
        genMap :: Gen (MonoidMap k v)
        genMap = arbitrary

        genIndex :: MonoidMap k v -> Gen Int
        genIndex m = oneof
            [ choose (negate (length m), -1)
            , pure 0
            , choose (1, length m - 1)
            , pure (length m)
            , choose (length m + 1, 2 * length m)
            ]

prop_take_toList_fromList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Slice k v
    -> Property
prop_take_toList_fromList (Slice i m) =
    MonoidMap.take i m
        === (fromList . Prelude.take i . toList) m
    & cover 1
        (i == 0 && 0 < MonoidMap.size m)
        "i == 0 && 0 < MonoidMap.size m"
    & cover 1
        (0 < i && i < MonoidMap.size m)
        "0 < i && i < MonoidMap.size m"
    & cover 1
        (0 < MonoidMap.size m && MonoidMap.size m == i)
        "0 < MonoidMap.size m && MonoidMap.size m == i"
    & cover 1
        (0 < MonoidMap.size m && MonoidMap.size m < i)
        "0 < MonoidMap.size m && MonoidMap.size m < i"

prop_drop_toList_fromList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Slice k v
    -> Property
prop_drop_toList_fromList (Slice i m) =
    MonoidMap.drop i m
        === (fromList . Prelude.drop i . toList) m
    & cover 1
        (i == 0 && 0 < MonoidMap.size m)
        "i == 0 && 0 < MonoidMap.size m"
    & cover 1
        (0 < i && i < MonoidMap.size m)
        "0 < i && i < MonoidMap.size m"
    & cover 1
        (0 < MonoidMap.size m && MonoidMap.size m == i)
        "0 < MonoidMap.size m && MonoidMap.size m == i"
    & cover 1
        (0 < MonoidMap.size m && MonoidMap.size m < i)
        "0 < MonoidMap.size m && MonoidMap.size m < i"

prop_splitAt_toList_fromList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Slice k v
    -> Property
prop_splitAt_toList_fromList (Slice i m) =
    MonoidMap.splitAt i m
        === (bimap fromList fromList . Prelude.splitAt i . toList) m
    & cover 1
        (i == 0 && 0 < MonoidMap.size m)
        "i == 0 && 0 < MonoidMap.size m"
    & cover 1
        (0 < i && i < MonoidMap.size m)
        "0 < i && i < MonoidMap.size m"
    & cover 1
        (0 < MonoidMap.size m && MonoidMap.size m == i)
        "0 < MonoidMap.size m && MonoidMap.size m == i"
    & cover 1
        (0 < MonoidMap.size m && MonoidMap.size m < i)
        "0 < MonoidMap.size m && MonoidMap.size m < i"

--------------------------------------------------------------------------------
-- Mapping
--------------------------------------------------------------------------------

prop_map_asList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun k k
    -> Fun v v
    -> MonoidMap k v
    -> Property
prop_map_asList (applyFun -> f) (applyFun -> g) m =
    n === (MonoidMap.fromList . fmap (bimap f g) . MonoidMap.toList $ m)
    & cover 1
        (0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m)
        "0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m"
  where
    n = MonoidMap.map f g m

prop_mapWith_asList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun (v, v) v
    -> Fun k k
    -> Fun v v
    -> MonoidMap k v
    -> Property
prop_mapWith_asList (applyFun2 -> c) (applyFun -> f) (applyFun -> g) m =
    n === (MonoidMap.fromListWith c . fmap (bimap f g) . MonoidMap.toList $ m)
    & cover 1
        (0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m)
        "0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m"
  where
    n = MonoidMap.mapWith c f g m

prop_mapKeys_asList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun k k
    -> MonoidMap k v
    -> Property
prop_mapKeys_asList (applyFun -> f) m =
    n === (MonoidMap.fromList . fmap (first f) . MonoidMap.toList $ m)
    & cover 1
        (0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m)
        "0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m"
  where
    n = MonoidMap.mapKeys f m

prop_mapKeysWith_asList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun (v, v) v
    -> Fun k k
    -> MonoidMap k v
    -> Property
prop_mapKeysWith_asList (applyFun2 -> c) (applyFun -> f) m =
    n === (MonoidMap.fromListWith c . fmap (first f) . MonoidMap.toList $ m)
    & cover 1
        (0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m)
        "0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m"
  where
    n = MonoidMap.mapKeysWith c f m

prop_mapValues_asList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun v v
    -> MonoidMap k v
    -> Property
prop_mapValues_asList (applyFun -> f) m =
    n === (MonoidMap.fromList . fmap (second f) . MonoidMap.toList $ m)
    & cover 1
        (0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m)
        "0 < MonoidMap.size n && MonoidMap.size n < MonoidMap.size m"
  where
    n = MonoidMap.mapValues f m

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
-- Unit tests: Group
--------------------------------------------------------------------------------

unitTestSpec_Group_invert_Product_Rational :: Spec
unitTestSpec_Group_invert_Product_Rational = unitTestSpec
    "Group.invert (Product Rational)"
    "invert"
    (invert)
    (unitTestData_Group_invert_Product_Rational)

unitTestData_Group_invert_Product_Rational :: UnitTestData1
    (MonoidMap LatinChar (Product Rational))
    (MonoidMap LatinChar (Product Rational))
unitTestData_Group_invert_Product_Rational = unitTestData1
    [ ( m [  2,   4,   8,   16]
      , m [1%2, 1%4, 1%8, 1%16]
      )
    , ( m [1%2, 1%4, 1%8, 1%16]
      , m [  2,   4,   8,   16]
      )
    , ( m [  2, 1%4,   8,   16]
      , m [1%2,   4, 1%8, 1%16]
      )
    , ( m [1%2,   4, 1%8, 1%16]
      , m [  2, 1%4,   8,   16]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Product

unitTestSpec_Group_invert_Sum_Integer :: Spec
unitTestSpec_Group_invert_Sum_Integer = unitTestSpec
    "Group.invert (Sum Integer)"
    "invert"
    (invert)
    (unitTestData_Group_invert_Sum_Integer)

unitTestData_Group_invert_Sum_Integer :: UnitTestData1
    (MonoidMap LatinChar (Sum Integer))
    (MonoidMap LatinChar (Sum Integer))
unitTestData_Group_invert_Sum_Integer = unitTestData1
    [ ( m [ 1,  2,  3,  4]
      , m [-1, -2, -3, -4]
      )
    , ( m [-1, -2, -3, -4]
      , m [ 1,  2,  3,  4]
      )
    , ( m [ 1, -2,  3, -4]
      , m [-1,  2, -3,  4]
      )
    , ( m [-1,  2, -3,  4]
      , m [ 1, -2,  3, -4]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Sum

unitTestSpec_Group_pow_Product_Rational :: Spec
unitTestSpec_Group_pow_Product_Rational = unitTestSpec
    "Group.pow (Product Rational)"
    "pow"
    (pow)
    (unitTestData_Group_pow_Product_Rational)

unitTestData_Group_pow_Product_Rational :: UnitTestData2
    (MonoidMap LatinChar (Product Rational))
    (Integer)
    (MonoidMap LatinChar (Product Rational))
unitTestData_Group_pow_Product_Rational = unitTestData2
    [ ( m [  2,   -4,   8,   -16], (-1)
      , m [1%2, -1%4, 1%8, -1%16]
      )
    , ( m [  2,   -4,   8,   -16], 0
      , m [  1,    1,   1,     1]
      )
    , ( m [  2,   -4,   8,   -16], 1
      , m [  2,   -4,   8,   -16]
      )
    , ( m [  2,   -4,   8,   -16], 2
      , m [  4,   16,  64,   256]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Product

unitTestSpec_Group_pow_Sum_Integer :: Spec
unitTestSpec_Group_pow_Sum_Integer = unitTestSpec
    "Group.pow (Sum Integer)"
    "pow"
    (pow)
    (unitTestData_Group_pow_Sum_Integer)

unitTestData_Group_pow_Sum_Integer :: UnitTestData2
    (MonoidMap LatinChar (Sum Integer))
    (Integer)
    (MonoidMap LatinChar (Sum Integer))
unitTestData_Group_pow_Sum_Integer = unitTestData2
    [ ( m [ 1, -2,  3, -4], (-1)
      , m [-1,  2, -3,  4]
      )
    , ( m [ 1, -2,  3, -4], 0
      , m [ 0,  0,  0,  0]
      )
    , ( m [ 1, -2,  3, -4], 1
      , m [ 1, -2,  3, -4]
      )
    , ( m [ 1, -2,  3, -4], 2
      , m [ 2, -4,  6, -8]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Sum

unitTestSpec_Group_subtract_Product_Rational :: Spec
unitTestSpec_Group_subtract_Product_Rational = unitTestSpec
    "Group.(~~) (Product Rational)"
    "(~~)"
    (~~)
    (unitTestData_Group_subtract_Product_Rational)

unitTestData_Group_subtract_Product_Rational :: UnitTestData2
    (MonoidMap LatinChar (Product Rational))
    (MonoidMap LatinChar (Product Rational))
    (MonoidMap LatinChar (Product Rational))
unitTestData_Group_subtract_Product_Rational = unitTestData2
    [ ( m [ 1,    1,    1,    1]
      , m [ 1,    2,    4,    8]
      , m [ 1,  1%2,  1%4,  1%8]
      )
    , ( m [-1,   -1,   -1,   -1]
      , m [ 1,    2,    4,    8]
      , m [-1, -1%2, -1%4, -1%8]
      )
    , ( m [ 1,    1,    1,    1]
      , m [-1,   -2,   -4,   -8]
      , m [-1, -1%2, -1%4, -1%8]
      )
    , ( m [-1,   -1,   -1,   -1]
      , m [-1,   -2,   -4,   -8]
      , m [ 1,  1%2,  1%4,  1%8]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Product

unitTestSpec_Group_subtract_Sum_Integer :: Spec
unitTestSpec_Group_subtract_Sum_Integer = unitTestSpec
    "Group.(~~) (Sum Integer)"
    "(~~)"
    (~~)
    (unitTestData_Group_subtract_Sum_Integer)

unitTestData_Group_subtract_Sum_Integer :: UnitTestData2
    (MonoidMap LatinChar (Sum Integer))
    (MonoidMap LatinChar (Sum Integer))
    (MonoidMap LatinChar (Sum Integer))
unitTestData_Group_subtract_Sum_Integer = unitTestData2
    [ ( m [ 1,  2,  3,  4]
      , m [ 1,  2,  3,  4]
      , m [ 0,  0,  0,  0]
      )
    , ( m [ 0,  0,  0,  0]
      , m [ 1,  2,  3,  4]
      , m [-1, -2, -3, -4]
      )
    , ( m [ 1,  2,  3,  4]
      , m [-1, -2, -3, -4]
      , m [ 2,  4,  6,  8]
      )
    , ( m [-1, -2, -3, -4]
      , m [-1, -2, -3, -4]
      , m [ 0,  0,  0,  0]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Sum

--------------------------------------------------------------------------------
-- Unit tests: Reductive
--------------------------------------------------------------------------------

unitTestSpec_Reductive_isPrefixOf_String :: Spec
unitTestSpec_Reductive_isPrefixOf_String = unitTestSpec
    "Reductive.isPrefixOf (String)"
    "isPrefixOf"
    (isPrefixOf)
    (unitTestData_Reductive_isPrefixOf_String)

unitTestData_Reductive_isPrefixOf_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (Bool)
unitTestData_Reductive_isPrefixOf_String = unitTestData2
    [ ( m ["A"   , "B"   , "C"   ]
      , m ["A123", "B123", "C123"]
      , True
      )
    , ( m ["A123", "B123", "C123"]
      , m ["A"   , "B"   , "C"   ]
      , False
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

unitTestSpec_Reductive_isSuffixOf_String :: Spec
unitTestSpec_Reductive_isSuffixOf_String = unitTestSpec
    "Reductive.isSuffixOf (String)"
    "isSuffixOf"
    (isSuffixOf)
    (unitTestData_Reductive_isSuffixOf_String)

unitTestData_Reductive_isSuffixOf_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (Bool)
unitTestData_Reductive_isSuffixOf_String = unitTestData2
    [ ( m [   "A",    "B",    "C"]
      , m ["123A", "123B", "123C"]
      , True
      )
    , ( m ["123A", "123B", "123C"]
      , m [   "A",    "B",    "C"]
      , False
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

unitTestSpec_Reductive_isPrefixOf_Sum_Natural :: Spec
unitTestSpec_Reductive_isPrefixOf_Sum_Natural = unitTestSpec
    "Reductive.isPrefixOf (Sum Natural)"
    "isPrefixOf"
    (isPrefixOf)
    (unitTestData_Reductive_Sum_Natural)

unitTestSpec_Reductive_isSuffixOf_Sum_Natural :: Spec
unitTestSpec_Reductive_isSuffixOf_Sum_Natural = unitTestSpec
    "Reductive.isSuffixOf (Sum Natural)"
    "isSuffixOf"
    (isSuffixOf)
    (unitTestData_Reductive_Sum_Natural)

unitTestData_Reductive_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (Bool)
unitTestData_Reductive_Sum_Natural = unitTestData2
    [ ( m [1, 1], m [1, 1], True )
    , ( m [1, 1], m [1, 2], True )
    , ( m [1, 1], m [2, 1], True )
    , ( m [1, 1], m [2, 2], True )
    , ( m [1, 2], m [1, 1], False)
    , ( m [1, 2], m [1, 2], True )
    , ( m [1, 2], m [2, 1], False)
    , ( m [1, 2], m [2, 2], True )
    , ( m [2, 1], m [1, 1], False)
    , ( m [2, 1], m [1, 2], False)
    , ( m [2, 1], m [2, 1], True )
    , ( m [2, 1], m [2, 2], True )
    , ( m [2, 2], m [1, 1], False)
    , ( m [2, 2], m [1, 2], False)
    , ( m [2, 2], m [2, 1], False)
    , ( m [2, 2], m [2, 2], True )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Sum

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

(➤) :: a -> b -> (a, b)
a ➤ b = (a, b)

data LatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Show)
