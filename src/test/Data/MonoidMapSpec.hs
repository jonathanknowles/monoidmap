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
import Data.Ratio
    ( (%) )
import Data.Semigroup.Cancellative
    ( LeftReductive (..), RightReductive (..) )
import Data.Set
    ( Set )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.Hspec.Unit
    ( UnitTestData1
    , UnitTestData2
    , unitTestData1
    , unitTestData2
    , unitTestSpec
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Fun (..)
    , Gen
    , Property
    , applyFun
    , applyFun2
    , checkCoverage
    , choose
    , cover
    , listOf
    , oneof
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

import qualified Data.List as List
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

    parallel $ describe "Conversion to and from lists" $ do
        it "prop_fromList_toMap" $
            prop_fromList_toMap & property
        it "prop_fromList_toList" $
            prop_fromList_toList & property
        it "prop_toList_fromList" $
            prop_toList_fromList & property

    parallel $ describe "Conversion to and from ordinary maps" $ do
        it "prop_fromMap_toMap" $
            prop_fromMap_toMap & property
        it "prop_toMap_fromMap" $
            prop_toMap_fromMap & property

    parallel $ describe "Singleton" $ do
        it "prop_singleton_get" $
            prop_singleton_get & property
        it "prop_singleton_member" $
            prop_singleton_member & property
        it "prop_singleton_keys" $
            prop_singleton_keys & property
        it "prop_singleton_null" $
            prop_singleton_null & property
        it "prop_singleton_delete" $
            prop_singleton_delete & property
        it "prop_singleton_size" $
            prop_singleton_size & property
        it "prop_singleton_toList" $
            prop_singleton_toList & property

    parallel $ describe "Get" $ do
        it "prop_get_member" $
            prop_get_member & property
        it "prop_get_keys" $
            prop_get_keys & property

    parallel $ describe "Set" $ do
        it "prop_set_get" $
            prop_set_get & property
        it "prop_set_member" $
            prop_set_member & property
        it "prop_set_keys" $
            prop_set_keys & property
        it "prop_set_toList" $
            prop_set_toList & property

    parallel $ describe "Nullify" $ do
        it "prop_delete_get" $
            prop_delete_get & property
        it "prop_delete_member" $
            prop_delete_member & property
        it "prop_delete_keys" $
            prop_delete_keys & property

    parallel $ describe "Keys" $ do
        it "prop_keys_get" $
            prop_keys_get & property

    parallel $ describe "Filtering" $ do
        it "prop_filter_toList" $
            prop_filter_toList & property
        it "prop_filterKeys_filter" $
            prop_filterKeys_filter & property
        it "prop_filterKeys_toList" $
            prop_filterKeys_toList & property
        it "prop_filterValues_filter" $
            prop_filterValues_filter & property
        it "prop_filterValues_toList" $
            prop_filterValues_toList & property

    parallel $ describe "Partitioning" $ do
        it "prop_partition_filter" $
            prop_partition_filter & property
        it "prop_partitionKeys_filterKeys" $
            prop_partitionKeys_filterKeys & property
        it "prop_partitionValues_filterValues" $
            prop_partitionValues_filterValues & property

    parallel $ describe "Slicing" $ do
        it "prop_take_toList_fromList" $
            prop_take_toList_fromList & property
        it "prop_drop_toList_fromList" $
            prop_drop_toList_fromList & property
        it "prop_splitAt_toList_fromList" $
            prop_splitAt_toList_fromList & property

    parallel $ describe "Mapping" $ do
        it "prop_map_asList" $
            prop_map_asList & property
        it "prop_mapWith_asList" $
            prop_mapWith_asList & property
        it "prop_mapKeys_asList" $
            prop_mapKeys_asList & property
        it "prop_mapKeysWith_asList" $
            prop_mapKeysWith_asList & property
        it "prop_mapValues_asList" $
            prop_mapValues_asList & property

    parallel $ describe "Unit tests" $ do

        describe "Group" $ do

            unitTestSpec_Group_invert_Product_Rational
            unitTestSpec_Group_invert_Sum_Integer
            unitTestSpec_Group_pow_Product_Rational
            unitTestSpec_Group_pow_Sum_Integer
            unitTestSpec_Group_subtract_Product_Rational
            unitTestSpec_Group_subtract_Sum_Integer

        describe "Reductive" $ do

            unitTestSpec_Reductive_isPrefixOf_String
            unitTestSpec_Reductive_isSuffixOf_String
            unitTestSpec_Reductive_isPrefixOf_Sum_Natural
            unitTestSpec_Reductive_isSuffixOf_Sum_Natural

--------------------------------------------------------------------------------
-- Test types
--------------------------------------------------------------------------------

type Key = Int
type Value = Sum Int

--------------------------------------------------------------------------------
-- Conversion to and from lists
--------------------------------------------------------------------------------

prop_fromList_toMap :: [(Key, Value)] -> Property
prop_fromList_toMap kvs =
    MonoidMap.toMap (MonoidMap.fromList kvs) ===
    Map.filter (/= mempty) (Map.fromListWith (<>) kvs)

prop_fromList_toList :: [(Key, Value)] -> Property
prop_fromList_toList kvs =
    MonoidMap.toList (MonoidMap.fromList kvs) ===
    Map.toList (Map.filter (/= mempty) (Map.fromListWith (<>) kvs))

prop_toList_fromList :: MonoidMap Key Value -> Property
prop_toList_fromList m =
    MonoidMap.fromList (MonoidMap.toList m) === m

--------------------------------------------------------------------------------
-- Conversion to and from ordinary maps
--------------------------------------------------------------------------------

prop_fromMap_toMap :: Map Key Value -> Property
prop_fromMap_toMap m =
    MonoidMap.toMap (MonoidMap.fromMap m) === Map.filter (/= mempty) m

prop_toMap_fromMap :: MonoidMap Key Value -> Property
prop_toMap_fromMap m =
    MonoidMap.fromMap (MonoidMap.toMap m) === m

--------------------------------------------------------------------------------
-- Singleton
--------------------------------------------------------------------------------

prop_singleton_get :: Key -> Value -> Property
prop_singleton_get k v =
    MonoidMap.get k (MonoidMap.singleton k v) === v

prop_singleton_member :: Key -> Value -> Property
prop_singleton_member k v =
    MonoidMap.member k (MonoidMap.singleton k v) === (v /= mempty)

prop_singleton_keys :: Key -> Value -> Property
prop_singleton_keys k v =
    MonoidMap.keys (MonoidMap.singleton k v) ===
        if v == mempty
        then Set.empty
        else Set.singleton k

prop_singleton_null :: Key -> Value -> Property
prop_singleton_null k v =
    MonoidMap.null (MonoidMap.singleton k v) === (v == mempty)

prop_singleton_delete :: Key -> Value -> Property
prop_singleton_delete k v =
    MonoidMap.delete k (MonoidMap.singleton k v) === mempty

prop_singleton_size :: Key -> Value -> Property
prop_singleton_size k v =
    MonoidMap.size (MonoidMap.singleton k v) ===
        if v == mempty
        then 0
        else 1

prop_singleton_toList :: Key -> Value -> Property
prop_singleton_toList k v =
    MonoidMap.toList (MonoidMap.singleton k v) ===
        [(k, v) | v /= mempty]

--------------------------------------------------------------------------------
-- Get
--------------------------------------------------------------------------------

prop_get_member :: MonoidMap Key Value -> Key -> Property
prop_get_member m k =
    MonoidMap.member k m === (MonoidMap.get k m /= mempty)
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 10
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"
    & checkCoverage

prop_get_keys :: MonoidMap Key Value -> Key -> Property
prop_get_keys m k =
    Set.member k (MonoidMap.keys m) === (MonoidMap.get k m /= mempty)
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 10
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"
    & checkCoverage

--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

prop_set_get :: MonoidMap Key Value -> Key -> Value -> Property
prop_set_get m k v =
    MonoidMap.get k (MonoidMap.set k v m) === v
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 10
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"
    & checkCoverage

prop_set_member :: MonoidMap Key Value -> Key -> Value -> Property
prop_set_member m k v =
    MonoidMap.member k (MonoidMap.set k v m) ===
        (v /= mempty)

prop_set_keys :: MonoidMap Key Value -> Key -> Value -> Property
prop_set_keys m k v =
    Set.member k (MonoidMap.keys (MonoidMap.set k v m)) ===
        (v /= mempty)

prop_set_toList :: MonoidMap Key Value -> Key -> Value -> Property
prop_set_toList m k v =
    filter ((== k) . fst) (MonoidMap.toList (MonoidMap.set k v m)) ===
        [(k, v) | v /= mempty]

--------------------------------------------------------------------------------
-- Nullify
--------------------------------------------------------------------------------

prop_delete_get :: MonoidMap Key Value -> Key -> Property
prop_delete_get m k =
    MonoidMap.get k (MonoidMap.delete k m) === mempty
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & checkCoverage

prop_delete_member :: MonoidMap Key Value -> Key -> Property
prop_delete_member m k =
    MonoidMap.member k (MonoidMap.delete k m) === False
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & checkCoverage

prop_delete_keys :: MonoidMap Key Value -> Key -> Property
prop_delete_keys m k =
    Set.member k (MonoidMap.keys (MonoidMap.delete k m)) === False
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & checkCoverage

--------------------------------------------------------------------------------
-- Keys
--------------------------------------------------------------------------------

prop_keys_get :: MonoidMap Key Value -> Property
prop_keys_get m =
    fmap
        (\k -> (k, MonoidMap.get k m))
        (Set.toList (MonoidMap.keys m))
    === MonoidMap.toList m

--------------------------------------------------------------------------------
-- Filtering
--------------------------------------------------------------------------------

prop_filter_toList
    :: Fun (Key, Value) Bool -> MonoidMap Key Value -> Property
prop_filter_toList (applyFun2 -> f) m =
    toList (MonoidMap.filter f m) === List.filter (uncurry f) (toList m)

prop_filterKeys_filter
    :: Fun Key Bool -> MonoidMap Key Value -> Property
prop_filterKeys_filter (applyFun -> f) m =
    MonoidMap.filterKeys f m === MonoidMap.filter (\k _ -> f k) m

prop_filterKeys_toList
    :: Fun Key Bool -> MonoidMap Key Value -> Property
prop_filterKeys_toList (applyFun -> f) m =
    toList (MonoidMap.filterKeys f m) === List.filter (f . fst) (toList m)

prop_filterValues_filter
    :: Fun Value Bool -> MonoidMap Key Value -> Property
prop_filterValues_filter (applyFun -> f) m =
    MonoidMap.filterValues f m === MonoidMap.filter (\_ v -> f v) m

prop_filterValues_toList
    :: Fun Value Bool -> MonoidMap Key Value -> Property
prop_filterValues_toList (applyFun -> f) m =
    toList (MonoidMap.filterValues f m) === List.filter (f . snd) (toList m)

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

prop_partition_filter
    :: Fun (Key, Value) Bool -> MonoidMap Key Value -> Property
prop_partition_filter (applyFun2 -> f) m =
    MonoidMap.partition f m ===
        ( MonoidMap.filter f m
        , MonoidMap.filter ((fmap . fmap) not f) m
        )

prop_partitionKeys_filterKeys
    :: Fun Key Bool -> MonoidMap Key Value -> Property
prop_partitionKeys_filterKeys (applyFun -> f) m =
    MonoidMap.partitionKeys f m ===
        ( MonoidMap.filterKeys f m
        , MonoidMap.filterKeys (not . f) m
        )

prop_partitionValues_filterValues
    :: Fun Value Bool -> MonoidMap Key Value -> Property
prop_partitionValues_filterValues (applyFun -> f) m =
    MonoidMap.partitionValues f m ===
        ( MonoidMap.filterValues f m
        , MonoidMap.filterValues (not . f) m
        )

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
    :: Slice Key Value -> Property
prop_take_toList_fromList (Slice i m) =
    MonoidMap.take i m
        === (fromList . Prelude.take i . toList) m

prop_drop_toList_fromList
    :: Slice Key Value -> Property
prop_drop_toList_fromList (Slice i m) =
    MonoidMap.drop i m
        === (fromList . Prelude.drop i . toList) m

prop_splitAt_toList_fromList
    :: Slice Key Value -> Property
prop_splitAt_toList_fromList (Slice i m) =
    MonoidMap.splitAt i m
        === (bimap fromList fromList . Prelude.splitAt i . toList) m

--------------------------------------------------------------------------------
-- Mapping
--------------------------------------------------------------------------------

prop_map_asList
    :: Fun Key Key
    -> Fun Value Value
    -> MonoidMap Key Value
    -> Property
prop_map_asList (applyFun -> f) (applyFun -> g) m =
    MonoidMap.map f g m
    ===
    (MonoidMap.fromList . fmap (bimap f g) . MonoidMap.toList $ m)

prop_mapWith_asList
    :: Fun (Value, Value) Value
    -> Fun Key Key
    -> Fun Value Value
    -> MonoidMap Key Value
    -> Property
prop_mapWith_asList (applyFun2 -> c) (applyFun -> f) (applyFun -> g) m =
    MonoidMap.mapWith c f g m
    ===
    (MonoidMap.fromListWith c . fmap (bimap f g) . MonoidMap.toList $ m)

prop_mapKeys_asList
    :: Fun Key Key
    -> MonoidMap Key Value
    -> Property
prop_mapKeys_asList (applyFun -> f) m =
    MonoidMap.mapKeys f m
    ===
    (MonoidMap.fromList . fmap (first f) . MonoidMap.toList $ m)

prop_mapKeysWith_asList
    :: Fun (Value, Value) Value
    -> Fun Key Key
    -> MonoidMap Key Value
    -> Property
prop_mapKeysWith_asList (applyFun2 -> c) (applyFun -> f) m =
    MonoidMap.mapKeysWith c f m
    ===
    (MonoidMap.fromListWith c . fmap (first f) . MonoidMap.toList $ m)

prop_mapValues_asList
    :: Fun Value Value
    -> MonoidMap Key Value
    -> Property
prop_mapValues_asList (applyFun -> f) m =
    MonoidMap.mapValues f m
    ===
    (MonoidMap.fromList . fmap (second f) . MonoidMap.toList $ m)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance (Arbitrary k, Ord k, Arbitrary v, MonoidNull v) =>
    Arbitrary (MonoidMap k v)
  where
    arbitrary = fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)
    shrink = shrinkMapBy MonoidMap.fromMap MonoidMap.toMap shrink

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
