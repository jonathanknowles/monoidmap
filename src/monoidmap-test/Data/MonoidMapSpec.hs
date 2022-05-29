{-# LANGUAGE OverloadedLists #-}
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
    , groupLaws
    , leftCancellativeLaws
    , leftGCDMonoidLaws
    , leftReductiveLaws
    , monoidNullLaws
    , monusLaws
    , overlappingGCDMonoidLaws
    , positiveMonoidLaws
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

    parallel $ describe "Nullification" $ do
        it "prop_nullify_nonNullKeys" $
            prop_nullify_nonNullKeys & property
        it "prop_nullify_get" $
            prop_nullify_get & property
        it "prop_nullify_nonNullKey" $
            prop_nullify_nonNullKey & property

    parallel $ describe "Set" $ do
        it "prop_set_nonNullKeys" $
            prop_set_nonNullKeys & property
        it "prop_set_get" $
            prop_set_get & property
        it "prop_set_NonNullKey" $
            prop_set_NonNullKey & property
        it "prop_set_toList" $
            prop_set_toList & property

    parallel $ describe "Keys" $ do
        it "prop_nonNullKeys_get" $
            prop_nonNullKeys_get & property

    parallel $ describe "Get" $ do
        it "prop_get_nonNullKeys" $
            prop_get_nonNullKeys & property
        it "prop_get_nonNullKey" $
            prop_get_nonNullKey & property

    parallel $ describe "Singleton" $ do
        it "prop_singleton_nullify" $
            prop_singleton_nullify & property
        it "prop_singleton_nonNullKeys" $
            prop_singleton_nonNullKeys & property
        it "prop_singleton_get" $
            prop_singleton_get & property
        it "prop_singleton_nonNullKey" $
            prop_singleton_nonNullKey & property
        it "prop_singleton_null" $
            prop_singleton_null & property
        it "prop_singleton_size" $
            prop_singleton_size & property
        it "prop_singleton_toList" $
            prop_singleton_toList & property

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
-- Deletion
--------------------------------------------------------------------------------

prop_nullify_nonNullKeys :: MonoidMap Key Value -> Key -> Property
prop_nullify_nonNullKeys m k =
    Set.member k (MonoidMap.nonNullKeys (MonoidMap.nullify m k)) === False
    & cover 10
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & checkCoverage

prop_nullify_get :: MonoidMap Key Value -> Key -> Property
prop_nullify_get m k =
    MonoidMap.nullify m k `MonoidMap.get` k === mempty
    & cover 10
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & checkCoverage

prop_nullify_nonNullKey :: MonoidMap Key Value -> Key -> Property
prop_nullify_nonNullKey m k =
    MonoidMap.nonNullKey k (MonoidMap.nullify m k) === False
    & cover 10
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & checkCoverage

--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

prop_set_nonNullKeys :: MonoidMap Key Value -> Key -> Value -> Property
prop_set_nonNullKeys m k v =
    Set.member k (MonoidMap.nonNullKeys (MonoidMap.set m k v)) ===
        (v /= mempty)

prop_set_get :: MonoidMap Key Value -> Key -> Value -> Property
prop_set_get m k v =
    MonoidMap.set m k v `MonoidMap.get` k === v
    & cover 10
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 10
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"
    & checkCoverage

prop_set_NonNullKey :: MonoidMap Key Value -> Key -> Value -> Property
prop_set_NonNullKey m k v =
    MonoidMap.nonNullKey k (MonoidMap.set m k v) ===
        (v /= mempty)

prop_set_toList :: MonoidMap Key Value -> Key -> Value -> Property
prop_set_toList m k v =
    filter ((== k) . fst) (MonoidMap.toList (MonoidMap.set m k v)) ===
        [(k, v) | v /= mempty]

--------------------------------------------------------------------------------
-- Keys
--------------------------------------------------------------------------------

prop_nonNullKeys_get :: MonoidMap Key Value -> Property
prop_nonNullKeys_get m =
    fmap
        (\k -> (k, m `MonoidMap.get` k))
        (Set.toList (MonoidMap.nonNullKeys m))
    === MonoidMap.toList m

--------------------------------------------------------------------------------
-- Get
--------------------------------------------------------------------------------

prop_get_nonNullKeys :: MonoidMap Key Value -> Key -> Property
prop_get_nonNullKeys m k =
    Set.member k (MonoidMap.nonNullKeys m) === (m `MonoidMap.get` k /= mempty)
    & cover 10
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 10
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"
    & checkCoverage

prop_get_nonNullKey :: MonoidMap Key Value -> Key -> Property
prop_get_nonNullKey m k =
    MonoidMap.nonNullKey k m === (m `MonoidMap.get` k /= mempty)
    & cover 10
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 10
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"
    & checkCoverage

--------------------------------------------------------------------------------
-- Singleton
--------------------------------------------------------------------------------

prop_singleton_nullify :: Key -> Value -> Property
prop_singleton_nullify k v =
    MonoidMap.singleton k v `MonoidMap.nullify` k === mempty

prop_singleton_nonNullKeys :: Key -> Value -> Property
prop_singleton_nonNullKeys k v =
    MonoidMap.nonNullKeys (MonoidMap.singleton k v) ===
        if v == mempty
        then Set.empty
        else Set.singleton k

prop_singleton_get :: Key -> Value -> Property
prop_singleton_get k v =
    MonoidMap.singleton k v `MonoidMap.get` k === v

prop_singleton_nonNullKey :: Key -> Value -> Property
prop_singleton_nonNullKey k v =
    MonoidMap.nonNullKey k (MonoidMap.singleton k v) === (v /= mempty)

prop_singleton_null :: Key -> Value -> Property
prop_singleton_null k v =
    MonoidMap.null (MonoidMap.singleton k v) === (v == mempty)

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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

(➤) :: a -> b -> (a, b)
a ➤ b = (a, b)

data LatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Show)
