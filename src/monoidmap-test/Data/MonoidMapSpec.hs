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

    parallel $ describe "Deletion" $ do
        it "prop_delete_keysSet" $
            prop_delete_keysSet & property
        it "prop_delete_lookup" $
            prop_delete_lookup & property
        it "prop_delete_member" $
            prop_delete_member & property

    parallel $ describe "Insertion" $ do
        it "prop_insert_keysSet" $
            prop_insert_keysSet & property
        it "prop_insert_lookup" $
            prop_insert_lookup & property
        it "prop_insert_member" $
            prop_insert_member & property
        it "prop_insert_toList" $
            prop_insert_toList & property

    parallel $ describe "Keys" $ do
        it "prop_keysSet_lookup" $
            prop_keysSet_lookup & property

    parallel $ describe "Lookup" $ do
        it "prop_lookup_keysSet" $
            prop_lookup_keysSet & property
        it "prop_lookup_member" $
            prop_lookup_member & property

    parallel $ describe "Singleton" $ do
        it "prop_singleton_delete" $
            prop_singleton_delete & property
        it "prop_singleton_keysSet" $
            prop_singleton_keysSet & property
        it "prop_singleton_lookup" $
            prop_singleton_lookup & property
        it "prop_singleton_member" $
            prop_singleton_member & property
        it "prop_singleton_null" $
            prop_singleton_null & property
        it "prop_singleton_size" $
            prop_singleton_size & property
        it "prop_singleton_toList" $
            prop_singleton_toList & property

    parallel $ describe "Unit tests" $ do

        unitTestSpec_invert_Product_Rational
        unitTestSpec_invert_Sum_Integer
        unitTestSpec_pow_Product_Rational
        unitTestSpec_pow_Sum_Integer
        unitTestSpec_isPrefixOf_String
        unitTestSpec_isSuffixOf_String

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

prop_delete_keysSet :: MonoidMap Key Value -> Key -> Property
prop_delete_keysSet m k =
    Set.member k (MonoidMap.keysSet (MonoidMap.delete k m)) === False
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & checkCoverage

prop_delete_lookup :: MonoidMap Key Value -> Key -> Property
prop_delete_lookup m k =
    MonoidMap.lookup k (MonoidMap.delete k m) === mempty
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

--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

prop_insert_keysSet :: MonoidMap Key Value -> Key -> Value -> Property
prop_insert_keysSet m k v =
    Set.member k (MonoidMap.keysSet (MonoidMap.insert k v m)) ===
        (v /= mempty)

prop_insert_lookup :: MonoidMap Key Value -> Key -> Value -> Property
prop_insert_lookup m k v =
    MonoidMap.lookup k (MonoidMap.insert k v m) === v
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 10
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"
    & checkCoverage

prop_insert_member :: MonoidMap Key Value -> Key -> Value -> Property
prop_insert_member m k v =
    MonoidMap.member k (MonoidMap.insert k v m) ===
        (v /= mempty)

prop_insert_toList :: MonoidMap Key Value -> Key -> Value -> Property
prop_insert_toList m k v =
    filter ((== k) . fst) (MonoidMap.toList (MonoidMap.insert k v m)) ===
        if v == mempty
        then []
        else [(k, v)]

--------------------------------------------------------------------------------
-- Keys
--------------------------------------------------------------------------------

prop_keysSet_lookup :: MonoidMap Key Value -> Property
prop_keysSet_lookup m =
    fmap
        (\k -> (k, MonoidMap.lookup k m))
        (Set.toList (MonoidMap.keysSet m))
    === MonoidMap.toList m

--------------------------------------------------------------------------------
-- Lookup
--------------------------------------------------------------------------------

prop_lookup_keysSet :: MonoidMap Key Value -> Key -> Property
prop_lookup_keysSet m k =
    Set.member k (MonoidMap.keysSet m) === (MonoidMap.lookup k m /= mempty)
    & cover 10
        (MonoidMap.member k m)
        "MonoidMap.member k m"
    & cover 10
        (not (MonoidMap.member k m))
        "not (MonoidMap.member k m)"
    & checkCoverage

prop_lookup_member :: MonoidMap Key Value -> Key -> Property
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
-- Singleton
--------------------------------------------------------------------------------

prop_singleton_delete :: Key -> Value -> Property
prop_singleton_delete k v =
    MonoidMap.delete k (MonoidMap.singleton k v) === mempty

prop_singleton_keysSet :: Key -> Value -> Property
prop_singleton_keysSet k v =
    MonoidMap.keysSet (MonoidMap.singleton k v) ===
        if v == mempty
        then Set.empty
        else Set.singleton k

prop_singleton_lookup :: Key -> Value -> Property
prop_singleton_lookup k v =
    MonoidMap.lookup k (MonoidMap.singleton k v) === v

prop_singleton_member :: Key -> Value -> Property
prop_singleton_member k v =
    MonoidMap.member k (MonoidMap.singleton k v) === (v /= mempty)

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
        if v == mempty
        then []
        else [(k, v)]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance (Arbitrary k, Ord k, Arbitrary v, MonoidNull v) =>
    Arbitrary (MonoidMap k v)
  where
    arbitrary = fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)
    shrink = shrinkMapBy MonoidMap.fromMap MonoidMap.toMap shrink

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTestSpec_invert_Product_Rational :: Spec
unitTestSpec_invert_Product_Rational = unitTestSpec
    "invert Product Rational"
    "invert"
    (invert)
    (unitTestData_invert_Product_Rational)

unitTestData_invert_Product_Rational :: UnitTestData1
    (MonoidMap LatinChar (Product Rational))
    (MonoidMap LatinChar (Product Rational))
unitTestData_invert_Product_Rational = unitTestData1 $
    [ ( [A ➤ p (  2), B ➤ p (  4), C ➤ p (  8), D ➤ p (  16)]
      , [A ➤ p (1%2), B ➤ p (1%4), C ➤ p (1%8), D ➤ p (1%16)]
      )
    , ( [A ➤ p (1%2), B ➤ p (1%4), C ➤ p (1%8), D ➤ p (1%16)]
      , [A ➤ p (  2), B ➤ p (  4), C ➤ p (  8), D ➤ p (  16)]
      )
    , ( [A ➤ p (  2), B ➤ p (1%4), C ➤ p (  8), D ➤ p (  16)]
      , [A ➤ p (1%2), B ➤ p (  4), C ➤ p (1%8), D ➤ p (1%16)]
      )
    , ( [A ➤ p (1%2), B ➤ p (  4), C ➤ p (1%8), D ➤ p (1%16)]
      , [A ➤ p (  2), B ➤ p (1%4), C ➤ p (  8), D ➤ p (  16)]
      )
    ]
  where
    p = Product

unitTestSpec_invert_Sum_Integer :: Spec
unitTestSpec_invert_Sum_Integer = unitTestSpec
    "invert Sum Integer"
    "invert"
    (invert)
    (unitTestData_invert_Sum_Integer)

unitTestData_invert_Sum_Integer :: UnitTestData1
    (MonoidMap LatinChar (Sum Integer))
    (MonoidMap LatinChar (Sum Integer))
unitTestData_invert_Sum_Integer = unitTestData1
    [ ( [A ➤ s ( 1), B ➤ s ( 2), C ➤ s ( 3), D ➤ s ( 4)]
      , [A ➤ s (-1), B ➤ s (-2), C ➤ s (-3), D ➤ s (-4)]
      )
    , ( [A ➤ s (-1), B ➤ s (-2), C ➤ s (-3), D ➤ s (-4)]
      , [A ➤ s ( 1), B ➤ s ( 2), C ➤ s ( 3), D ➤ s ( 4)]
      )
    , ( [A ➤ s ( 1), B ➤ s (-2), C ➤ s ( 3), D ➤ s (-4)]
      , [A ➤ s (-1), B ➤ s ( 2), C ➤ s (-3), D ➤ s ( 4)]
      )
    , ( [A ➤ s (-1), B ➤ s ( 2), C ➤ s (-3), D ➤ s ( 4)]
      , [A ➤ s ( 1), B ➤ s (-2), C ➤ s ( 3), D ➤ s (-4)]
      )
    ]
  where
    s = Sum

unitTestSpec_pow_Product_Rational :: Spec
unitTestSpec_pow_Product_Rational = unitTestSpec
    "pow Product Rational"
    "pow"
    (pow)
    (unitTestData_pow_Product_Rational)

unitTestData_pow_Product_Rational :: UnitTestData2
    (MonoidMap LatinChar (Product Rational))
    (Integer)
    (MonoidMap LatinChar (Product Rational))
unitTestData_pow_Product_Rational = unitTestData2
    [ ( [A ➤ p (  2), B ➤ p (-   4), C ➤ p (  8), D ➤ p (-   16)], (-1)
      , [A ➤ p (1%2), B ➤ p (- 1%4), C ➤ p (1%8), D ➤ p (- 1%16)]
      )
    , ( [A ➤ p (  2), B ➤ p (-   4), C ➤ p (  8), D ➤ p (-   16)], 0
      , [A ➤ p (  1), B ➤ p (    1), C ➤ p (  1), D ➤ p (     1)]
      )
    , ( [A ➤ p (  2), B ➤ p (-   4), C ➤ p (  8), D ➤ p (-   16)], 1
      , [A ➤ p (  2), B ➤ p (-   4), C ➤ p (  8), D ➤ p (-   16)]
      )
    , ( [A ➤ p (  2), B ➤ p (-   4), C ➤ p (  8), D ➤ p (-   16)], 2
      , [A ➤ p (  4), B ➤ p (   16), C ➤ p ( 64), D ➤ p (   256)]
      )
    ]
  where
    p = Product

unitTestSpec_pow_Sum_Integer :: Spec
unitTestSpec_pow_Sum_Integer = unitTestSpec
    "pow Sum Integer"
    "pow"
    (pow)
    (unitTestData_pow_Sum_Integer)

unitTestData_pow_Sum_Integer :: UnitTestData2
    (MonoidMap LatinChar (Sum Integer))
    (Integer)
    (MonoidMap LatinChar (Sum Integer))
unitTestData_pow_Sum_Integer = unitTestData2
    [ ( [A ➤ s ( 1), B ➤ s (-2), C ➤ s ( 3), D ➤ s (-4)], (-1)
      , [A ➤ s (-1), B ➤ s ( 2), C ➤ s (-3), D ➤ s ( 4)]
      )
    , ( [A ➤ s ( 1), B ➤ s (-2), C ➤ s ( 3), D ➤ s (-4)], 0
      , [A ➤ s ( 0), B ➤ s ( 0), C ➤ s ( 0), D ➤ s ( 0)]
      )
    , ( [A ➤ s ( 1), B ➤ s (-2), C ➤ s ( 3), D ➤ s (-4)], 1
      , [A ➤ s ( 1), B ➤ s (-2), C ➤ s ( 3), D ➤ s (-4)]
      )
    , ( [A ➤ s ( 1), B ➤ s (-2), C ➤ s ( 3), D ➤ s (-4)], 2
      , [A ➤ s ( 2), B ➤ s (-4), C ➤ s ( 6), D ➤ s (-8)]
      )
    ]
  where
    s = Sum

unitTestSpec_isPrefixOf_String :: Spec
unitTestSpec_isPrefixOf_String = unitTestSpec
    "isPrefixOf String"
    "isPrefixOf"
    (isPrefixOf)
    (unitTestData_isPrefixOf_String)

unitTestData_isPrefixOf_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (Bool)
unitTestData_isPrefixOf_String = unitTestData2
    [ ( [A ➤ "A"   , B ➤ "B"   , C ➤ "C"   ]
      , [A ➤ "A123", B ➤ "B123", C ➤ "C123"]
      , True
      )
    , ( [A ➤ "A123", B ➤ "B123", C ➤ "C123"]
      , [A ➤ "A"   , B ➤ "B"   , C ➤ "C"   ]
      , False
      )
    ]

unitTestSpec_isSuffixOf_String :: Spec
unitTestSpec_isSuffixOf_String = unitTestSpec
    "isSuffixOf String"
    "isSuffixOf"
    (isSuffixOf)
    (unitTestData_isSuffixOf_String)

unitTestData_isSuffixOf_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (Bool)
unitTestData_isSuffixOf_String = unitTestData2
    [ ( [A ➤    "A", B ➤    "B", C ➤    "C"]
      , [A ➤ "123A", B ➤ "123B", C ➤ "123C"]
      , True
      )
    , ( [A ➤ "123A", B ➤ "123B", C ➤ "123C"]
      , [A ➤    "A", B ➤    "B", C ➤    "C"]
      , False
      )
    ]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

(➤) :: a -> b -> (a, b)
a ➤ b = (a, b)

data LatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Show)
