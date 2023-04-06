{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use null" #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Examples.IndexSpec
    where

import Prelude

import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Set
    ( Set )
import Data.Typeable
    ( Typeable, typeRep )
import Examples.Index
    ( Index )
import Examples.Index.Index1
    ( Index1 )
import Examples.Index.Index2
    ( Index2 )
import Examples.Index.Index3
    ( Index3 )
import Examples.Index.Index4
    ( Index4 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , Testable
    , checkCoverage
    , counterexample
    , cover
    , shrinkMap
    , (.||.)
    , (===)
    )
import Test.QuickCheck.Instances.Natural
    ()
import Test.QuickCheck.Instances.Text
    ()

import qualified Data.Set as Set
import qualified Examples.Index as I
import qualified Test.QuickCheck as QC

spec :: Spec
spec = do

    -- Uncomment the following lines to see property test failures for an
    -- unlawful implementation of 'Index':
    --
    specFor (Proxy @(Index1 Int Int))
    specFor (Proxy @(Index2 Int Int))
    specFor (Proxy @(Index3 Int Int))
    specFor (Proxy @(Index4 Int Int))

type TestConstraints i k v =
        ( Arbitrary (i k v)
        , Arbitrary k
        , Arbitrary v
        , Index i k v
        , Show (i k v)
        , Show k
        , Show v
        , Typeable i
        , Typeable k
        , Typeable v
        )

specFor :: forall i k v. TestConstraints i k v => Proxy (i k v) -> Spec
specFor indexType = do

    let description = show (typeRep indexType)

    let property :: Testable t => t -> Property
        property = checkCoverage . QC.property

    describe description $ do

        it "prop_empty_lookup" $
            prop_empty_lookup
                @i @k @v & property
        it "prop_update_lookup" $
            prop_update_lookup
                @i @k @v & property
        it "prop_update_nonNullKeys_lookup_all_notNull" $
            prop_update_nonNullKeys_lookup_all_notNull
                @i @k @v & property
        it "prop_nonNullKey_lookup" $
            prop_nonNullKey_lookup
                @i @k @v & property
        it "prop_nonNullKeys_lookup_all_notNull" $
            prop_nonNullKeys_lookup_all_notNull
                @i @k @v & property
        it "prop_nonNullKeys_nonNullKeyCount" $
            prop_nonNullKeys_nonNullKeyCount
                @i @k @v & property
        it "prop_null_nullKey" $
            prop_null_nullKey
                @i @k @v & property
        it "prop_insert_lookup" $
            prop_insert_lookup
                @i @k @v & property
        it "prop_remove_lookup" $
            prop_remove_lookup
                @i @k @v & property
        it "prop_union_lookup" $
            prop_union_lookup
                @i @k @v & property
        it "prop_isSubIndexOf_lookup" $
            prop_isSubIndexOf_lookup
                @i @k @v & property

        it "prop_intersection_nonNullKeys_lookup_all_notNull" $
            prop_intersection_nonNullKeys_lookup_all_notNull
                @i @k @v & property
        it "prop_intersection_toList_all_nonNull" $
            prop_intersection_toList_all_nonNull
                @i @k @v & property
        it "prop_intersection_isSubIndexOf_1" $
            prop_intersection_isSubIndexOf_1
                @i @k @v & property
        it "prop_intersection_isSubIndexOf_2" $
            prop_intersection_isSubIndexOf_2
                @i @k @v & property

        it "prop_union_nonNullKeys_lookup_all_nonNull" $
            prop_union_nonNullKeys_lookup_all_nonNull
                @i @k @v & property
        it "prop_union_toList_all_nonNull" $
            prop_union_toList_all_nonNull
                @i @k @v & property
        it "prop_union_isSubIndexOf_1" $
            prop_union_isSubIndexOf_1
                @i @k @v & property
        it "prop_union_isSubIndexOf_2" $
            prop_union_isSubIndexOf_2
                @i @k @v & property

        it "prop_union_intersection_distributive" $
            prop_union_intersection_distributive
                @i @k @v & property

prop_toList_all_nonNull
    :: TestConstraints i k v => i k v -> Property
prop_toList_all_nonNull i = QC.property $
    all (\(_, v) -> v /= Set.empty) (I.toList i)

prop_empty_lookup :: forall i k v. ()
    => TestConstraints i k v => k -> Property
prop_empty_lookup k =
    I.lookup k (I.empty @i @k @v) === Set.empty

prop_update_lookup
    :: TestConstraints i k v => k -> Set v -> i k v -> Property
prop_update_lookup k vs i =
    I.lookup k (I.update k vs i) === vs

prop_update_nonNullKeys_lookup_all_notNull
    :: TestConstraints i k v => k -> Set v -> i k v -> Property
prop_update_nonNullKeys_lookup_all_notNull k vs i =
    prop_nonNullKeys_lookup_all_notNull (I.update k vs i)

prop_nonNullKey_lookup
    :: TestConstraints i k v => k -> i k v -> Property
prop_nonNullKey_lookup k i =
    I.nonNullKey k i === (I.lookup k i /= Set.empty)

prop_nonNullKeys_lookup_all_notNull
    :: TestConstraints i k v => i k v -> Property
prop_nonNullKeys_lookup_all_notNull i = QC.property $
    all (\k -> I.lookup k i /= Set.empty) (I.nonNullKeys i)

prop_nonNullKeys_nonNullKeyCount
    :: TestConstraints i k v => i k v -> Property
prop_nonNullKeys_nonNullKeyCount i =
    I.nonNullKeyCount i === Set.size (I.nonNullKeys i)

prop_null_nullKey
    :: TestConstraints i k v => k -> i k v -> Property
prop_null_nullKey k i =
    I.null i ==> not (I.nonNullKey k i)

prop_insert_lookup
    :: TestConstraints i k v => k -> Set v -> i k v -> Property
prop_insert_lookup k vs i =
    I.lookup k (I.insert k vs i)
    ===
    I.lookup k i `Set.union` vs

prop_remove_lookup
    :: TestConstraints i k v => k -> Set v -> i k v -> Property
prop_remove_lookup k vs i =
    I.lookup k (I.remove k vs i)
    ===
    I.lookup k i `Set.difference` vs

prop_union_lookup
    :: TestConstraints i k v => k -> i k v -> i k v -> Property
prop_union_lookup k i1 i2 =
    I.lookup k (i1 `I.union` i2)
    ===
    I.lookup k i1 `Set.union` I.lookup k i2

prop_isSubIndexOf_lookup
    :: TestConstraints i k v => k -> i k v -> i k v -> Property
prop_isSubIndexOf_lookup k i1 i2 =
    (i1 `I.isSubIndexOf` i2
        ==>
        I.lookup k i1 `Set.isSubsetOf` I.lookup k i2)
    & cover 1
        (i1 `I.isSubIndexOf` i2)
        "i1 `I.isSubIndexOf` i2"

prop_intersection_nonNullKeys_lookup_all_notNull
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_intersection_nonNullKeys_lookup_all_notNull i1 i2 =
    prop_nonNullKeys_lookup_all_notNull (i1 `I.intersection` i2)

prop_intersection_toList_all_nonNull
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_intersection_toList_all_nonNull i1 i2 =
    prop_toList_all_nonNull (i1 `I.intersection` i2)

prop_union_nonNullKeys_lookup_all_nonNull
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_union_nonNullKeys_lookup_all_nonNull i1 i2 =
    prop_nonNullKeys_lookup_all_notNull (i1 `I.union` i2)

prop_union_toList_all_nonNull
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_union_toList_all_nonNull i1 i2 =
    prop_toList_all_nonNull (i1 `I.union` i2)

prop_intersection_isSubIndexOf_1
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_intersection_isSubIndexOf_1 i1 i2 =
    (i1 `I.intersection` i2) `I.isSubIndexOf` i1
    & cover 1
        (not (I.null (i1 `I.intersection` i2)))
        "not (I.null (i1 `I.intersection` i2))"

prop_intersection_isSubIndexOf_2
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_intersection_isSubIndexOf_2 i1 i2 =
    (i1 `I.intersection` i2) `I.isSubIndexOf` i2
    & cover 1
        (not (I.null (i1 `I.intersection` i2)))
        "not (I.null (i1 `I.intersection` i2))"

prop_union_isSubIndexOf_1
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_union_isSubIndexOf_1 i1 i2 =
    i1 `I.isSubIndexOf` (i1 `I.union` i2) === True

prop_union_isSubIndexOf_2
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_union_isSubIndexOf_2 i1 i2 =
    i2 `I.isSubIndexOf` (i1 `I.union` i2) === True

prop_union_intersection_distributive
    :: TestConstraints i k v => i k v -> i k v -> i k v -> Property
prop_union_intersection_distributive i1 i2 i3 =
    (i1 `I.intersection` (i2 `I.union` i3)) ===
    ((i1 `I.intersection` i2) `I.union` (i1 `I.intersection` i3))

infixr 0 ==>
(==>) :: Bool -> Bool -> Property
a ==> b = not a .||. b

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

genIndex :: (Arbitrary k, Arbitrary v, Index i k v) => Gen (i k v)
genIndex = I.fromList <$> arbitrary

shrinkIndex :: (Arbitrary k, Arbitrary v, Index i k v) => i k v -> [i k v]
shrinkIndex = shrinkMap I.fromList toFlatList
  where
    toFlatList i = [(k, v) | (k, vs) <- I.toList i, v <- Set.toList vs]

instance (Arbitrary k, Arbitrary v, Ord k, Ord v) => Arbitrary (Index1 k v)
  where
    arbitrary = genIndex
    shrink = shrinkIndex

instance (Arbitrary k, Arbitrary v, Ord k, Ord v) => Arbitrary (Index2 k v)
  where
    arbitrary = genIndex
    shrink = shrinkIndex

instance (Arbitrary k, Arbitrary v, Ord k, Ord v) => Arbitrary (Index3 k v)
  where
    arbitrary = genIndex
    shrink = shrinkIndex

instance (Arbitrary k, Arbitrary v, Ord k, Ord v) => Arbitrary (Index4 k v)
  where
    arbitrary = genIndex
    shrink = shrinkIndex

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

report :: (Show a, Testable prop) => String -> a -> prop -> Property
report name a = counterexample $
    (replaceSpecialChars <$> name) <> ":\n" <> show a <> "\n"

replaceSpecialChars :: Char -> Char
replaceSpecialChars = \case
    'λ'   -> '\\'
    other -> other
