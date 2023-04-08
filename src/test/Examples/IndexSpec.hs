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
    --specFor (Proxy @(Index1 Int Int))
    specFor (Proxy @(Index2 Int Int))
    specFor (Proxy @(Index3 Int Int))
    specFor (Proxy @(Index4 Int Int))

type TestConstraints i k v =
        ( Arbitrary k
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

        describe "Validity" $ do

            it "prop_empty_valid" $
                prop_empty_valid
                    @i @k @v & property
            it "prop_fromList_valid" $
                prop_fromList_valid
                    @i @k @v & property
            it "prop_update_valid" $
                prop_update_valid
                    @i @k @v & property
            it "prop_insert_valid" $
                prop_insert_valid
                    @i @k @v & property
            it "prop_remove_valid" $
                prop_remove_valid
                    @i @k @v & property
            it "prop_union_valid" $
                prop_union_valid
                    @i @k @v & property
            it "prop_intersection_valid" $
                prop_intersection_valid
                    @i @k @v & property

{-
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
        it "prop_valid_nonNullKeys" $
            prop_valid_nonNullKeys
                @i @k @v & property
        it "prop_nonNullKeys_nonNullCount" $
            prop_nonNullKeys_nonNullCount
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
-}

--------------------------------------------------------------------------------
-- Validity properties
--------------------------------------------------------------------------------
-- An index is valid if (and only if) it is not possible to reveal an empty
-- set when traversing the set of mappings from keys to value sets.
--------------------------------------------------------------------------------

prop_valid
    :: TestConstraints i k v => i k v -> Property
prop_valid i = QC.conjoin
    [ counterexample
        "prop_valid_nonNullKeys"
        (prop_valid_nonNullKeys i)
    , counterexample
        "prop_valid_toList"
        (prop_valid_toList i)
    ]

prop_valid_nonNullKeys
    :: TestConstraints i k v => i k v -> Property
prop_valid_nonNullKeys i = QC.property $
    all (\k -> I.lookup k i /= Set.empty) (I.nonNullKeys i)

prop_valid_toList
    :: TestConstraints i k v => i k v -> Property
prop_valid_toList i = QC.property $
    all (\(_, v) -> v /= Set.empty) (I.toList i)

--------------------------------------------------------------------------------
-- Validity of operations that produce indices
--------------------------------------------------------------------------------

prop_empty_valid
    :: forall i k v. TestConstraints i k v => Property
prop_empty_valid = prop_valid (I.empty @i @k @v)

prop_fromList_valid
    :: forall i k v. TestConstraints i k v => [(k, Set v)] -> Property
prop_fromList_valid kvs = prop_valid (I.fromList @i @k @v kvs)

prop_update_valid
    :: forall i k v. TestConstraints i k v
    => k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_update_valid k vs kvs =
    prop_valid @i @k @v (I.update k vs (I.fromList kvs))

prop_insert_valid
    :: forall i k v. TestConstraints i k v
    => k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_insert_valid k vs kvs =
    prop_valid @i @k @v (I.insert k vs (I.fromList kvs))

prop_remove_valid
    :: forall i k v. TestConstraints i k v
    => k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_remove_valid k vs kvs =
    prop_valid @i @k @v (I.remove k vs (I.fromList kvs))

prop_union_valid
    :: forall i k v. TestConstraints i k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_union_valid kvs1 kvs2 =
    prop_valid @i @k @v (I.union (I.fromList kvs1) (I.fromList kvs2))

prop_intersection_valid
    :: forall i k v. TestConstraints i k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_intersection_valid kvs1 kvs2 =
    prop_valid @i @k @v (I.intersection (I.fromList kvs1) (I.fromList kvs2))







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
    prop_valid_nonNullKeys (I.update k vs i)

prop_nonNullKey_lookup
    :: TestConstraints i k v => k -> i k v -> Property
prop_nonNullKey_lookup k i =
    I.nonNullKey k i === (I.lookup k i /= Set.empty)

prop_nonNullKeys_nonNullCount
    :: TestConstraints i k v => i k v -> Property
prop_nonNullKeys_nonNullCount i =
    I.nonNullCount i === Set.size (I.nonNullKeys i)

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
    prop_valid_nonNullKeys (i1 `I.intersection` i2)

prop_intersection_toList_all_nonNull
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_intersection_toList_all_nonNull i1 i2 =
    prop_valid_toList (i1 `I.intersection` i2)

prop_union_nonNullKeys_lookup_all_nonNull
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_union_nonNullKeys_lookup_all_nonNull i1 i2 =
    prop_valid_nonNullKeys (i1 `I.union` i2)

prop_union_toList_all_nonNull
    :: TestConstraints i k v => i k v -> i k v -> Property
prop_union_toList_all_nonNull i1 i2 =
    prop_valid_toList (i1 `I.union` i2)

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
-- Utilities
--------------------------------------------------------------------------------

report :: (Show a, Testable prop) => String -> a -> prop -> Property
report name a = counterexample $
    (replaceSpecialChars <$> name) <> ":\n" <> show a <> "\n"

replaceSpecialChars :: Char -> Char
replaceSpecialChars = \case
    'λ'   -> '\\'
    other -> other
