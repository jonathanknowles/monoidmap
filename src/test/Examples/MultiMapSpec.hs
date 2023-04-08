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
module Examples.MultiMapSpec
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
import Examples.MultiMap
    ( MultiMap )
import Examples.MultiMap.MultiMap1
    ( MultiMap1 )
import Examples.MultiMap.MultiMap2
    ( MultiMap2 )
import Examples.MultiMap.MultiMap3
    ( MultiMap3 )
import Examples.MultiMap.MultiMap4
    ( MultiMap4 )
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
import qualified Examples.MultiMap as M
import qualified Test.QuickCheck as QC

spec :: Spec
spec = do

    -- Uncomment the following lines to see property test failures for an
    -- unlawful implementation of 'MultiMap':
    --
    specFor (Proxy @(MultiMap1 Int Int))
    specFor (Proxy @(MultiMap2 Int Int))
    specFor (Proxy @(MultiMap3 Int Int))
    specFor (Proxy @(MultiMap4 Int Int))

type TestConstraints m k v =
        ( Arbitrary k
        , Arbitrary v
        , MultiMap m k v
        , Show (m k v)
        , Show k
        , Show v
        , Typeable m
        , Typeable k
        , Typeable v
        )

specFor :: forall m k v. TestConstraints m k v => Proxy (m k v) -> Spec
specFor multimapType = do

    let description = show (typeRep multimapType)

    let property :: Testable t => t -> Property
        property = checkCoverage . QC.property

    describe description $ do

        describe "Validity" $ do

            it "prop_empty_valid" $
                prop_empty_valid
                    @m @k @v & property
            it "prop_fromList_valid" $
                prop_fromList_valid
                    @m @k @v & property
            it "prop_update_valid" $
                prop_update_valid
                    @m @k @v & property
            it "prop_insert_valid" $
                prop_insert_valid
                    @m @k @v & property
            it "prop_remove_valid" $
                prop_remove_valid
                    @m @k @v & property
            it "prop_union_valid" $
                prop_union_valid
                    @m @k @v & property
            it "prop_intersection_valid" $
                prop_intersection_valid
                    @m @k @v & property

{-
        it "prop_empty_lookup" $
            prop_empty_lookup
                @m @k @v & property
        it "prop_update_lookup" $
            prop_update_lookup
                @m @k @v & property
        it "prop_update_nonNullKeys_lookup_all_notNull" $
            prop_update_nonNullKeys_lookup_all_notNull
                @m @k @v & property
        it "prop_nonNullKey_lookup" $
            prop_nonNullKey_lookup
                @m @k @v & property
        it "prop_valid_nonNullKeys" $
            prop_valid_nonNullKeys
                @m @k @v & property
        it "prop_nonNullKeys_nonNullCount" $
            prop_nonNullKeys_nonNullCount
                @m @k @v & property
        it "prop_null_nullKey" $
            prop_null_nullKey
                @m @k @v & property
        it "prop_insert_lookup" $
            prop_insert_lookup
                @m @k @v & property
        it "prop_remove_lookup" $
            prop_remove_lookup
                @m @k @v & property
        it "prop_union_lookup" $
            prop_union_lookup
                @m @k @v & property
        it "prop_isSubMultiMapOf_lookup" $
            prop_isSubMultiMapOf_lookup
                @m @k @v & property

        it "prop_intersection_nonNullKeys_lookup_all_notNull" $
            prop_intersection_nonNullKeys_lookup_all_notNull
                @m @k @v & property
        it "prop_intersection_toList_all_nonNull" $
            prop_intersection_toList_all_nonNull
                @m @k @v & property
        it "prop_intersection_isSubMultiMapOf_1" $
            prop_intersection_isSubMultiMapOf_1
                @m @k @v & property
        it "prop_intersection_isSubMultiMapOf_2" $
            prop_intersection_isSubMultiMapOf_2
                @m @k @v & property

        it "prop_union_nonNullKeys_lookup_all_nonNull" $
            prop_union_nonNullKeys_lookup_all_nonNull
                @m @k @v & property
        it "prop_union_toList_all_nonNull" $
            prop_union_toList_all_nonNull
                @m @k @v & property
        it "prop_union_isSubMultiMapOf_1" $
            prop_union_isSubMultiMapOf_1
                @m @k @v & property
        it "prop_union_isSubMultiMapOf_2" $
            prop_union_isSubMultiMapOf_2
                @m @k @v & property

        it "prop_union_intersection_distributive" $
            prop_union_intersection_distributive
                @m @k @v & property
-}

--------------------------------------------------------------------------------
-- Validity properties
--------------------------------------------------------------------------------
-- A multimap is valid if (and only if) it is not possible to reveal an empty
-- set when traversing the set of mappings from keys to value sets.
--------------------------------------------------------------------------------

prop_valid
    :: TestConstraints m k v => m k v -> Property
prop_valid m = QC.conjoin
    [ counterexample
        "prop_valid_nonNullKeys"
        (prop_valid_nonNullKeys m)
    , counterexample
        "prop_valid_toList"
        (prop_valid_toList m)
    ]

prop_valid_nonNullKeys
    :: TestConstraints m k v => m k v -> Property
prop_valid_nonNullKeys m = QC.property $
    all (\k -> M.lookup k m /= Set.empty) (M.nonNullKeys m)

prop_valid_toList
    :: TestConstraints m k v => m k v -> Property
prop_valid_toList m = QC.property $
    all (\(_, v) -> v /= Set.empty) (M.toList m)

--------------------------------------------------------------------------------
-- Validity of operations that produce indices
--------------------------------------------------------------------------------

prop_empty_valid
    :: forall m k v. TestConstraints m k v
    => Property
prop_empty_valid =
    prop_valid @m @k @v M.empty

prop_fromList_valid
    :: forall m k v. TestConstraints m k v
    => [(k, Set v)]
    -> Property
prop_fromList_valid kvs =
    prop_valid @m @k @v (M.fromList kvs)

prop_update_valid
    :: forall m k v. TestConstraints m k v
    => k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_update_valid k vs kvs =
    prop_valid @m @k @v (M.update k vs (M.fromList kvs))

prop_insert_valid
    :: forall m k v. TestConstraints m k v
    => k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_insert_valid k vs kvs =
    prop_valid @m @k @v (M.insert k vs (M.fromList kvs))

prop_remove_valid
    :: forall m k v. TestConstraints m k v
    => k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_remove_valid k vs kvs =
    prop_valid @m @k @v (M.remove k vs (M.fromList kvs))

prop_union_valid
    :: forall m k v. TestConstraints m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_union_valid kvs1 kvs2 =
    prop_valid @m @k @v (M.union (M.fromList kvs1) (M.fromList kvs2))

prop_intersection_valid
    :: forall m k v. TestConstraints m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_intersection_valid kvs1 kvs2 =
    prop_valid @m @k @v (M.intersection (M.fromList kvs1) (M.fromList kvs2))







prop_empty_lookup :: forall m k v. ()
    => TestConstraints m k v => k -> Property
prop_empty_lookup k =
    M.lookup k (M.empty @m @k @v) === Set.empty

prop_update_lookup
    :: TestConstraints m k v => k -> Set v -> m k v -> Property
prop_update_lookup k vs m =
    M.lookup k (M.update k vs m) === vs

prop_update_nonNullKeys_lookup_all_notNull
    :: TestConstraints m k v => k -> Set v -> m k v -> Property
prop_update_nonNullKeys_lookup_all_notNull k vs m =
    prop_valid_nonNullKeys (M.update k vs m)

prop_nonNullKey_lookup
    :: TestConstraints m k v => k -> m k v -> Property
prop_nonNullKey_lookup k m =
    M.nonNullKey k m === (M.lookup k m /= Set.empty)

prop_nonNullKeys_nonNullCount
    :: TestConstraints m k v => m k v -> Property
prop_nonNullKeys_nonNullCount m =
    M.nonNullCount m === Set.size (M.nonNullKeys m)

prop_null_nullKey
    :: TestConstraints m k v => k -> m k v -> Property
prop_null_nullKey k m =
    M.null m ==> not (M.nonNullKey k m)

prop_insert_lookup
    :: TestConstraints m k v => k -> Set v -> m k v -> Property
prop_insert_lookup k vs m =
    M.lookup k (M.insert k vs m)
    ===
    M.lookup k m `Set.union` vs

prop_remove_lookup
    :: TestConstraints m k v => k -> Set v -> m k v -> Property
prop_remove_lookup k vs m =
    M.lookup k (M.remove k vs m)
    ===
    M.lookup k m `Set.difference` vs

prop_union_lookup
    :: TestConstraints m k v => k -> m k v -> m k v -> Property
prop_union_lookup k m1 m2 =
    M.lookup k (m1 `M.union` m2)
    ===
    M.lookup k m1 `Set.union` M.lookup k m2

prop_isSubMultiMapOf_lookup
    :: TestConstraints m k v => k -> m k v -> m k v -> Property
prop_isSubMultiMapOf_lookup k m1 m2 =
    (m1 `M.isSubMultiMapOf` m2
        ==>
        M.lookup k m1 `Set.isSubsetOf` M.lookup k m2)
    & cover 1
        (m1 `M.isSubMultiMapOf` m2)
        "m1 `M.isSubMultiMapOf` m2"

prop_intersection_nonNullKeys_lookup_all_notNull
    :: TestConstraints m k v => m k v -> m k v -> Property
prop_intersection_nonNullKeys_lookup_all_notNull m1 m2 =
    prop_valid_nonNullKeys (m1 `M.intersection` m2)

prop_intersection_toList_all_nonNull
    :: TestConstraints m k v => m k v -> m k v -> Property
prop_intersection_toList_all_nonNull m1 m2 =
    prop_valid_toList (m1 `M.intersection` m2)

prop_union_nonNullKeys_lookup_all_nonNull
    :: TestConstraints m k v => m k v -> m k v -> Property
prop_union_nonNullKeys_lookup_all_nonNull m1 m2 =
    prop_valid_nonNullKeys (m1 `M.union` m2)

prop_union_toList_all_nonNull
    :: TestConstraints m k v => m k v -> m k v -> Property
prop_union_toList_all_nonNull m1 m2 =
    prop_valid_toList (m1 `M.union` m2)

prop_intersection_isSubMultiMapOf_1
    :: TestConstraints m k v => m k v -> m k v -> Property
prop_intersection_isSubMultiMapOf_1 m1 m2 =
    (m1 `M.intersection` m2) `M.isSubMultiMapOf` m1
    & cover 1
        (not (M.null (m1 `M.intersection` m2)))
        "not (M.null (m1 `M.intersection` m2))"

prop_intersection_isSubMultiMapOf_2
    :: TestConstraints m k v => m k v -> m k v -> Property
prop_intersection_isSubMultiMapOf_2 m1 m2 =
    (m1 `M.intersection` m2) `M.isSubMultiMapOf` m2
    & cover 1
        (not (M.null (m1 `M.intersection` m2)))
        "not (M.null (m1 `M.intersection` m2))"

prop_union_isSubMultiMapOf_1
    :: TestConstraints m k v => m k v -> m k v -> Property
prop_union_isSubMultiMapOf_1 m1 m2 =
    m1 `M.isSubMultiMapOf` (m1 `M.union` m2) === True

prop_union_isSubMultiMapOf_2
    :: TestConstraints m k v => m k v -> m k v -> Property
prop_union_isSubMultiMapOf_2 m1 m2 =
    m2 `M.isSubMultiMapOf` (m1 `M.union` m2) === True

prop_union_intersection_distributive
    :: TestConstraints m k v => m k v -> m k v -> m k v -> Property
prop_union_intersection_distributive m1 m2 m3 =
    (m1 `M.intersection` (m2 `M.union` m3)) ===
    ((m1 `M.intersection` m2) `M.union` (m1 `M.intersection` m3))

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
