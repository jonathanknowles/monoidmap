{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use any" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Examples.MultiMapSpec
    where

import Prelude

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Set
    ( Set )
import Data.Typeable
    ( Typeable, typeRep )
import Data.MonoidMap.Examples.MultiMap.Class
    ( MultiMap )
import Data.MonoidMap.Examples.MultiMap.Instances.MultiMap1
    ( MultiMap1 )
import Data.MonoidMap.Examples.MultiMap.Instances.MultiMap2
    ( MultiMap2 )
import Data.MonoidMap.Examples.MultiMap.Instances.MultiMap3
    ( MultiMap3 )
import Data.MonoidMap.Examples.MultiMap.Instances.MultiMap4
    ( MultiMap4 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Testable
    , checkCoverage
    , counterexample
    , cover
    , (.||.)
    , (===)
    )

import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.MonoidMap.Examples.MultiMap.Class as M
import qualified Test.QuickCheck as QC

spec :: Spec
spec = do

    -- Uncomment the following line to see property test failures for an
    -- unlawful implementation of 'MultiMap':
    --
    -- specFor (Proxy @(MultiMap1 Int Int))

    specFor (Proxy @(MultiMap2 Int Int))
    specFor (Proxy @(MultiMap3 Int Int))
    specFor (Proxy @(MultiMap4 Int Int))

type Test m k v =
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

specFor :: forall m k v. Test m k v => Proxy (m k v) -> Spec
specFor multimapType = do

    let description = show (typeRep multimapType)

    let property :: Testable t => t -> Property
        property = checkCoverage . QC.property

    describe description $ do

        describe "Validity properties" $ do

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

        describe "General properties" $ do

            it "prop_fromList_filter" $
                prop_fromList_filter
                    @m @k @v & property
            it "prop_toList_filter" $
                prop_toList_filter
                    @m @k @v & property
            it "prop_empty_fromList" $
                prop_empty_fromList
                    @m @k @v & property
            it "prop_lookup_filter_fold" $
                prop_lookup_filter_fold
                    @m @k @v & property
            it "prop_null_lookup" $
                prop_null_lookup
                    @m @k @v & property
            it "prop_nonNull_lookup" $
                prop_nonNull_lookup
                    @m @k @v & property
            it "prop_nonNullKey_lookup" $
                prop_nonNullKey_lookup
                    @m @k @v & property
            it "prop_nonNullKeys_nonNullKey" $
                prop_nonNullKeys_nonNullKey
                    @m @k @v & property
            it "prop_nonNullCount_nonNullKeys" $
                prop_nonNullCount_nonNullKeys
                    @m @k @v & property
            it "prop_isSubmapOf_lookup" $
                prop_isSubmapOf_lookup
                    @m @k @v & property
            it "prop_update_lookup" $
                prop_update_lookup
                    @m @k @v & property
            it "prop_insert_lookup" $
                prop_insert_lookup
                    @m @k @v & property
            it "prop_remove_lookup" $
                prop_remove_lookup
                    @m @k @v & property
            it "prop_union_idempotence" $
                prop_union_idempotence
                    @m @k @v & property
            it "prop_union_identity_left" $
                prop_union_identity_left
                    @m @k @v & property
            it "prop_union_identity_right" $
                prop_union_identity_right
                    @m @k @v & property
            it "prop_union_commutativity" $
                prop_union_commutativity
                    @m @k @v & property
            it "prop_union_associativity" $
                prop_union_associativity
                    @m @k @v & property
            it "prop_union_containment_left" $
                prop_union_containment_left
                    @m @k @v & property
            it "prop_union_containment_right" $
                prop_union_containment_right
                    @m @k @v & property
            it "prop_union_distributivity" $
                prop_union_distributivity
                    @m @k @v & property
            it "prop_intersection_idempotence" $
                prop_intersection_idempotence
                    @m @k @v & property
            it "prop_intersection_identity_left" $
                prop_intersection_identity_left
                    @m @k @v & property
            it "prop_intersection_identity_right" $
                prop_intersection_identity_right
                    @m @k @v & property
            it "prop_intersection_commutativity" $
                prop_intersection_commutativity
                    @m @k @v & property
            it "prop_intersection_associativity" $
                prop_intersection_associativity
                    @m @k @v & property
            it "prop_intersection_containment_left" $
                prop_intersection_containment_left
                    @m @k @v & property
            it "prop_intersection_containment_right" $
                prop_intersection_containment_right
                    @m @k @v & property
            it "prop_intersection_distributivity" $
                prop_intersection_distributivity
                    @m @k @v & property

--------------------------------------------------------------------------------
-- Validity properties
--------------------------------------------------------------------------------

-- A multimap is valid if (and only if):
--
-- - all keys included in 'nonNullKeys' are associated with non-empty sets.
-- - all keys included in 'toList'      are associated with non-empty sets.

prop_valid
    :: Test m k v => m k v -> Property
prop_valid m = QC.conjoin
    [ counterexample
        "prop_valid_nonNullKeys"
        (prop_valid_nonNullKeys)
    , counterexample
        "prop_valid_toList"
        (prop_valid_toList)
    ]
    & cover 1
        (M.null m)
        "M.null m"
    & cover 1
        (M.nonNull m)
        "M.nonNull m"
  where
    prop_valid_nonNullKeys =
        all (\k -> M.lookup k m /= Set.empty) (M.nonNullKeys m)
    prop_valid_toList =
        all (\(_, v) -> v /= Set.empty) (M.toList m)

--------------------------------------------------------------------------------
-- Validity of operations that produce multimaps
--------------------------------------------------------------------------------

prop_fromList_valid
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_fromList_valid kvs =
    prop_valid @m @k @v (M.fromList kvs)

prop_update_valid
    :: forall m k v. Test m k v
    => k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_update_valid k vs kvs =
    prop_valid @m @k @v (M.update k vs (M.fromList kvs))

prop_insert_valid
    :: forall m k v. Test m k v
    => k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_insert_valid k vs kvs =
    prop_valid @m @k @v (M.insert k vs (M.fromList kvs))

prop_remove_valid
    :: forall m k v. Test m k v
    => k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_remove_valid k vs kvs =
    prop_valid @m @k @v (M.remove k vs (M.fromList kvs))

prop_union_valid
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_union_valid kvs1 kvs2 =
    prop_valid @m @k @v (M.union (M.fromList kvs1) (M.fromList kvs2))

prop_intersection_valid
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_intersection_valid kvs1 kvs2 =
    prop_valid @m @k @v (M.intersection (M.fromList kvs1) (M.fromList kvs2))

--------------------------------------------------------------------------------
-- General properties
--------------------------------------------------------------------------------

prop_fromList_filter
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_fromList_filter kvs =
    M.fromList @m @k @v kvs === M.fromList (filter ((/= Set.empty) . snd) kvs)

prop_toList_filter
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_toList_filter kvs =
    M.toList m === filter ((/= Set.empty) . snd) (M.toList m)
  where
    m :: m k v
    m = M.fromList kvs

prop_empty_fromList
    :: forall m k v. Test m k v
    => Property
prop_empty_fromList =
    M.empty @m @k @v === M.fromList []

prop_lookup_filter_fold
    :: forall m k v. Test m k v
    => k
    -> [(k, Set v)]
    -> Property
prop_lookup_filter_fold k kvs =
    M.lookup k m === F.foldMap snd (filter ((== k) . fst) kvs)
    & cover 10
        (isJust (lookup k kvs))
        "isJust (lookup k kvs)"
    & cover 10
        (isNothing (lookup k kvs))
        "isNothing (lookup k kvs)"
  where
    m :: m k v
    m = M.fromList kvs

prop_null_lookup
    :: forall m k v. Test m k v
    => k
    -> [(k, Set v)]
    -> Property
prop_null_lookup k kvs =
    M.null m ==> M.lookup k m == Set.empty
    & cover 2
        (M.lookup k m == Set.empty && M.null m)
        "M.lookup k m == Set.empty && M.null m"
    & cover 2
        (M.lookup k m == Set.empty && M.nonNull m)
        "M.lookup k m == Set.empty && M.nonNull m"
    & cover 2
        (M.lookup k m /= Set.empty && M.nonNull m)
        "M.lookup k m /= Set.empty && M.nonNull m"
  where
    m :: m k v
    m = M.fromList kvs

prop_nonNull_lookup
    :: forall m k v. Test m k v
    => k
    -> [(k, Set v)]
    -> Property
prop_nonNull_lookup k kvs =
    M.lookup k m /= Set.empty ==> M.nonNull m
    & cover 2
        (M.lookup k m == Set.empty && M.null m)
        "M.lookup k m == Set.empty && M.null m"
    & cover 2
        (M.lookup k m == Set.empty && M.nonNull m)
        "M.lookup k m == Set.empty && M.nonNull m"
    & cover 2
        (M.lookup k m /= Set.empty && M.nonNull m)
        "M.lookup k m /= Set.empty && M.nonNull m"
  where
    m :: m k v
    m = M.fromList kvs

prop_nonNullKey_lookup
    :: forall m k v. Test m k v
    => k
    -> [(k, Set v)]
    -> Property
prop_nonNullKey_lookup k kvs =
    M.nonNullKey k m === (M.lookup k m /= Set.empty)
    & cover 2
        (M.lookup k m == Set.empty && M.null m)
        "M.lookup k m == Set.empty && M.null m"
    & cover 2
        (M.lookup k m == Set.empty && M.nonNull m)
        "M.lookup k m == Set.empty && M.nonNull m"
    & cover 2
        (M.lookup k m /= Set.empty && M.nonNull m)
        "M.lookup k m /= Set.empty && M.nonNull m"
  where
    m :: m k v
    m = M.fromList kvs

prop_nonNullKeys_nonNullKey
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_nonNullKeys_nonNullKey kvs = QC.property $
    all (`M.nonNullKey` m) (M.nonNullKeys m)
    & cover 2
        (M.null m)
        "M.null m"
    & cover 2
        (M.nonNull m)
        "M.nonNull m"
  where
    m :: m k v
    m = M.fromList kvs

prop_nonNullCount_nonNullKeys
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_nonNullCount_nonNullKeys kvs =
    M.nonNullCount m === Set.size (M.nonNullKeys m)
    & cover 1
        (M.nonNullCount m == 0)
        "M.nonNullCount m == 0"
    & cover 1
        (M.nonNullCount m == 1)
        "M.nonNullCount m == 1"
    & cover 1
        (M.nonNullCount m == 2)
        "M.nonNullCount m == 2"
    & cover 1
        (M.nonNullCount m >= 3)
        "M.nonNullCount m >= 3"
  where
    m :: m k v
    m = M.fromList kvs

prop_isSubmapOf_lookup
    :: forall m k v. Test m k v
    => k
    -> [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_isSubmapOf_lookup k kvs1 kvs2 =
    m1 `M.isSubmapOf` m2
        ==>
        M.lookup k m1 `Set.isSubsetOf` M.lookup k m2
    & cover 1
        (m1 `M.isSubmapOf` m2)
        "m1 `M.isSubmapOf` m2"
  where
    m1, m2 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2

prop_update_lookup
    :: forall m k v. Test m k v
    => k
    -> k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_update_lookup k1 k2 vs kvs =
    M.lookup k1 (M.update k2 vs m) === (if k1 == k2 then vs else M.lookup k1 m)
    & cover 1
        (k1 == k2)
        "k1 == k2"
    & cover 10
        (k1 /= k2)
        "k1 /= k2"
  where
    m :: m k v
    m = M.fromList kvs

prop_insert_lookup
    :: forall m k v. Test m k v
    => k
    -> k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_insert_lookup k1 k2 vs kvs =
    M.lookup k1 (M.insert k2 vs m) ===
        (if k1 == k2 then M.lookup k1 m `Set.union` vs else M.lookup k1 m)
    & cover 1
        (k1 == k2)
        "k1 == k2"
    & cover 10
        (k1 /= k2)
        "k1 /= k2"
  where
    m :: m k v
    m = M.fromList kvs

prop_remove_lookup
    :: forall m k v. Test m k v
    => k
    -> k
    -> Set v
    -> [(k, Set v)]
    -> Property
prop_remove_lookup k1 k2 vs kvs =
    M.lookup k1 (M.remove k2 vs m) ===
        (if k1 == k2 then M.lookup k1 m `Set.difference` vs else M.lookup k1 m)
    & cover 1
        (k1 == k2)
        "k1 == k2"
    & cover 10
        (k1 /= k2)
        "k1 /= k2"
  where
    m :: m k v
    m = M.fromList kvs

prop_union_idempotence
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_union_idempotence kvs =
    M.union m m === m
  where
    m :: m k v
    m = M.fromList kvs

prop_union_identity_left
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_union_identity_left kvs =
    M.union m M.empty === m
  where
    m :: m k v
    m = M.fromList kvs

prop_union_identity_right
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_union_identity_right kvs =
    M.union M.empty m === m
  where
    m :: m k v
    m = M.fromList kvs

prop_union_commutativity
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_union_commutativity kvs1 kvs2 =
    M.union m1 m2 === M.union m2 m1
  where
    m1, m2 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2

prop_union_associativity
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_union_associativity kvs1 kvs2 kvs3 =
    M.union m1 (M.union m2 m3)
        === M.union (M.union m1 m2) m3
  where
    m1, m2, m3 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2
    m3 = M.fromList kvs3

prop_union_containment_left
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_union_containment_left kvs1 kvs2 = QC.property $
    m1 `M.isSubmapOf` M.union m1 m2
  where
    m1, m2 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2

prop_union_containment_right
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_union_containment_right kvs1 kvs2 = QC.property $
    m2 `M.isSubmapOf` M.union m1 m2
  where
    m1, m2 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2

prop_union_distributivity
    :: forall m k v. Test m k v
    => k
    -> [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_union_distributivity k kvs1 kvs2 =
    M.lookup k (M.union m1 m2) === Set.union (M.lookup k m1) (M.lookup k m2)
    & cover 1
        (M.nonNullKey k (M.union m1 m2))
        "M.nonNullKey k (M.union m1 m2)"
  where
    m1, m2 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2

prop_intersection_idempotence
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_intersection_idempotence kvs =
    M.intersection m m === m
  where
    m :: m k v
    m = M.fromList kvs

prop_intersection_identity_left
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_intersection_identity_left kvs =
    M.intersection m M.empty === M.empty
  where
    m :: m k v
    m = M.fromList kvs

prop_intersection_identity_right
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> Property
prop_intersection_identity_right kvs =
    M.intersection M.empty m === M.empty
  where
    m :: m k v
    m = M.fromList kvs

prop_intersection_commutativity
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_intersection_commutativity kvs1 kvs2 =
    M.intersection m1 m2 === M.intersection m2 m1
    & cover 1
        (M.nonNull (M.intersection m1 m2))
        "M.nonNull (M.intersection m1 m2)"
    & cover 1
        (M.nonNull (M.intersection m2 m1))
        "M.nonNull (M.intersection m2 m1)"
  where
    m1, m2 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2

prop_intersection_associativity
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_intersection_associativity kvs1 kvs2 kvs3 =
    M.intersection m1 (M.intersection m2 m3)
        === M.intersection (M.intersection m1 m2) m3
    & cover 1
        (M.nonNull (M.intersection m1 (M.intersection m2 m3)))
        "M.nonNull (M.intersection m1 (M.intersection m2 m3))"
    & cover 1
        (M.nonNull (M.intersection (M.intersection m1 m2) m3))
        "M.nonNull (M.intersection (M.intersection m1 m2) m3)"
  where
    m1, m2, m3 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2
    m3 = M.fromList kvs3

prop_intersection_containment_left
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_intersection_containment_left kvs1 kvs2 = QC.property $
    M.intersection m1 m2 `M.isSubmapOf` m1
    & cover 1
        (M.nonNull (M.intersection m1 m2))
        "M.nonNull (M.intersection m1 m2)"
  where
    m1, m2 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2

prop_intersection_containment_right
    :: forall m k v. Test m k v
    => [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_intersection_containment_right kvs1 kvs2 = QC.property $
    M.intersection m1 m2 `M.isSubmapOf` m2
    & cover 1
        (M.nonNull (M.intersection m1 m2))
        "M.nonNull (M.intersection m1 m2)"
  where
    m1, m2 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2

prop_intersection_distributivity
    :: forall m k v. Test m k v
    => k
    -> [(k, Set v)]
    -> [(k, Set v)]
    -> Property
prop_intersection_distributivity k kvs1 kvs2 =
    M.lookup k (M.intersection m1 m2)
        === Set.intersection (M.lookup k m1) (M.lookup k m2)
    & cover 1
        (M.nonNullKey k (M.intersection m1 m2))
        "M.nonNullKey k (M.intersection m1 m2)"
  where
    m1, m2 :: m k v
    m1 = M.fromList kvs1
    m2 = M.fromList kvs2

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

infixr 3 ==>
(==>) :: Bool -> Bool -> Property
a ==> b = not a .||. b

_preventRedundantImportErrors :: ()
_preventRedundantImportErrors = ()
  where
    _multiMap1 :: MultiMap1 () ()
    _multiMap1 = M.empty
