{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.ComparisonSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Monoid.Cancellative
    ( Reductive (..) )
import Data.Monoid.GCD
    ( GCDMonoid )
import Data.MonoidMap
    ( MonoidMap )
import Data.Proxy
    ( Proxy (..) )
import Test.Common
    ( Key
    , Test
    , TestValueType (TestValueType)
    , makeSpec
    , property
    , testValueTypesGCDMonoid
    , testValueTypesAll
    , testValueTypesOrd
    , testValueTypesOrdLeftInvariant
    , testValueTypesOrdRightInvariant
    , testValueTypesReductive
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..)
    , Property
    , applyFun2
    , conjoin
    , cover
    , expectFailure
    , (.||.)
    , (=/=)
    , (===)
    )

import qualified Data.Monoid.GCD as GCDMonoid
    ( GCDMonoid (..) )
import qualified Data.Monoid.Null as Null
    ( MonoidNull (..) )
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Comparison" $ do

    forM_ testValueTypesGCDMonoid $
        \(TestValueType p) -> specGCDMonoid
            (Proxy @Key) p

    forM_ testValueTypesReductive $
        \(TestValueType p) -> specReductive
            (Proxy @Key) p

    forM_ testValueTypesAll $
        \(TestValueType p) -> specMonoidNull
            (Proxy @Key) p

    forM_ testValueTypesOrd $
        \(TestValueType p) -> specOrd
            (Proxy @Key) p

    forM_ testValueTypesOrdLeftInvariant $
        \(TestValueType p) -> specOrdLeftInvariant
            (Proxy @Key) p

    forM_ testValueTypesOrdRightInvariant $
        \(TestValueType p) -> specOrdRightInvariant
            (Proxy @Key) p

specGCDMonoid
    :: forall k v. (Test k v, GCDMonoid v) => Proxy k -> Proxy v -> Spec
specGCDMonoid = makeSpec $ do
    it "prop_disjoint_gcd" $
        prop_disjoint_gcd
            @k @v & property
    it "prop_disjoint_intersection" $
        prop_disjoint_intersection
            @k @v & property

specReductive
    :: forall k v. (Test k v, Reductive v) => Proxy k -> Proxy v -> Spec
specReductive = makeSpec $ do
    it "prop_isSubmapOf_minusMaybe" $
        prop_isSubmapOf_minusMaybe
            @k @v & property
    it "prop_isSubmapOf_reduce" $
        prop_isSubmapOf_reduce
            @k @v & property

specMonoidNull
    :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specMonoidNull = makeSpec $ do
    it "prop_disjointBy_get_total" $
        prop_disjointBy_get_total
            @k @v & property
    it "prop_disjointBy_get_total_failure" $
        prop_disjointBy_get_total_failure
            @k @v & property
    it "prop_isSubmapOfBy_get_total" $
        prop_isSubmapOfBy_get_total
            @k @v & property
    it "prop_isSubmapOfBy_get_total_failure" $
        prop_isSubmapOfBy_get_total_failure
            @k @v & property

specOrd
    :: forall k v. (Test k v, Ord v) => Proxy k -> Proxy v -> Spec
specOrd = makeSpec $ do
    it "prop_compare_filterKeys" $
        prop_compare_filterKeys
            @k @v & property
    it "prop_compare_get" $
        prop_compare_get
            @k @v & property
    it "prop_compare_singleton" $
        prop_compare_singleton
            @k @v & property
    it "prop_compare_unionWith_max" $
        prop_compare_unionWith_max
            @k @v & property
    it "prop_compare_unionWith_min" $
        prop_compare_unionWith_min
            @k @v & property

specOrdLeftInvariant
    :: forall k v. (Test k v, Ord v) => Proxy k -> Proxy v -> Spec
specOrdLeftInvariant = makeSpec $ do
    it "prop_compare_append_left" $
        prop_compare_append_left
            @k @v & property

specOrdRightInvariant
    :: forall k v. (Test k v, Ord v) => Proxy k -> Proxy v -> Spec
specOrdRightInvariant = makeSpec $ do
    it "prop_compare_append_right" $
        prop_compare_append_right
            @k @v & property

prop_compare_append_left
    :: (Test k v, Ord v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_compare_append_left m m1 m2 =
    MonoidMap.compare (m <> m1) (m <> m2)
        ===
        result
    & cover 8
        (result == LT)
        "result == LT"
    & cover 2
        (result == EQ)
        "result == EQ"
    & cover 8
        (result == GT)
        "result == GT"
    & cover 8
        (MonoidMap.nonNull m)
        "MonoidMap.nonNull m"
  where
    result = MonoidMap.compare m1 m2

prop_compare_append_right
    :: (Test k v, Ord v)
    => MonoidMap k v
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_compare_append_right m m1 m2 =
    MonoidMap.compare (m1 <> m) (m2 <> m)
        ===
        result
    & cover 8
        (result == LT)
        "result == LT"
    & cover 2
        (result == EQ)
        "result == EQ"
    & cover 8
        (result == GT)
        "result == GT"
    & cover 8
        (MonoidMap.nonNull m)
        "MonoidMap.nonNull m"
  where
    result = MonoidMap.compare m1 m2

prop_compare_filterKeys
    :: (Test k v, Ord v)
    => k
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_compare_filterKeys k m1 m2 =
    MonoidMap.compare m1 m2
        ===
        MonoidMap.compare m1LT m2LT <> MonoidMap.compare m1GE m2GE
    & cover 8
        (m1LT /= m2LT)
        "m1LT /= m2LT"
    & cover 8
        (m1LT == m2LT && m1GE /= m2GE)
        "m1LT == m2LT && m1GE /= m2GE"
  where
    m1LT = MonoidMap.filterKeys (<  k) m1
    m1GE = MonoidMap.filterKeys (>= k) m1
    m2LT = MonoidMap.filterKeys (<  k) m2
    m2GE = MonoidMap.filterKeys (>= k) m2

prop_compare_get
    :: (Test k v, Ord v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_compare_get m1 m2 =
    result
        ===
        foldMap
            (\k -> compare (MonoidMap.get k m1) (MonoidMap.get k m2))
            (MonoidMap.nonNullKeys m1 <> MonoidMap.nonNullKeys m2)
    & cover 4
        (result == LT)
        "result == LT"
    & cover 1
        (result == EQ)
        "result == EQ"
    & cover 4
        (result == GT)
        "result == GT"
    & cover 4
        (decidingKeyNullInL && decidingKeyNonNullInR)
        "decidingKeyNullInL && decidingKeyNonNullInR"
    & cover 4
        (decidingKeyNonNullInL && decidingKeyNullInR)
        "decidingKeyNonNullInL && decidingKeyNullInR"
    & cover 4
        (decidingKeyNonNullInL && decidingKeyNonNullInR)
        "decidingKeyNonNullInL && decidingKeyNonNullInR"
  where
    result = MonoidMap.compare m1 m2
    -- The smallest key for which the two maps have different values:
    decidingKey =
        Set.lookupMin
            $ Set.filter (\k -> MonoidMap.get k m1 /= MonoidMap.get k m2)
            $ MonoidMap.nonNullKeys m1 <> MonoidMap.nonNullKeys m2
    decidingKeyNullInL    = any (`MonoidMap.nullKey`    m1) decidingKey
    decidingKeyNullInR    = any (`MonoidMap.nullKey`    m2) decidingKey
    decidingKeyNonNullInL = any (`MonoidMap.nonNullKey` m1) decidingKey
    decidingKeyNonNullInR = any (`MonoidMap.nonNullKey` m2) decidingKey

prop_compare_singleton
    :: (Test k v, Ord v)
    => k
    -> v
    -> v
    -> Property
prop_compare_singleton k v1 v2 =
    MonoidMap.compare (MonoidMap.singleton k v1) (MonoidMap.singleton k v2)
        ===
        compare v1 v2
    & cover 1
        (v1 == mempty && v2 /= mempty)
        "v1 == mempty && v2 /= mempty"
    & cover 1
        (v1 /= mempty && v2 == mempty)
        "v1 /= mempty && v2 == mempty"
    & cover 1
        (v1 /= mempty && v2 /= mempty)
        "v1 /= mempty && v2 /= mempty"

-- If every value in map 'a' is less than or equal to the matching value in
-- map 'b', then 'a' must not be greater than 'b'.
--
-- Every such pair of maps (a, b) can be expressed as (m1, unionWith max m1 m2)
-- for some pair of maps (m1, m2), so this property covers all such pairs.
--
prop_compare_unionWith_max
    :: (Test k v, Ord v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_compare_unionWith_max m1 m2 =
    conjoin
        [ MonoidMap.compare m1 m =/= GT
        , MonoidMap.compare m2 m =/= GT
        ]
    & cover 8
        (m /= m1 && m /= m2)
        "m /= m1 && m /= m2"
    & cover 2
        (m == m1 && m /= m2)
        "m == m1 && m /= m2"
    & cover 2
        (m /= m1 && m == m2)
        "m /= m1 && m == m2"
  where
    m = MonoidMap.unionWith max m1 m2

-- If every value in map 'a' is less than or equal to the matching value in
-- map 'b', then 'a' must not be greater than 'b'.
--
-- Every such pair of maps (a, b) can be expressed as (unionWith min m1 m2, m1)
-- for some pair of maps (m1, m2), so this property covers all such pairs.
--
prop_compare_unionWith_min
    :: (Test k v, Ord v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_compare_unionWith_min m1 m2 =
    conjoin
        [ MonoidMap.compare m m1 =/= GT
        , MonoidMap.compare m m2 =/= GT
        ]
    & cover 8
        (m /= m1 && m /= m2)
        "m /= m1 && m /= m2"
    & cover 2
        (m == m1 && m /= m2)
        "m == m1 && m /= m2"
    & cover 2
        (m /= m1 && m == m2)
        "m /= m1 && m == m2"
  where
    m = MonoidMap.unionWith min m1 m2

prop_disjoint_gcd
    :: (Test k v, GCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_disjoint_gcd m1 m2 k =
    MonoidMap.disjoint m1 m2 ==>
        (Null.null (GCDMonoid.gcd (MonoidMap.get k m1) (MonoidMap.get k m2)))
    & cover 8
        (MonoidMap.disjoint m1 m2)
        "MonoidMap.disjoint m1 m2"
    & cover 8
        (not (MonoidMap.disjoint m1 m2))
        "not (MonoidMap.disjoint m1 m2)"

prop_disjoint_intersection
    :: (Test k v, GCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_disjoint_intersection m1 m2 =
    MonoidMap.disjoint m1 m2 === (MonoidMap.intersection m1 m2 == mempty)
    & cover 8
        (MonoidMap.disjoint m1 m2)
        "MonoidMap.disjoint m1 m2"
    & cover 8
        (not (MonoidMap.disjoint m1 m2))
        "not (MonoidMap.disjoint m1 m2)"

prop_disjointBy_get_total
    :: Test k v
    => Fun (v, v) Bool
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_disjointBy_get_total (applyFun2 -> f0) m1 m2 k =
    MonoidMap.disjointBy f m1 m2
        ==>
        f (MonoidMap.get k m1) (MonoidMap.get k m2)
    & cover 8
        (m1 /= mempty && m2 /= mempty && MonoidMap.disjointBy f m1 m2)
        "m1 /= mempty && m2 /= mempty && MonoidMap.disjointBy f m1 m2"
    & cover 2
        (keyWithinIntersection)
        "keyWithinIntersection"
    & cover 2
        (not keyWithinIntersection)
        "not keyWithinIntersection"
  where
    keyWithinIntersection =
        k `Set.member` Set.intersection
            (MonoidMap.nonNullKeys m1)
            (MonoidMap.nonNullKeys m2)
    f v1 v2
        | Null.null v1 = True
        | Null.null v2 = True
        | otherwise = f0 v1 v2

prop_disjointBy_get_total_failure
    :: Test k v
    => Fun (v, v) Bool
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_disjointBy_get_total_failure (applyFun2 -> f) m1 m2 k =
    expectFailure $
    MonoidMap.disjointBy f m1 m2
        ==>
        f (MonoidMap.get k m1) (MonoidMap.get k m2)

prop_isSubmapOf_minusMaybe
    :: (Test k v, Reductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_isSubmapOf_minusMaybe m1 m2 =
    MonoidMap.isSubmapOf m1 m2
        ==> isJust (m2 `MonoidMap.minusMaybe` m1)
    & cover 0.01
        (nonTrivialSubmap)
        "nonTrivialSubmap"
  where
    nonTrivialSubmap =
        MonoidMap.isSubmapOf m1 m2
        && m1 /= mempty
        && m2 /= mempty
        && m1 /= m2

prop_isSubmapOf_reduce
    :: (Test k v, Reductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_isSubmapOf_reduce m1 m2 k =
    MonoidMap.isSubmapOf m1 m2
        ==> isJust (MonoidMap.get k m2 </> MonoidMap.get k m1)
    & cover 0.001
        (nonTrivialSubmap && nonNullKeyL && nonNullKeyR)
        "nonTrivialSubmap && nonNullKeyL && nonNullKeyR"
    & cover 0.001
        (nonTrivialSubmap && nullKeyL && nonNullKeyR)
        "nonTrivialSubmap && nullKeyL && nonNullKeyR"
    & cover 0.001
        (nonTrivialSubmap && nullKeyL && nullKeyR)
        "nonTrivialSubmap && nullKeyL && nullKeyR"
  where
    nonTrivialSubmap =
        MonoidMap.isSubmapOf m1 m2
        && m1 /= mempty
        && m2 /= mempty
        && m1 /= m2
    nonNullKeyL = MonoidMap.nonNullKey k m1
    nonNullKeyR = MonoidMap.nonNullKey k m2
    nullKeyL = MonoidMap.nullKey k m1
    nullKeyR = MonoidMap.nullKey k m2

prop_isSubmapOfBy_get_total
    :: Test k v
    => Fun (v, v) Bool
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_isSubmapOfBy_get_total (applyFun2 -> f0) m1 m2 k =
    MonoidMap.isSubmapOfBy f m1 m2
        ==>
        f (MonoidMap.get k m1) (MonoidMap.get k m2)
    & cover 0.01
        (nonTrivialSubmap && nonNullKeyL && nonNullKeyR)
        "nonTrivialSubmap && nonNullKeyL && nonNullKeyR"
    & cover 0.1
        (nonTrivialSubmap && nullKeyL && nonNullKeyR)
        "nonTrivialSubmap && nullKeyL && nonNullKeyR"
    & cover 0.1
        (nonTrivialSubmap && nonNullKeyL && nullKeyR)
        "nonTrivialSubmap && nonNullKeyL && nullKeyR"
    & cover 0.1
        (nonTrivialSubmap && nullKeyL && nullKeyR)
        "nonTrivialSubmap && nullKeyL && nullKeyR"
  where
    f v1 v2
        | Null.null v1 = True
        | otherwise = f0 v1 v2
    nonTrivialSubmap =
        MonoidMap.isSubmapOfBy f m1 m2
        && m1 /= mempty
        && m2 /= mempty
        && m1 /= m2
    nonNullKeyL = MonoidMap.nonNullKey k m1
    nonNullKeyR = MonoidMap.nonNullKey k m2
    nullKeyL = MonoidMap.nullKey k m1
    nullKeyR = MonoidMap.nullKey k m2

prop_isSubmapOfBy_get_total_failure
    :: Test k v
    => Fun (v, v) Bool
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_isSubmapOfBy_get_total_failure (applyFun2 -> f) m1 m2 k =
    expectFailure $
    MonoidMap.isSubmapOfBy f m1 m2
        ==>
        f (MonoidMap.get k m1) (MonoidMap.get k m2)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

infixr 3 ==>
(==>) :: Bool -> Bool -> Property
a ==> b = not a .||. b
