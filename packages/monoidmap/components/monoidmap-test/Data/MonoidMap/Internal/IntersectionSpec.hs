{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.IntersectionSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Monoid.Cancellative
    ( GCDMonoid )
import Data.MonoidMap.Internal
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
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..), Property, applyFun2, conjoin, cover, expectFailure, (===) )

import qualified Data.Monoid.Null as Null
import qualified Data.MonoidMap.Internal as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Intersection" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specMonoidNull
            (Proxy @Key) p
    forM_ testValueTypesGCDMonoid $
        \(TestValueType p) -> specGCDMonoid
            (Proxy @Key) p

specMonoidNull :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specMonoidNull = makeSpec $ do
    it "prop_intersectionWith_get" $
        prop_intersectionWith_get
            @k @v & property
    it "prop_intersectionWith_get_total" $
        prop_intersectionWith_get_total
            @k @v & property
    it "prop_intersectionWith_get_total_failure" $
        prop_intersectionWith_get_total_failure
            @k @v & property
    it "prop_intersectionWith_intersectionWithA" $
        prop_intersectionWith_intersectionWithA
            @k @v & property

specGCDMonoid
    :: forall k v. (Test k v, GCDMonoid v) => Proxy k -> Proxy v -> Spec
specGCDMonoid = makeSpec $ do
    it "prop_intersection_isSubmapOf" $
        prop_intersection_isSubmapOf
            @k @v & property

prop_intersection_isSubmapOf
    :: (Test k v, GCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_intersection_isSubmapOf m1 m2 = conjoin
    [ intersection_m1_m2 `MonoidMap.isSubmapOf` m1
    , intersection_m1_m2 `MonoidMap.isSubmapOf` m2
    ]
    & cover 2
        (m1 /= m2 && MonoidMap.nonNull (intersection_m1_m2))
        "m1 /= m2 && MonoidMap.nonNull (intersection_m1_m2)"
  where
    intersection_m1_m2 = MonoidMap.intersection m1 m2

prop_intersectionWith_get
    :: Test k v
    => Fun (v, v) v
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_intersectionWith_get (applyFun2 -> f) m1 m2 k =
    (MonoidMap.get k result
        ===
        if keyWithinIntersection
        then f (MonoidMap.get k m1) (MonoidMap.get k m2)
        else mempty)
    & cover 2
        (keyWithinIntersection)
        "keyWithinIntersection"
    & cover 2
        (not keyWithinIntersection)
        "not keyWithinIntersection"
    & cover 2
        (MonoidMap.null result)
        "MonoidMap.null result"
    & cover 2
        (MonoidMap.nonNull result)
        "MonoidMap.nonNull result"
    & cover 2
        (MonoidMap.nullKey k result)
        "MonoidMap.nullKey k result"
    & cover 2
        (MonoidMap.nonNullKey k result)
        "MonoidMap.nonNullKey k result"
  where
    keyWithinIntersection =
        k `Set.member` Set.intersection
            (MonoidMap.nonNullKeys m1)
            (MonoidMap.nonNullKeys m2)
    result =
        MonoidMap.intersectionWith f m1 m2

prop_intersectionWith_get_total
    :: Test k v
    => Fun (v, v) v
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_intersectionWith_get_total (applyFun2 -> f0) m1 m2 k =
    (MonoidMap.get k result
        ===
        f (MonoidMap.get k m1) (MonoidMap.get k m2))
    & cover 2
        (keyWithinIntersection)
        "keyWithinIntersection"
    & cover 2
        (not keyWithinIntersection)
        "not keyWithinIntersection"
    & cover 2
        (MonoidMap.null result)
        "MonoidMap.null result"
    & cover 2
        (MonoidMap.nonNull result)
        "MonoidMap.nonNull result"
    & cover 2
        (MonoidMap.nullKey k result)
        "MonoidMap.nullKey k result"
    & cover 2
        (MonoidMap.nonNullKey k result)
        "MonoidMap.nonNullKey k result"
  where
    result =
        MonoidMap.intersectionWith f m1 m2
    keyWithinIntersection =
        k `Set.member` Set.intersection
            (MonoidMap.nonNullKeys m1)
            (MonoidMap.nonNullKeys m2)
    f v1 v2
        | Null.null v1 = mempty
        | Null.null v2 = mempty
        | otherwise = f0 v1 v2

prop_intersectionWith_get_total_failure
    :: Test k v
    => Fun (v, v) v
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_intersectionWith_get_total_failure (applyFun2 -> f) m1 m2 k =
    expectFailure $
    MonoidMap.get k (MonoidMap.intersectionWith f m1 m2)
        ===
        f (MonoidMap.get k m1) (MonoidMap.get k m2)

prop_intersectionWith_intersectionWithA
    :: Test k v
    => Fun (v, v) v
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_intersectionWith_intersectionWithA (applyFun2 -> f) m1 m2 =
    runIdentity (MonoidMap.intersectionWithA ((fmap . fmap) Identity f) m1 m2)
    ===         (MonoidMap.intersectionWith                          f  m1 m2)
