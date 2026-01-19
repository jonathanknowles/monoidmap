{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.UnionSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Monoid.LCM
    ( LCMMonoid )
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
    , testValueTypesLCMMonoid
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
spec = describe "Union" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specMonoidNull
            (Proxy @Key) p
    forM_ testValueTypesLCMMonoid $
        \(TestValueType p) -> specLCMMonoid
            (Proxy @Key) p

specMonoidNull :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specMonoidNull = makeSpec $ do
    it "prop_unionWith_get" $
        prop_unionWith_get
            @k @v & property
    it "prop_unionWith_get_total" $
        prop_unionWith_get_total
            @k @v & property
    it "prop_unionWith_get_total_failure" $
        prop_unionWith_get_total_failure
            @k @v & property
    it "prop_unionWith_unionWithA" $
        prop_unionWith_unionWithA
            @k @v & property

specLCMMonoid
    :: forall k v. (Test k v, LCMMonoid v) => Proxy k -> Proxy v -> Spec
specLCMMonoid = makeSpec $ do
    it "prop_union_isSubmapOf" $
        prop_union_isSubmapOf
            @k @v & property

prop_union_isSubmapOf
    :: (Test k v, LCMMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_union_isSubmapOf m1 m2 = conjoin
    [ m1 `MonoidMap.isSubmapOf` union_m1_m2
    , m2 `MonoidMap.isSubmapOf` union_m1_m2
    ]
    & cover 2
        (m1 /= m2 && MonoidMap.nonNull (union_m1_m2))
        "m1 /= m2 && MonoidMap.nonNull (union_m1_m2)"
  where
    union_m1_m2 = MonoidMap.union m1 m2

prop_unionWith_get
    :: Test k v
    => Fun (v, v) v
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_unionWith_get (applyFun2 -> f) m1 m2 k =
    (MonoidMap.get k result
        ===
        if keyWithinUnion
        then f (MonoidMap.get k m1) (MonoidMap.get k m2)
        else mempty)
    & cover 2
        (keyWithinUnion)
        "keyWithinUnion"
    & cover 2
        (not keyWithinUnion)
        "not keyWithinUnion"
    & cover 2
        (MonoidMap.null result)
        "MonoidMap.null result"
    & cover 2
        (MonoidMap.nonNull result)
        "MonoidMap.nonNull result)"
    & cover 2
        (MonoidMap.nullKey k result)
        "MonoidMap.nullKey k result"
    & cover 2
        (MonoidMap.nonNullKey k result)
        "MonoidMap.nonNullKey k result"
  where
    keyWithinUnion =
        k `Set.member` Set.union
            (MonoidMap.nonNullKeys m1)
            (MonoidMap.nonNullKeys m2)
    result =
        MonoidMap.unionWith f m1 m2

prop_unionWith_get_total
    :: Test k v
    => Fun (v, v) v
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_unionWith_get_total (applyFun2 -> f0) m1 m2 k =
    (MonoidMap.get k result
        ===
        f (MonoidMap.get k m1) (MonoidMap.get k m2))
    & cover 2
        (keyWithinUnion)
        "keyWithinUnion"
    & cover 2
        (not keyWithinUnion)
        "not keyWithinUnion"
    & cover 2
        (MonoidMap.null result)
        "MonoidMap.null result"
    & cover 2
        (MonoidMap.nonNull result)
        "MonoidMap.nonNull result)"
    & cover 2
        (MonoidMap.nullKey k result)
        "MonoidMap.nullKey k result"
    & cover 2
        (MonoidMap.nonNullKey k result)
        "MonoidMap.nonNullKey k result"
  where
    keyWithinUnion =
        k `Set.member` Set.union
            (MonoidMap.nonNullKeys m1)
            (MonoidMap.nonNullKeys m2)
    result =
        MonoidMap.unionWith f m1 m2
    f v1 v2
        | Null.null v1 && Null.null v2 = mempty
        | otherwise = f0 v1 v2

prop_unionWith_get_total_failure
    :: Test k v
    => Fun (v, v) v
    -> MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_unionWith_get_total_failure (applyFun2 -> f) m1 m2 k =
    expectFailure $
    MonoidMap.get k (MonoidMap.unionWith f m1 m2)
        ===
        f (MonoidMap.get k m1) (MonoidMap.get k m2)

prop_unionWith_unionWithA
    :: Test k v
    => Fun (v, v) v
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_unionWith_unionWithA (applyFun2 -> f) m1 m2 =
    runIdentity (MonoidMap.unionWithA ((fmap . fmap) Identity f) m1 m2)
    ===         (MonoidMap.unionWith                          f  m1 m2)
