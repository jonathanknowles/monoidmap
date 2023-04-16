{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.UnionSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Total.MonoidMap
    ( MonoidMap )
import Data.Typeable
    ( typeRep )
import Test.Common
    ( Key
    , TestConstraints
    , TestInstance (TestInstance)
    , property
    , testInstancesMonoidNull
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..), Property, applyFun2, cover, expectFailure, (===) )

import qualified Data.Monoid.Null as Null
import qualified Data.Set as Set
import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Union" $ do

    forM_ testInstancesMonoidNull $ \(TestInstance p) -> specFor (Proxy @Key) p

specFor :: forall k v. TestConstraints k v => Proxy k -> Proxy v -> Spec
specFor _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do

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

prop_unionWith_get
    :: TestConstraints k v
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
    :: TestConstraints k v
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
    :: TestConstraints k v
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
    :: TestConstraints k v
    => Fun (v, v) v
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_unionWith_unionWithA (applyFun2 -> f) m1 m2 =
    runIdentity (MonoidMap.unionWithA ((fmap . fmap) Identity f) m1 m2)
    ===         (MonoidMap.unionWith                          f  m1 m2)
