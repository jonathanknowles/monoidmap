{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.MapSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Bifunctor
    ( first, second )
import Data.Function
    ( (&) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal
    ( MonoidMap, nonNullCount )
import Data.Proxy
    ( Proxy (..) )
import Test.Common
    ( Key
    , Test
    , TestValueType (TestValueType)
    , makeSpec
    , property
    , testValueTypesAll
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..), Property, applyFun, applyFun2, cover, expectFailure, (===) )

import qualified Data.Foldable as F
import qualified Data.Monoid.Null as Null
import qualified Data.MonoidMap.Internal as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Mapping" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    it "prop_map_asList" $
        prop_map_asList
            @k @v & property
    it "prop_map_composition" $
        prop_map_composition
            @k @v & property
    it "prop_map_composition_failure" $
        prop_map_composition_failure
            @k @v & property
    it "prop_map_get" $
        prop_map_get
            @k @v & property
    it "prop_map_get_total" $
        prop_map_get_total
            @k @v & property
    it "prop_map_get_total_failure" $
        prop_map_get_total_failure
            @k @v & property
    it "prop_mapKeys_asList" $
        prop_mapKeys_asList
            @k @v & property
    it "prop_mapKeys_get" $
        prop_mapKeys_get
            @k @v & property
    it "prop_mapKeysWith_asList" $
        prop_mapKeysWith_asList
            @k @v & property
    it "prop_mapWithKey_asList" $
        prop_mapWithKey_asList
            @k @v & property
    it "prop_mapWithKey_get" $
        prop_mapWithKey_get
            @k @v & property
    it "prop_mapWithKey_get_total" $
        prop_mapWithKey_get_total
            @k @v & property
    it "prop_mapWithKey_get_total_failure" $
        prop_mapWithKey_get_total_failure
            @k @v & property

--------------------------------------------------------------------------------
-- Mapping
--------------------------------------------------------------------------------

prop_map_asList
    :: Test k v
    => Fun v v
    -> MonoidMap k v
    -> Property
prop_map_asList (applyFun -> f) m =
    n === (MonoidMap.fromList . fmap (second f) . MonoidMap.toList $ m)
    & cover 2
        (0 < nonNullCount n && nonNullCount n < nonNullCount m)
        "0 < nonNullCount n && nonNullCount n < nonNullCount m"
  where
    n = MonoidMap.map f m

prop_map_composition
    :: forall k v. Test k v
    => Fun v v
    -> Fun v v
    -> MonoidMap k v
    -> Property
prop_map_composition (applyFun -> f0) (applyFun -> g0) m =
    MonoidMap.map (f . g) m === MonoidMap.map f (MonoidMap.map g m)
    & cover 2
        (MonoidMap.nonNull m)
        "MonoidMap.nonNull m"
  where
    f = toNullPreservingFn f0
    g = g0

prop_map_composition_failure
    :: forall k v. Test k v
    => Fun v v
    -> Fun v v
    -> MonoidMap k v
    -> Property
prop_map_composition_failure (applyFun -> f) (applyFun -> g) m =
    expectFailure $
    MonoidMap.map (f . g) m === MonoidMap.map f (MonoidMap.map g m)
    & cover 1
        (MonoidMap.map (f . g) m /= MonoidMap.map f (MonoidMap.map g m))
        "MonoidMap.map (f . g) m /= MonoidMap.map f (MonoidMap.map g m)"

prop_map_get
    :: Test k v
    => Fun v v
    -> k
    -> MonoidMap k v
    -> Property
prop_map_get (applyFun -> f) k m =
    MonoidMap.get k (MonoidMap.map f m)
    ===
    (if MonoidMap.nullKey k m then mempty else f (MonoidMap.get k m))
    & cover 2
        (MonoidMap.nullKey k m)
        "MonoidMap.nullKey k m"
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"

prop_map_get_total
    :: forall k v. Test k v
    => Fun v v
    -> k
    -> MonoidMap k v
    -> Property
prop_map_get_total (applyFun -> f0) k m =
    MonoidMap.get k (MonoidMap.map f m) === f (MonoidMap.get k m)
    & cover 2
        (MonoidMap.nullKey k m)
        "MonoidMap.nullKey k m"
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
  where
    f = toNullPreservingFn f0

prop_map_get_total_failure
    :: Test k v
    => Fun v v
    -> k
    -> MonoidMap k v
    -> Property
prop_map_get_total_failure (applyFun -> f) k m =
    expectFailure $
    MonoidMap.get k (MonoidMap.map f m) === f (MonoidMap.get k m)

prop_mapKeys_asList
    :: Test k v
    => Fun k k
    -> MonoidMap k v
    -> Property
prop_mapKeys_asList (applyFun -> f) m =
    n === (MonoidMap.fromList . fmap (first f) . MonoidMap.toList $ m)
    & cover 2
        (0 < nonNullCount n && nonNullCount n < nonNullCount m)
        "0 < nonNullCount n && nonNullCount n < nonNullCount m"
  where
    n = MonoidMap.mapKeys f m

prop_mapKeys_get
    :: Test k v
    => Fun k k
    -> k
    -> MonoidMap k v
    -> Property
prop_mapKeys_get (applyFun -> f) k m =
    MonoidMap.get k (MonoidMap.mapKeys f m)
        ===
        F.foldMap
            (`MonoidMap.get` m)
            (Set.filter ((==) k . f) (MonoidMap.nonNullKeys m))
    & cover 2
        (MonoidMap.nullKey k (MonoidMap.mapKeys f m))
        "MonoidMap.nullKey k (MonoidMap.mapKeys f m)"
    & cover 2
        (MonoidMap.nonNullKey k (MonoidMap.mapKeys f m))
        "MonoidMap.nonNullKey k (MonoidMap.mapKeys f m)"

prop_mapKeysWith_asList
    :: Test k v
    => Fun (v, v) v
    -> Fun k k
    -> MonoidMap k v
    -> Property
prop_mapKeysWith_asList (applyFun2 -> c) (applyFun -> f) m =
    n === (MonoidMap.fromListWith c . fmap (first f) . MonoidMap.toList $ m)
    & cover 2
        (0 < nonNullCount n && nonNullCount n < nonNullCount m)
        "0 < nonNullCount n && nonNullCount n < nonNullCount m"
  where
    n = MonoidMap.mapKeysWith c f m

prop_mapWithKey_asList
    :: Test k v
    => Fun (k, v) v
    -> MonoidMap k v
    -> Property
prop_mapWithKey_asList (applyFun2 -> f) m =
    n ===
        ( MonoidMap.fromList
        . fmap (\(k, v) -> (k, (f k v)))
        . MonoidMap.toList
        $ m
        )
    & cover 2
        (0 < nonNullCount n && nonNullCount n < nonNullCount m)
        "0 < nonNullCount n && nonNullCount n < nonNullCount m"
  where
    n = MonoidMap.mapWithKey f m

prop_mapWithKey_get
    :: Test k v
    => Fun (k, v) v
    -> k
    -> MonoidMap k v
    -> Property
prop_mapWithKey_get (applyFun2 -> f) k m =
    MonoidMap.get k (MonoidMap.mapWithKey f m)
    ===
    (if MonoidMap.nullKey k m then mempty else f k (MonoidMap.get k m))
    & cover 2
        (MonoidMap.nullKey k m)
        "MonoidMap.nullKey k m"
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"

prop_mapWithKey_get_total
    :: forall k v. Test k v
    => Fun (k, v) v
    -> k
    -> MonoidMap k v
    -> Property
prop_mapWithKey_get_total (applyFun2 -> f0) k m =
    MonoidMap.get k (MonoidMap.mapWithKey f m) === f k (MonoidMap.get k m)
    & cover 2
        (MonoidMap.nullKey k m)
        "MonoidMap.nullKey k m"
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
  where
    f = toNullPreservingFn . f0

prop_mapWithKey_get_total_failure
    :: Test k v
    => Fun (k, v) v
    -> k
    -> MonoidMap k v
    -> Property
prop_mapWithKey_get_total_failure (applyFun2 -> f) k m =
    expectFailure $
    MonoidMap.get k (MonoidMap.mapWithKey f m) === f k (MonoidMap.get k m)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Creates a function that never maps null values to non-null values.
--
toNullPreservingFn :: MonoidNull v => (v -> v) -> (v -> v)
toNullPreservingFn f v
    | Null.null v = v
    | otherwise = f v
