{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.MapSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Bifunctor
    ( first, second )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Total.MonoidMap
    ( MonoidMap, nonNullCount )
import Test.Common
    ( Key
    , Test
    , TestType (TestType)
    , describeType
    , property
    , testTypesMonoidNull
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..), Property, applyFun, applyFun2, cover, expectFailure, (===) )

import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Mapping" $ do

    forM_ testTypesMonoidNull $
        \(TestType v) -> specFor (Proxy @Key) v

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = describeType $ do

    it "prop_map_asList" $
        prop_map_asList
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
prop_map_get_total (applyFun -> g) k m =
    MonoidMap.get k (MonoidMap.map f m) === f (MonoidMap.get k m)
    & cover 2
        (MonoidMap.nullKey k m)
        "MonoidMap.nullKey k m"
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
  where
    -- A function that preserves null values:
    f :: v -> v
    f v
        | v == mempty = mempty
        | otherwise   = g v

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
