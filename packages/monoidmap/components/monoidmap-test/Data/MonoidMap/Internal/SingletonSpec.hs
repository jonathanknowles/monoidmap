{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.SingletonSpec
    ( spec
    ) where

import Prelude

import Data.Function
    ( (&) )
import Data.MonoidMap.Internal
    ( nonNullCount )
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
    ( Property, cover, (===) )

import Control.Monad
    ( forM_ )
import qualified Data.MonoidMap.Internal as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Singletons" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    it "prop_singleton_get" $
        prop_singleton_get
            @k @v & property
    it "prop_singleton_nonNullKey" $
        prop_singleton_nonNullKey
            @k @v & property
    it "prop_singleton_nonNullKeys" $
        prop_singleton_nonNullKeys
            @k @v & property
    it "prop_singleton_null" $
        prop_singleton_null
            @k @v & property
    it "prop_singleton_nullify" $
        prop_singleton_nullify
            @k @v & property
    it "prop_singleton_nonNullCount" $
        prop_singleton_nonNullCount
            @k @v & property
    it "prop_singleton_toList" $
        prop_singleton_toList
            @k @v & property

prop_singleton_get
    :: Test k v => k -> v -> Property
prop_singleton_get k v =
    MonoidMap.get k (MonoidMap.singleton k v) === v
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_nonNullKey
    :: Test k v => k -> v -> Property
prop_singleton_nonNullKey k v =
    MonoidMap.nonNullKey k (MonoidMap.singleton k v) === (v /= mempty)
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_nonNullKeys
    :: Test k v => k -> v -> Property
prop_singleton_nonNullKeys k v =
    MonoidMap.nonNullKeys (MonoidMap.singleton k v) ===
        (if v == mempty then Set.empty else Set.singleton k)
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_null
    :: Test k v => k -> v -> Property
prop_singleton_null k v =
    MonoidMap.null (MonoidMap.singleton k v) === (v == mempty)
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_nullify
    :: Test k v => k -> v -> Property
prop_singleton_nullify k v =
    MonoidMap.nullify k (MonoidMap.singleton k v) === mempty
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_nonNullCount
    :: Test k v => k -> v -> Property
prop_singleton_nonNullCount k v =
    nonNullCount (MonoidMap.singleton k v) ===
        (if v == mempty then 0 else 1)
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_toList
    :: Test k v => k -> v -> Property
prop_singleton_toList k v =
    MonoidMap.toList (MonoidMap.singleton k v) ===
        [(k, v) | v /= mempty]
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"
