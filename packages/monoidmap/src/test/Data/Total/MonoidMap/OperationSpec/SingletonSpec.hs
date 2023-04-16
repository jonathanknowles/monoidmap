{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.SingletonSpec
    ( spec
    ) where

import Prelude

import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Total.MonoidMap
    ( MonoidMap, nonNullCount )
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
    ( Property, cover, (===) )

import Control.Monad
    ( forM_ )
import qualified Data.Set as Set
import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Singletons" $ do

    forM_ testInstancesMonoidNull $ \(TestInstance p) -> specFor (Proxy @Key) p

specFor :: forall k v. TestConstraints k v => Proxy k -> Proxy v -> Spec
specFor _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do

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
    :: TestConstraints k v => k -> v -> Property
prop_singleton_get k v =
    MonoidMap.get k (MonoidMap.singleton k v) === v
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_nonNullKey
    :: TestConstraints k v => k -> v -> Property
prop_singleton_nonNullKey k v =
    MonoidMap.nonNullKey k (MonoidMap.singleton k v) === (v /= mempty)
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_nonNullKeys
    :: TestConstraints k v => k -> v -> Property
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
    :: TestConstraints k v => k -> v -> Property
prop_singleton_null k v =
    MonoidMap.null (MonoidMap.singleton k v) === (v == mempty)
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_nullify
    :: TestConstraints k v => k -> v -> Property
prop_singleton_nullify k v =
    MonoidMap.nullify k (MonoidMap.singleton k v) === mempty
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_singleton_nonNullCount
    :: TestConstraints k v => k -> v -> Property
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
    :: TestConstraints k v => k -> v -> Property
prop_singleton_toList k v =
    MonoidMap.toList (MonoidMap.singleton k v) ===
        [(k, v) | v /= mempty]
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"
