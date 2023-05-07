{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.AccessSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.MonoidMap
    ( MonoidMap )
import Data.Proxy
    ( Proxy (..) )
import Test.Common
    ( Key, Test, TestType (TestType), makeSpec, property, testTypesMonoidNull )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, cover, (===) )

import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Accessors" $ do

    forM_ testTypesMonoidNull $ \(TestType p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    describe "Get" $ do
        it "prop_get_nonNullKey" $
            prop_get_nonNullKey
                @k @v & property
        it "prop_get_nonNullKeys" $
            prop_get_nonNullKeys
                @k @v & property

    describe "Set" $ do
        it "prop_set_get" $
            prop_set_get
                @k @v & property
        it "prop_set_nonNullKey" $
            prop_set_nonNullKey
                @k @v & property
        it "prop_set_nonNullKeys" $
            prop_set_nonNullKeys
                @k @v & property
        it "prop_set_toList" $
            prop_set_toList
                @k @v & property

--------------------------------------------------------------------------------
-- Get
--------------------------------------------------------------------------------

prop_get_nonNullKey
    :: Test k v => MonoidMap k v -> k -> Property
prop_get_nonNullKey m k =
    MonoidMap.nonNullKey k m === (MonoidMap.get k m /= mempty)
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 2
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"

prop_get_nonNullKeys
    :: Test k v => MonoidMap k v -> k -> Property
prop_get_nonNullKeys m k =
    Set.member k (MonoidMap.nonNullKeys m) === (MonoidMap.get k m /= mempty)
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 2
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"

--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

prop_set_get
    :: Test k v => MonoidMap k v -> k -> v -> Property
prop_set_get m k v =
    MonoidMap.get k (MonoidMap.set k v m) === v
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 2
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"

prop_set_nonNullKey
    :: Test k v => MonoidMap k v -> k -> v -> Property
prop_set_nonNullKey m k v =
    MonoidMap.nonNullKey k (MonoidMap.set k v m) ===
        (v /= mempty)
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_set_nonNullKeys
    :: Test k v => MonoidMap k v -> k -> v -> Property
prop_set_nonNullKeys m k v =
    Set.member k (MonoidMap.nonNullKeys (MonoidMap.set k v m)) ===
        (v /= mempty)
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"

prop_set_toList
    :: Test k v => MonoidMap k v -> k -> v -> Property
prop_set_toList m k v =
    filter ((== k) . fst) (MonoidMap.toList (MonoidMap.set k v m)) ===
        [(k, v) | v /= mempty]
    & cover 2
        (v == mempty)
        "v == mempty"
    & cover 2
        (v /= mempty)
        "v /= mempty"
