{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.AccessSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
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
    , testValueTypesAll
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun, Property, applyFun, cover, (===) )

import qualified Data.Monoid.Null as Null
import qualified Data.MonoidMap.Internal as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Accessors" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specFor (Proxy @Key) p

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

    describe "Adjust" $ do
        it "prop_adjust_get_set" $
            prop_adjust_get_set
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

--------------------------------------------------------------------------------
-- Adjust
--------------------------------------------------------------------------------

prop_adjust_get_set
    :: Test k v => MonoidMap k v -> Fun v v -> k -> Property
prop_adjust_get_set m (applyFun -> f) k =
    MonoidMap.adjust f k m === MonoidMap.set k (f (MonoidMap.get k m)) m
    & cover 1
        (MonoidMap.nullKey k m && Null.null (f mempty))
        "MonoidMap.nullKey k m && Null.null (f mempty)"
    & cover 1
        (MonoidMap.nullKey k m && not (Null.null (f mempty)))
        "MonoidMap.nullKey k m && not (Null.null (f mempty))"
    & cover 0.1
        (MonoidMap.nonNullKey k m && Null.null (f (MonoidMap.get k m)))
        "MonoidMap.nonNullKey k m && Null.null (f (MonoidMap.get k m))"
    & cover 0.1
        (MonoidMap.nonNullKey k m && not (Null.null (f (MonoidMap.get k m))))
        "MonoidMap.nonNullKey k m && not (Null.null (f (MonoidMap.get k m)))"
