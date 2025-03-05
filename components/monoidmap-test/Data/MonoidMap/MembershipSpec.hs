{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.MembershipSpec
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
import Data.Set
    ( Set )
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

import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Membership" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    it "prop_nonNullKeys_get" $
        prop_nonNullKeys_get
            @k @v & property
    it "prop_nullify_get" $
        prop_nullify_get
            @k @v & property
    it "prop_nullify_nonNullKey" $
        prop_nullify_nonNullKey
            @k @v & property
    it "prop_nullify_nonNullKeys" $
        prop_nullify_nonNullKeys
            @k @v & property
    it "prop_nullifyKeysIn_get" $
        prop_nullifyKeysIn_get
            @k @v & property
    it "prop_nullifyKeysNotIn_get" $
        prop_nullifyKeysNotIn_get
            @k @v & property

prop_nonNullKeys_get
    :: Test k v => MonoidMap k v -> Property
prop_nonNullKeys_get m =
    fmap
        (\k -> (k, MonoidMap.get k m))
        (Set.toList (MonoidMap.nonNullKeys m))
        === MonoidMap.toList m
    & cover 2
        (MonoidMap.null m)
        "MonoidMap.null m"
    & cover 2
        (not (MonoidMap.null m))
        "not (MonoidMap.null m)"

prop_nullify_get
    :: Test k v => MonoidMap k v -> k -> Property
prop_nullify_get m k =
    MonoidMap.get k (MonoidMap.nullify k m) === mempty
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 2
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"

prop_nullify_nonNullKey
    :: Test k v => MonoidMap k v -> k -> Property
prop_nullify_nonNullKey m k =
    MonoidMap.nonNullKey k (MonoidMap.nullify k m) === False
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 2
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"

prop_nullify_nonNullKeys
    :: Test k v => MonoidMap k v -> k -> Property
prop_nullify_nonNullKeys m k =
    Set.member k (MonoidMap.nonNullKeys (MonoidMap.nullify k m)) === False
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 2
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"

prop_nullifyKeysIn_get
    :: Test k v => MonoidMap k v -> Set k -> k -> Property
prop_nullifyKeysIn_get m ks k =
    MonoidMap.get k (MonoidMap.nullifyKeysIn ks m) ===
        (if Set.member k ks then mempty else MonoidMap.get k m)
    & cover 2
        (Set.member k ks && MonoidMap.nullKey k m)
        "Set.member k ks && MonoidMap.nullKey k m"
    & cover 2
        (Set.member k ks && MonoidMap.nonNullKey k m)
        "Set.member k ks && MonoidMap.nonNullKey k m"
    & cover 2
        (Set.notMember k ks && MonoidMap.nullKey k m)
        "Set.notMember k ks && MonoidMap.nullKey k m"
    & cover 2
        (Set.notMember k ks && MonoidMap.nonNullKey k m)
        "Set.notMember k ks && MonoidMap.nonNullKey k m"

prop_nullifyKeysNotIn_get
    :: Test k v => MonoidMap k v -> Set k -> k -> Property
prop_nullifyKeysNotIn_get m ks k =
    MonoidMap.get k (MonoidMap.nullifyKeysNotIn ks m) ===
        (if Set.notMember k ks then mempty else MonoidMap.get k m)
    & cover 2
        (Set.member k ks && MonoidMap.nullKey k m)
        "Set.member k ks && MonoidMap.nullKey k m"
    & cover 2
        (Set.member k ks && MonoidMap.nonNullKey k m)
        "Set.member k ks && MonoidMap.nonNullKey k m"
    & cover 2
        (Set.notMember k ks && MonoidMap.nullKey k m)
        "Set.notMember k ks && MonoidMap.nullKey k m"
    & cover 2
        (Set.notMember k ks && MonoidMap.nonNullKey k m)
        "Set.notMember k ks && MonoidMap.nonNullKey k m"
