{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.MembershipSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Total.MonoidMap
    ( MonoidMap )
import Data.Typeable
    ( typeRep )
import Test.Common
    ( Key
    , Test
    , TestInstance (TestInstance)
    , property
    , testInstancesMonoidNull
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, cover, (===) )

import qualified Data.Set as Set
import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Membership" $ do

    forM_ testInstancesMonoidNull $ \(TestInstance p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do

    it "prop_nullify_get" $
        prop_nullify_get
            @k @v & property
    it "prop_nullify_nonNullKey" $
        prop_nullify_nonNullKey
            @k @v & property
    it "prop_nullify_nonNullKeys" $
        prop_nullify_nonNullKeys
            @k @v & property
    it "prop_nonNullKeys_get" $
        prop_nonNullKeys_get
            @k @v & property

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
