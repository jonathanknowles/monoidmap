{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.DistributivitySpec
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
    ( MonoidMap, get )
import Test.Common
    ( Key
    , Test
    , TestInstance (..)
    , makeSpec
    , property
    , testInstancesMonoidNull
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, cover, (===) )

spec :: Spec
spec = describe "Distributivity" $ do

    forM_ testInstancesMonoidNull $ \(TestInstance p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    it "prop_distributive_get_mappend" $
        prop_distributive_get_mappend
            @k @v & property

prop_distributive_get
    :: Test k v
    => (MonoidMap k v -> MonoidMap k v -> MonoidMap k v)
    -> (v -> v -> v)
    -> k
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_distributive_get f g k m1 m2 =
    get k (f m1 m2) === g (get k m1) (get k m2)
    & cover 2
        (get k (f m1 m2) == mempty)
        "get k (f m1 m2) == mempty"
    & cover 2
        (get k (f m1 m2) /= mempty)
        "get k (f m1 m2) /= mempty"
    & cover 2
        (get k m1 == mempty && get k m2 == mempty)
        "get k m1 == mempty && get k m2 == mempty"
    & cover 2
        (get k m1 == mempty && get k m2 /= mempty)
        "get k m1 == mempty && get k m2 /= mempty"
    & cover 2
        (get k m1 /= mempty && get k m2 == mempty)
        "get k m1 /= mempty && get k m2 == mempty"
    & cover 2
        (get k m1 /= mempty && get k m2 /= mempty)
        "get k m1 /= mempty && get k m2 /= mempty"

prop_distributive_get_mappend
    :: Test k v => k -> MonoidMap k v -> MonoidMap k v -> Property
prop_distributive_get_mappend = prop_distributive_get mappend mappend
