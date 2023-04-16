{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.PrefixSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( LeftReductive (..) )
import Data.Total.MonoidMap
    ( MonoidMap )
import Test.Common
    ( Key
    , Test
    , TestInstance (TestInstance)
    , makeSpec
    , property
    , testInstancesLeftReductive
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, cover, (===) )

import qualified Data.Total.MonoidMap as MonoidMap
import qualified Test.QuickCheck as QC

spec :: Spec
spec = describe "Prefixes" $ do

    forM_ testInstancesLeftReductive $
        \(TestInstance p) -> specLeftReductive (Proxy @Key) p

specLeftReductive
    :: forall k v. (Test k v, LeftReductive v) => Proxy k -> Proxy v -> Spec
specLeftReductive = makeSpec $ do
    it "prop_stripPrefix_isJust" $
        prop_stripPrefix_isJust
            @k @v & property
    it "prop_stripPrefix_get" $
        prop_stripPrefix_get
            @k @v & property
    it "prop_stripPrefix_mappend" $
        prop_stripPrefix_mappend
            @k @v & property

prop_stripPrefix_isJust
    :: (Test k v, LeftReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_stripPrefix_isJust m1 m2 =
    isJust (stripPrefix m1 m2) === m1 `isPrefixOf` m2
    & cover 1
        (m1 `isPrefixOf` m2)
        "m1 `isPrefixOf` m2"

prop_stripPrefix_get
    :: (Test k v, LeftReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_stripPrefix_get m1 m2 k = QC.property $
    all
        (\r ->
            Just (MonoidMap.get k r)
            ==
            stripPrefix (MonoidMap.get k m1) (MonoidMap.get k m2)
        )
        (stripPrefix m1 m2)
    & cover 1
        (isJust (stripPrefix m1 m2))
        "isJust (stripPrefix m1 m2)"

prop_stripPrefix_mappend
    :: (Test k v, LeftReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_stripPrefix_mappend m1 m2 = QC.property $
    all
        (\r -> m1 <> r == m2)
        (stripPrefix m1 m2)
    & cover 1
        (isJust (stripPrefix m1 m2))
        "isJust (stripPrefix m1 m2)"
