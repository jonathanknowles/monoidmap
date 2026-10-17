{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.SuffixSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.MonoidMap.Internal
    ( MonoidMap )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( RightReductive (..) )
import Test.Common
    ( Key
    , Test
    , TestValueType (TestValueType)
    , makeSpec
    , property
    , testValueTypesRightReductive
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, cover, (===) )

import qualified Test.QuickCheck as QC

spec :: Spec
spec = describe "Suffixes" $ do

    forM_ testValueTypesRightReductive $
        \(TestValueType p) -> specFor (Proxy @Key) p

specFor
    :: forall k v. (Test k v, RightReductive v) => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do
    it "prop_stripSuffix_isJust" $
        prop_stripSuffix_isJust
            @k @v & property
    it "prop_stripSuffix_mappend" $
        prop_stripSuffix_mappend
            @k @v & property

prop_stripSuffix_isJust
    :: (Test k v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_stripSuffix_isJust m1 m2 =
    isJust (stripSuffix m1 m2) === m1 `isSuffixOf` m2
    & cover 1
        (m1 `isSuffixOf` m2)
        "m1 `isSuffixOf` m2"

prop_stripSuffix_mappend
    :: (Test k v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_stripSuffix_mappend m1 m2 = QC.property $
    all
        (\r -> r <> m1 == m2)
        (stripSuffix m1 m2)
    & cover 1
        (isJust (stripSuffix m1 m2))
        "isJust (stripSuffix m1 m2)"
