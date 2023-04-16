{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.SuffixSpec
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
    ( RightReductive (..) )
import Data.Total.MonoidMap
    ( MonoidMap )
import Test.Common
    ( Key
    , Test
    , TestInstance (TestInstance)
    , makeSpec
    , property
    , testInstancesRightReductive
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, cover, (===) )

import qualified Data.Total.MonoidMap as MonoidMap
import qualified Test.QuickCheck as QC

spec :: Spec
spec = describe "Suffixes" $ do

    forM_ testInstancesRightReductive $
        \(TestInstance p) -> specRightReductive (Proxy @Key) p

specRightReductive
    :: forall k v. (Test k v, RightReductive v) => Proxy k -> Proxy v -> Spec
specRightReductive = makeSpec $ do
    it "prop_stripSuffix_isJust" $
        prop_stripSuffix_isJust
            @k @v & property
    it "prop_stripSuffix_get" $
        prop_stripSuffix_get
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

prop_stripSuffix_get
    :: (Test k v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_stripSuffix_get m1 m2 k = QC.property $
    all
        (\r ->
            Just (MonoidMap.get k r)
            ==
            stripSuffix (MonoidMap.get k m1) (MonoidMap.get k m2)
        )
        (stripSuffix m1 m2)
    & cover 1
        (isJust (stripSuffix m1 m2))
        "isJust (stripSuffix m1 m2)"

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
