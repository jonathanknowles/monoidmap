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
import Data.Monoid.GCD
    ( RightGCDMonoid (..) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( RightReductive (..) )
import Data.Total.MonoidMap
    ( MonoidMap )
import Data.Typeable
    ( typeRep )
import Test.Common
    ( Key
    , TestConstraints
    , TestInstance (TestInstance)
    , property
    , testInstancesRightGCDMonoid
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
    forM_ testInstancesRightGCDMonoid $
        \(TestInstance p) -> specRightGCDMonoid (Proxy @Key) p

specRightReductive
    :: forall k v. (TestConstraints k v, RightReductive v)
    => Proxy k
    -> Proxy v
    -> Spec
specRightReductive _k _v =
    describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do
        it "prop_stripSuffix_isJust" $
            prop_stripSuffix_isJust
                @k @v & property
        it "prop_stripSuffix_get" $
            prop_stripSuffix_get
                @k @v & property
        it "prop_stripSuffix_mappend" $
            prop_stripSuffix_mappend
                @k @v & property

specRightGCDMonoid
    :: forall k v. (TestConstraints k v, RightGCDMonoid v)
    => Proxy k
    -> Proxy v
    -> Spec
specRightGCDMonoid _k _v =
    describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do
        it "prop_commonSuffix_get" $
            prop_commonSuffix_get
                @k @v & property

prop_stripSuffix_isJust
    :: (Ord k, MonoidNull v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_stripSuffix_isJust m1 m2 =
    isJust (stripSuffix m1 m2) === m1 `isSuffixOf` m2
    & cover 1
        (m1 `isSuffixOf` m2)
        "m1 `isSuffixOf` m2"

prop_stripSuffix_get
    :: (Ord k, Eq v, MonoidNull v, RightReductive v)
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
    :: (Ord k, Eq v, MonoidNull v, RightReductive v)
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

prop_commonSuffix_get
    :: (Ord k, Eq v, Show v, MonoidNull v, RightGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_commonSuffix_get m1 m2 k =
    MonoidMap.get k (commonSuffix m1 m2)
    ===
    commonSuffix (MonoidMap.get k m1) (MonoidMap.get k m2)
    & cover 1
        (MonoidMap.get k (commonSuffix m1 m2) == mempty)
        "MonoidMap.get k (commonSuffix m1 m2) == mempty"
    & cover 0.1
        (MonoidMap.get k (commonSuffix m1 m2) /= mempty)
        "MonoidMap.get k (commonSuffix m1 m2) /= mempty"
