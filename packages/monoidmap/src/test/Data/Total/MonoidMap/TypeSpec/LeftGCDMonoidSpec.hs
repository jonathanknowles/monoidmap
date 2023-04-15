{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.TypeSpec.LeftGCDMonoidSpec
    ( spec
    ) where

import Prelude

import Data.Function
    ( (&) )
import Data.Monoid
    ( Dual, Sum (..) )
import Data.Monoid.GCD
    ( LeftGCDMonoid (..) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Proxy
    ( Proxy (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Total.MonoidMap
    ( MonoidMap )
import Data.Typeable
    ( typeRep )
import Numeric.Natural
    ( Natural )
import Test.Common
    ( Key, TestConstraints, property )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, cover, (===) )

import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Operations requiring a LeftGCDMonoid constraint" $ do

    specFor (Proxy @Key) (Proxy @(Set Int))
    specFor (Proxy @Key) (Proxy @(Set Natural))
    specFor (Proxy @Key) (Proxy @(Sum Natural))
    specFor (Proxy @Key) (Proxy @[Int])
    specFor (Proxy @Key) (Proxy @[Natural])
    specFor (Proxy @Key) (Proxy @(Text))
    specFor (Proxy @Key) (Proxy @(Dual [Int]))
    specFor (Proxy @Key) (Proxy @(Dual [Natural]))
    specFor (Proxy @Key) (Proxy @(Dual Text))

specFor
    :: forall k v. (TestConstraints k v, LeftGCDMonoid v)
    => Proxy k
    -> Proxy v
    -> Spec
specFor _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do

    it "prop_commonPrefix_get" $
        prop_commonPrefix_get
            @k @v & property

prop_commonPrefix_get
    :: (Ord k, Eq v, Show v, MonoidNull v, LeftGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> k
    -> Property
prop_commonPrefix_get m1 m2 k =
    MonoidMap.get k (commonPrefix m1 m2)
    ===
    commonPrefix (MonoidMap.get k m1) (MonoidMap.get k m2)
    & cover 1
        (MonoidMap.get k (commonPrefix m1 m2) == mempty)
        "MonoidMap.get k (commonPrefix m1 m2) == mempty"
    & cover 0.1
        (MonoidMap.get k (commonPrefix m1 m2) /= mempty)
        "MonoidMap.get k (commonPrefix m1 m2) /= mempty"
