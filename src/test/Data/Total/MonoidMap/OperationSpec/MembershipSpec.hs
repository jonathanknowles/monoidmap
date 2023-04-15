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

import Data.Function
    ( (&) )
import Data.Monoid
    ( Dual, Sum (..) )
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

import qualified Data.Set as Set
import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Membership" $ do

    specFor (Proxy @Key) (Proxy @(Set Int))
    specFor (Proxy @Key) (Proxy @(Set Natural))
    specFor (Proxy @Key) (Proxy @(Sum Int))
    specFor (Proxy @Key) (Proxy @(Sum Natural))
    specFor (Proxy @Key) (Proxy @[Int])
    specFor (Proxy @Key) (Proxy @[Natural])
    specFor (Proxy @Key) (Proxy @(Text))
    specFor (Proxy @Key) (Proxy @(Dual [Int]))
    specFor (Proxy @Key) (Proxy @(Dual [Natural]))
    specFor (Proxy @Key) (Proxy @(Dual Text))

specFor
    :: forall k v. TestConstraints k v
    => Proxy k
    -> Proxy v
    -> Spec
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
    :: (Ord k, Eq v, Monoid v, Show v)
    => MonoidMap k v
    -> k
    -> Property
prop_nullify_get m k =
    MonoidMap.get k (MonoidMap.nullify k m) === mempty
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 2
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"

prop_nullify_nonNullKey
    :: Ord k
    => MonoidMap k v
    -> k
    -> Property
prop_nullify_nonNullKey m k =
    MonoidMap.nonNullKey k (MonoidMap.nullify k m) === False
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 2
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"

prop_nullify_nonNullKeys
    :: Ord k
    => MonoidMap k v
    -> k
    -> Property
prop_nullify_nonNullKeys m k =
    Set.member k (MonoidMap.nonNullKeys (MonoidMap.nullify k m)) === False
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"
    & cover 2
        (not (MonoidMap.nonNullKey k m))
        "not (MonoidMap.nonNullKey k m)"

prop_nonNullKeys_get
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => MonoidMap k v
    -> Property
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
