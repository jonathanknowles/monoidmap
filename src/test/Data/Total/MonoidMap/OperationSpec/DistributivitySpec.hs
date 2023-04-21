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

import Prelude hiding
    ( gcd, lcm )

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Group
    ( Group )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, OverlappingGCDMonoid, RightGCDMonoid )
import Data.Monoid.LCM
    ( LCMMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.Proxy
    ( Proxy (..) )
import Data.Total.MonoidMap
    ( MonoidMap, get )
import Test.Common
    ( Key
    , Test
    , TestType (..)
    , makeSpec
    , property
    , testTypesGCDMonoid
    , testTypesGroup
    , testTypesLCMMonoid
    , testTypesLeftGCDMonoid
    , testTypesMonoidNull
    , testTypesMonus
    , testTypesOverlappingGCDMonoid
    , testTypesRightGCDMonoid
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, cover, (===) )

import qualified Data.Group as Group
    ( Group (..) )
import qualified Data.Monoid.GCD as LeftGCDMonoid
    ( LeftGCDMonoid (..) )
import qualified Data.Monoid.GCD as RightGCDMonoid
    ( RightGCDMonoid (..) )
import qualified Data.Monoid.GCD as OverlappingGCDMonoid
    ( OverlappingGCDMonoid (..) )
import qualified Data.Monoid.GCD as GCDMonoid
    ( GCDMonoid (..) )
import qualified Data.Monoid.LCM as LCMMonoid
    ( LCMMonoid (..) )
import qualified Data.Monoid.Monus as Monus
    ( Monus (..) )
import qualified Data.Semigroup as Semigroup
    ( Semigroup (..) )

spec :: Spec
spec = describe "Distributivity" $ do

    forM_ testTypesMonoidNull $
        \(TestType p) -> specMonoidNull
            (Proxy @Key) p
    forM_ testTypesGroup $
        \(TestType p) -> specGroup
            (Proxy @Key) p
    forM_ testTypesMonus $
        \(TestType p) -> specMonus
            (Proxy @Key) p
    forM_ testTypesLeftGCDMonoid $
        \(TestType p) -> specLeftGCDMonoid
            (Proxy @Key) p
    forM_ testTypesRightGCDMonoid $
        \(TestType p) -> specRightGCDMonoid
            (Proxy @Key) p
    forM_ testTypesOverlappingGCDMonoid $
        \(TestType p) -> specOverlappingGCDMonoid
            (Proxy @Key) p
    forM_ testTypesGCDMonoid $
        \(TestType p) -> specGCDMonoid
            (Proxy @Key) p
    forM_ testTypesLCMMonoid $
        \(TestType p) -> specLCMMonoid
            (Proxy @Key) p

specMonoidNull
    :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specMonoidNull = makeSpec $ do
    it "prop_distributive_get_mappend" $
        prop_distributive_get_mappend
            @k @v & property

specGroup
    :: forall k v. (Test k v, Group v) => Proxy k -> Proxy v -> Spec
specGroup = makeSpec $ do
    it "prop_distributive_get_minus" $
        prop_distributive_get_minus
            @k @v & property

specMonus
    :: forall k v. (Test k v, Monus v) => Proxy k -> Proxy v -> Spec
specMonus = makeSpec $ do
    it "prop_distributive_get_monus" $
        prop_distributive_get_monus
            @k @v & property

specLeftGCDMonoid
    :: forall k v. (Test k v, LeftGCDMonoid v) => Proxy k -> Proxy v -> Spec
specLeftGCDMonoid = makeSpec $ do
    it "prop_distributive_get_commonPrefix" $
        prop_distributive_get_commonPrefix
            @k @v & property

specRightGCDMonoid
    :: forall k v. (Test k v, RightGCDMonoid v) => Proxy k -> Proxy v -> Spec
specRightGCDMonoid = makeSpec $ do
    it "prop_distributive_get_commonSuffix" $
        prop_distributive_get_commonSuffix
            @k @v & property

specOverlappingGCDMonoid
    :: forall k v. (Test k v, OverlappingGCDMonoid v)
    => Proxy k
    -> Proxy v
    -> Spec
specOverlappingGCDMonoid = makeSpec $ do
    it "prop_distributive_get_overlap" $
        prop_distributive_get_overlap
            @k @v & property

specGCDMonoid
    :: forall k v. (Test k v, GCDMonoid v) => Proxy k -> Proxy v -> Spec
specGCDMonoid = makeSpec $ do
    it "prop_distributive_get_gcd" $
        prop_distributive_get_gcd
            @k @v & property

specLCMMonoid
    :: forall k v. (Test k v, LCMMonoid v) => Proxy k -> Proxy v -> Spec
specLCMMonoid = makeSpec $ do
    it "prop_distributive_get_lcm" $
        prop_distributive_get_lcm
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
prop_distributive_get_mappend =
    prop_distributive_get
        (Semigroup.<>)
        (Semigroup.<>)

prop_distributive_get_minus
    :: (Test k v, Group v) => k -> MonoidMap k v -> MonoidMap k v -> Property
prop_distributive_get_minus =
    prop_distributive_get
        (Group.~~)
        (Group.~~)

prop_distributive_get_monus
    :: (Test k v, Monus v) => k -> MonoidMap k v -> MonoidMap k v -> Property
prop_distributive_get_monus =
    prop_distributive_get
        (Monus.<\>)
        (Monus.<\>)

prop_distributive_get_commonPrefix
    :: (Test k v, LeftGCDMonoid v)
    => k
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_distributive_get_commonPrefix =
    prop_distributive_get
        LeftGCDMonoid.commonPrefix
        LeftGCDMonoid.commonPrefix

prop_distributive_get_commonSuffix
    :: (Test k v, RightGCDMonoid v)
    => k
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_distributive_get_commonSuffix =
    prop_distributive_get
        RightGCDMonoid.commonSuffix
        RightGCDMonoid.commonSuffix

prop_distributive_get_overlap
    :: (Test k v, OverlappingGCDMonoid v)
    => k
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_distributive_get_overlap =
    prop_distributive_get
        OverlappingGCDMonoid.overlap
        OverlappingGCDMonoid.overlap

prop_distributive_get_gcd
    :: (Test k v, GCDMonoid v)
    => k
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_distributive_get_gcd =
    prop_distributive_get
        GCDMonoid.gcd
        GCDMonoid.gcd

prop_distributive_get_lcm
    :: (Test k v, LCMMonoid v)
    => k
    -> MonoidMap k v
    -> MonoidMap k v
    -> Property
prop_distributive_get_lcm =
    prop_distributive_get
        LCMMonoid.lcm
        LCMMonoid.lcm
