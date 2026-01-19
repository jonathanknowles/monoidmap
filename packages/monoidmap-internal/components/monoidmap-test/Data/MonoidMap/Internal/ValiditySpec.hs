{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.ValiditySpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Data
    ( Proxy (Proxy) )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity )
import Data.Group
    ( Group )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust )
import Data.Monoid.Cancellative
    ( GCDMonoid
    , LeftGCDMonoid
    , LeftReductive
    , OverlappingGCDMonoid
    , Reductive
    , RightGCDMonoid
    , RightReductive
    )
import Data.Monoid.LCM
    ( LCMMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.MonoidMap.Internal
    ( MonoidMap )
import Data.MonoidMap.Internal.SliceSpec
    ( Slice (..) )
import Data.Set
    ( Set )
import Test.Common
    ( Key
    , Test
    , TestValueType (TestValueType)
    , makeSpec
    , property
    , testValueTypesGCDMonoid
    , testValueTypesGroup
    , testValueTypesLCMMonoid
    , testValueTypesLeftGCDMonoid
    , testValueTypesLeftReductive
    , testValueTypesAll
    , testValueTypesMonus
    , testValueTypesOverlappingGCDMonoid
    , testValueTypesReductive
    , testValueTypesRightGCDMonoid
    , testValueTypesRightReductive
    )
import Test.Hspec
    ( Spec, it )
import Test.QuickCheck
    ( Fun
    , Property
    , applyFun
    , applyFun2
    , applyFun3
    , conjoin
    , counterexample
    , cover
    )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Monoid.Null as Null
import qualified Data.MonoidMap.Internal as MonoidMap

spec :: Spec
spec = do
    specForAll
        testValueTypesAll
        specValidMonoidNull
    specForAll
        testValueTypesLeftReductive
        specValidLeftReductive
    specForAll
        testValueTypesRightReductive
        specValidRightReductive
    specForAll
        testValueTypesReductive
        specValidReductive
    specForAll
        testValueTypesLeftGCDMonoid
        specValidLeftGCDMonoid
    specForAll
        testValueTypesRightGCDMonoid
        specValidRightGCDMonoid
    specForAll
        testValueTypesOverlappingGCDMonoid
        specValidOverlappingGCDMonoid
    specForAll
        testValueTypesGCDMonoid
        specValidGCDMonoid
    specForAll
        testValueTypesLCMMonoid
        specValidLCMMonoid
    specForAll
        testValueTypesMonus
        specValidMonus
    specForAll
        testValueTypesGroup
        specValidGroup
  where
    specForAll
        :: [TestValueType c]
        -> (forall k v. (Test k v, c v) => Proxy k -> Proxy v -> Spec)
        -> Spec
    specForAll testValueTypes specFn = forM_ testValueTypes (specFor specFn)

    specFor
        :: (forall k v. (Test k v, c v) => Proxy k -> Proxy v -> Spec)
        -> TestValueType c
        -> Spec
    specFor specFn (TestValueType (v :: Proxy v)) =
        specFn (Proxy @Key) v

specValidMonoidNull
    :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specValidMonoidNull = makeSpec $ do
    it "propValid_fromList" $
        propValid_fromList
            @k @v & property
    it "propValid_fromListWith" $
        propValid_fromListWith
            @k @v & property
    it "propValid_fromMap" $
        propValid_fromMap
            @k @v & property
    it "propValid_fromSet" $
        propValid_fromSet
            @k @v & property
    it "propValid_singleton" $
        propValid_singleton
            @k @v & property
    it "propValid_set" $
        propValid_set
            @k @v & property
    it "propValid_adjust" $
        propValid_adjust
            @k @v & property
    it "propValid_nullify" $
        propValid_nullify
            @k @v & property
    it "propValid_take" $
        propValid_take
            @k @v & property
    it "propValid_drop" $
        propValid_drop
            @k @v & property
    it "propValid_splitAt" $
        propValid_splitAt
            @k @v & property
    it "propValid_filter" $
        propValid_filter
            @k @v & property
    it "propValid_filterKeys" $
        propValid_filterKeys
            @k @v & property
    it "propValid_filterWithKey" $
        propValid_filterWithKey
            @k @v & property
    it "propValid_partition" $
        propValid_partition
            @k @v & property
    it "propValid_partitionKeys" $
        propValid_partitionKeys
            @k @v & property
    it "propValid_partitionWithKey" $
        propValid_partitionWithKey
            @k @v & property
    it "propValid_map" $
        propValid_map
            @k @v & property
    it "propValid_mapKeys" $
        propValid_mapKeys
            @k @v & property
    it "propValid_mapKeysWith" $
        propValid_mapKeysWith
            @k @v & property
    it "propValid_mapWithKey" $
        propValid_mapWithKey
            @k @v & property
    it "propValid_mapAccumL" $
        propValid_mapAccumL
            @k @v & property
    it "propValid_mapAccumR" $
        propValid_mapAccumR
            @k @v & property
    it "propValid_mapAccumLWithKey" $
        propValid_mapAccumLWithKey
            @k @v & property
    it "propValid_mapAccumRWithKey" $
        propValid_mapAccumRWithKey
            @k @v & property
    it "propValid_traverse" $
        propValid_traverse
            @k @v & property
    it "propValid_traverseWithKey" $
        propValid_traverseWithKey
            @k @v & property
    it "propValid_intersectionWith" $
        propValid_intersectionWith
            @k @v & property
    it "propValid_unionWith" $
        propValid_unionWith
            @k @v & property
    it "propValid_append" $
        propValid_append
            @k @v & property

specValidLeftReductive
    :: forall k v. (Test k v, LeftReductive v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidLeftReductive = makeSpec $ do
    it "propValid_stripPrefix" $
        propValid_stripPrefix
            @k @v & property

specValidRightReductive
    :: forall k v. (Test k v, RightReductive v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidRightReductive = makeSpec $ do
    it "propValid_stripSuffix" $
        propValid_stripSuffix
            @k @v & property

specValidReductive
    :: forall k v. (Test k v, Reductive v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidReductive = makeSpec $ do
    it "propValid_minusMaybe" $
        propValid_minusMaybe
            @k @v & property

specValidLeftGCDMonoid
    :: forall k v. (Test k v, LeftGCDMonoid v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidLeftGCDMonoid = makeSpec $ do
    it "propValid_commonPrefix" $
        propValid_commonPrefix
            @k @v & property
    it "propValid_stripCommonPrefix" $
        propValid_stripCommonPrefix
            @k @v & property

specValidRightGCDMonoid
    :: forall k v. (Test k v, RightGCDMonoid v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidRightGCDMonoid = makeSpec $ do
    it "propValid_commonSuffix" $
        propValid_commonSuffix
            @k @v & property
    it "propValid_stripCommonSuffix" $
        propValid_stripCommonSuffix
            @k @v & property

specValidOverlappingGCDMonoid
    :: forall k v. (Test k v, OverlappingGCDMonoid v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidOverlappingGCDMonoid = makeSpec $ do
    it "propValid_overlap" $
        propValid_overlap
            @k @v & property
    it "propValid_stripPrefixOverlap" $
        propValid_stripPrefixOverlap
            @k @v & property
    it "propValid_stripSuffixOverlap" $
        propValid_stripSuffixOverlap
            @k @v & property
    it "propValid_stripOverlap" $
        propValid_stripOverlap
            @k @v & property

specValidGCDMonoid
    :: forall k v. (Test k v, GCDMonoid v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidGCDMonoid = makeSpec $ do
    it "propValid_intersection" $
        propValid_intersection
            @k @v & property

specValidLCMMonoid
    :: forall k v. (Test k v, LCMMonoid v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidLCMMonoid = makeSpec $ do
    it "propValid_union" $
        propValid_union
            @k @v & property

specValidMonus
    :: forall k v. (Test k v, Monus v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidMonus = makeSpec $ do
    it "propValid_monus" $
        propValid_monus
            @k @v & property

specValidGroup
    :: forall k v. (Test k v, Group v)
    => Proxy k
    -> Proxy v
    -> Spec
specValidGroup = makeSpec $ do
    it "propValid_minus" $
        propValid_minus
            @k @v & property
    it "propValid_invert" $
        propValid_invert
            @k @v & property
    it "propValid_power" $
        propValid_power
            @k @v & property

propValid
    :: Test k v => MonoidMap k v -> Property
propValid m = conjoin
    [ counterexample
        "propValid_nonNullKeys"
        (propValid_nonNullKeys)
    , counterexample
        "propValid_toList"
        (propValid_toList)
    ]
    & cover 2
        (not (Null.null m))
        "not (Null.null m)"
  where
    propValid_nonNullKeys =
        all (\k -> MonoidMap.get k m /= mempty) (MonoidMap.nonNullKeys m)
    propValid_toList =
        all (\(_, v) -> v /= mempty) (MonoidMap.toList m)

propValid_fromList
    :: Test k v => [(k, v)] -> Property
propValid_fromList kvs =
    propValid (MonoidMap.fromList kvs)
    & cover 2
        (filter (Null.null . snd) kvs /= [])
        "filter (Null.null . snd) kvs /= []"

propValid_fromListWith
    :: Test k v => Fun (v, v) v -> [(k, v)] -> Property
propValid_fromListWith (applyFun2 -> f) kvs =
    propValid (MonoidMap.fromListWith f kvs)
    & cover 2
        (filter (Null.null . snd) kvs /= [])
        "filter (Null.null . snd) kvs /= []"

propValid_fromMap
    :: Test k v => Map k v -> Property
propValid_fromMap m =
    propValid (MonoidMap.fromMap m)
    & cover 2
        (Map.filter Null.null m /= mempty)
        "Map.filter Null.null m /= mempty"

propValid_fromSet
    :: Test k v => Fun k v -> Set k -> Property
propValid_fromSet (applyFun -> f) ks =
    propValid (MonoidMap.fromSet f ks)
    & cover 2
        (Map.filter Null.null (Map.fromSet f ks) /= mempty)
        "Map.filter Null.null (Map.fromSet f ks) /= mempty"

propValid_singleton
    :: Test k v => k -> v -> Property
propValid_singleton k v =
    propValid (MonoidMap.singleton k v)
    & cover 2
        (Null.null v)
        "Null.null v"

propValid_set
    :: Test k v => k -> v -> MonoidMap k v -> Property
propValid_set k v m =
    propValid (MonoidMap.set k v m)
    & cover 2
        (Null.null v)
        "Null.null v"

propValid_adjust
    :: Test k v => Fun v v -> k -> MonoidMap k v -> Property
propValid_adjust (applyFun -> f) k m =
    propValid (MonoidMap.adjust f k m)
    & cover 1
        (Null.null (f (MonoidMap.get k m)))
        "Null.null (f (MonoidMap.get k m))"

propValid_nullify
    :: Test k v => k -> MonoidMap k v -> Property
propValid_nullify k m =
    propValid (MonoidMap.nullify k m)
    & cover 2
        (MonoidMap.nonNullKey k m)
        "MonoidMap.nonNullKey k m"

propValid_take
    :: Test k v => Slice k v -> Property
propValid_take (Slice i m) =
    propValid (MonoidMap.take i m)

propValid_drop
    :: Test k v => Slice k v -> Property
propValid_drop (Slice i m) =
    propValid (MonoidMap.drop i m)

propValid_splitAt
    :: Test k v => Slice k v -> Property
propValid_splitAt (Slice i m) =
    conjoin
        [ counterexample "propValid m1" (propValid m1)
        , counterexample "propValid m2" (propValid m2)
        ]
  where
    (m1, m2) = MonoidMap.splitAt i m

propValid_filter
    :: Test k v => Fun v Bool -> MonoidMap k v -> Property
propValid_filter (applyFun -> f) m =
    propValid (MonoidMap.filter f m)

propValid_filterKeys
    :: Test k v => Fun k Bool -> MonoidMap k v -> Property
propValid_filterKeys (applyFun -> f) m =
    propValid (MonoidMap.filterKeys f m)

propValid_filterWithKey
    :: Test k v => Fun (k, v) Bool -> MonoidMap k v -> Property
propValid_filterWithKey (applyFun2 -> f) m =
    propValid (MonoidMap.filterWithKey f m)

propValid_partition
    :: Test k v => Fun v Bool -> MonoidMap k v -> Property
propValid_partition (applyFun -> f) m =
    conjoin
        [ counterexample "propValid m1" (propValid m1)
        , counterexample "propValid m2" (propValid m2)
        ]
  where
    (m1, m2) = MonoidMap.partition f m

propValid_partitionKeys
    :: Test k v => Fun k Bool -> MonoidMap k v -> Property
propValid_partitionKeys (applyFun -> f) m =
    conjoin
        [ counterexample "propValid m1" (propValid m1)
        , counterexample "propValid m2" (propValid m2)
        ]
  where
    (m1, m2) = MonoidMap.partitionKeys f m

propValid_partitionWithKey
    :: Test k v => Fun (k, v) Bool -> MonoidMap k v -> Property
propValid_partitionWithKey (applyFun2 -> f) m =
    conjoin
        [ counterexample "propValid m1" (propValid m1)
        , counterexample "propValid m2" (propValid m2)
        ]
  where
    (m1, m2) = MonoidMap.partitionWithKey f m

propValid_map
    :: Test k v => Fun v v -> MonoidMap k v -> Property
propValid_map (applyFun -> f) m =
    propValid (MonoidMap.map f m)

propValid_mapKeys
    :: Test k v => Fun k k -> MonoidMap k v -> Property
propValid_mapKeys (applyFun -> f) m =
    propValid (MonoidMap.mapKeys f m)

propValid_mapKeysWith
    :: Test k v => Fun (v, v) v -> Fun k k -> MonoidMap k v -> Property
propValid_mapKeysWith (applyFun2 -> f) (applyFun -> g) m =
    propValid (MonoidMap.mapKeysWith f g m)

propValid_mapWithKey
    :: Test k v => Fun (k, v) v -> MonoidMap k v -> Property
propValid_mapWithKey (applyFun2 -> f) m =
    propValid (MonoidMap.mapWithKey f m)

propValid_mapAccumL
    :: forall k v s. s ~ Int
    => Test k v
    => Fun (s, v) (s, v)
    -> s
    -> MonoidMap k v
    -> Property
propValid_mapAccumL (applyFun2 -> f) s m =
    propValid $ snd $ MonoidMap.mapAccumL f s m

propValid_mapAccumR
    :: forall k v s. s ~ Int
    => Test k v
    => Fun (s, v) (s, v)
    -> s
    -> MonoidMap k v
    -> Property
propValid_mapAccumR (applyFun2 -> f) s m =
    propValid $ snd $ MonoidMap.mapAccumR f s m

propValid_mapAccumLWithKey
    :: forall k v s. s ~ Int
    => Test k v
    => Fun (s, k, v) (s, v)
    -> s
    -> MonoidMap k v
    -> Property
propValid_mapAccumLWithKey (applyFun3 -> f) s m =
    propValid $ snd $ MonoidMap.mapAccumLWithKey f s m

propValid_mapAccumRWithKey
    :: forall k v s. s ~ Int
    => Test k v
    => Fun (s, k, v) (s, v)
    -> s
    -> MonoidMap k v
    -> Property
propValid_mapAccumRWithKey (applyFun3 -> f) s m =
    propValid $ snd $ MonoidMap.mapAccumRWithKey f s m

propValid_traverse
    :: forall k v t. (Applicative t, Foldable t, Test k v)
    => t ~ Identity
    => Fun v (t v)
    -> MonoidMap k v
    -> Property
propValid_traverse (applyFun -> f) m
    = conjoin
    $ fmap propValid
    $ F.toList @t
    $ MonoidMap.traverse f m

propValid_traverseWithKey
    :: forall k v t. (Applicative t, Foldable t, Test k v)
    => t ~ Identity
    => Fun (k, v) (t v)
    -> MonoidMap k v
    -> Property
propValid_traverseWithKey (applyFun2 -> f) m
    = conjoin
    $ fmap propValid
    $ F.toList @t
    $ MonoidMap.traverseWithKey f m

propValid_intersection
    :: (Test k v, GCDMonoid v) => MonoidMap k v -> MonoidMap k v -> Property
propValid_intersection m1 m2 =
    propValid (MonoidMap.intersection m1 m2)

propValid_intersectionWith
    :: Test k v => Fun (v, v) v -> MonoidMap k v -> MonoidMap k v -> Property
propValid_intersectionWith (applyFun2 -> f) m1 m2 =
    propValid (MonoidMap.intersectionWith f m1 m2)

propValid_union
    :: (Test k v, LCMMonoid v) => MonoidMap k v -> MonoidMap k v -> Property
propValid_union m1 m2 =
    propValid (MonoidMap.union m1 m2)

propValid_unionWith
    :: Test k v => Fun (v, v) v -> MonoidMap k v -> MonoidMap k v -> Property
propValid_unionWith (applyFun2 -> f) m1 m2 =
    propValid (MonoidMap.unionWith f m1 m2)

propValid_append
    :: Test k v => MonoidMap k v -> MonoidMap k v -> Property
propValid_append m1 m2 =
    propValid (MonoidMap.append m1 m2)

propValid_minus
    :: (Test k v, Group v) => MonoidMap k v -> MonoidMap k v -> Property
propValid_minus m1 m2 =
    propValid (MonoidMap.minus m1 m2)

propValid_minusMaybe
    :: (Test k v, Reductive v) => MonoidMap k v -> MonoidMap k v -> Property
propValid_minusMaybe m1 m2 =
    maybe (property True) propValid mr
    & cover 2 (isJust mr) "isJust mr"
  where
    mr = MonoidMap.minusMaybe m1 m2

propValid_monus
    :: (Test k v, Monus v) => MonoidMap k v -> MonoidMap k v -> Property
propValid_monus m1 m2 =
    propValid (MonoidMap.monus m1 m2)

propValid_invert
    :: (Test k v, Group v) => MonoidMap k v -> Property
propValid_invert m =
    propValid (MonoidMap.invert m)

propValid_power
    :: (Test k v, Group v) => MonoidMap k v -> Int -> Property
propValid_power m i =
    propValid (MonoidMap.power m i)

propValid_commonPrefix
    :: (Test k v, LeftGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_commonPrefix m1 m2 =
    propValid (MonoidMap.commonPrefix m1 m2)

propValid_commonSuffix
    :: (Test k v, RightGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_commonSuffix m1 m2 =
    propValid (MonoidMap.commonSuffix m1 m2)

propValid_stripPrefix
    :: (Test k v, LeftReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_stripPrefix m1 m2 =
    maybe (property True) propValid mr
    & cover 2 (isJust mr) "isJust mr"
  where
    mr = MonoidMap.stripPrefix m1 m2

propValid_stripSuffix
    :: (Test k v, RightReductive v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_stripSuffix m1 m2 =
    maybe (property True) propValid mr
    & cover 2 (isJust mr) "isJust mr"
  where
    mr = MonoidMap.stripSuffix m1 m2

propValid_stripCommonPrefix
    :: (Test k v, LeftGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_stripCommonPrefix m1 m2 =
    conjoin
        [ counterexample "propValid r1" (propValid r1)
        , counterexample "propValid r2" (propValid r2)
        , counterexample "propValid r3" (propValid r3)
        ]
  where
    (r1, r2, r3) = MonoidMap.stripCommonPrefix m1 m2

propValid_stripCommonSuffix
    :: (Test k v, RightGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_stripCommonSuffix m1 m2 =
    conjoin
        [ counterexample "propValid r1" (propValid r1)
        , counterexample "propValid r2" (propValid r2)
        , counterexample "propValid r3" (propValid r3)
        ]
  where
    (r1, r2, r3) = MonoidMap.stripCommonSuffix m1 m2

propValid_overlap
    :: (Test k v, OverlappingGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_overlap m1 m2 =
    propValid (MonoidMap.overlap m1 m2)

propValid_stripPrefixOverlap
    :: (Test k v, OverlappingGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_stripPrefixOverlap m1 m2 =
    propValid (MonoidMap.stripPrefixOverlap m1 m2)

propValid_stripSuffixOverlap
    :: (Test k v, OverlappingGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_stripSuffixOverlap m1 m2 =
    propValid (MonoidMap.stripSuffixOverlap m1 m2)

propValid_stripOverlap
    :: (Test k v, OverlappingGCDMonoid v)
    => MonoidMap k v
    -> MonoidMap k v
    -> Property
propValid_stripOverlap m1 m2 =
    conjoin
        [ counterexample "propValid r1" (propValid r1)
        , counterexample "propValid r2" (propValid r2)
        , counterexample "propValid r3" (propValid r3)
        ]
  where
    (r1, r2, r3) = MonoidMap.stripOverlap m1 m2
