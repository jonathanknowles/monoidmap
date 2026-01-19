{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use null" #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.RecoveredMapSpec
    where

import Prelude

import Data.Function
    ( on, (&) )
import Data.List
    ( nubBy )
import Data.Monoid
    ( Sum (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup
    ( Semigroup (stimes) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable, typeRep )
import Numeric.Natural
    ( Natural )
import Test.Common
    ()
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary
    , Fun
    , Function
    , NonNegative (..)
    , Property
    , Testable
    , applyFun
    , applyFun2
    , applyFun3
    , checkCoverage
    , cover
    , listOf
    , shrinkMapBy
    , (===)
    )
import Test.QuickCheck.Classes
    ( eqLaws, functorLaws, monoidLaws, semigroupLaws, semigroupMonoidLaws )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )

import qualified Data.Map.Strict as OMap
import qualified Data.Set as Set
import qualified Data.MonoidMap.Internal.RecoveredMap as RMap
import qualified Test.QuickCheck as QC

spec :: Spec
spec = do
    specFor (Proxy @Int) (Proxy @(Set Int))
    specFor (Proxy @Int) (Proxy @(Set Natural))
    specFor (Proxy @Int) (Proxy @(Sum Int))
    specFor (Proxy @Int) (Proxy @(Sum Natural))
    specFor (Proxy @Int) (Proxy @Text)

specFor
    :: forall k v. () =>
        ( Arbitrary k
        , Arbitrary v
        , CoArbitrary k
        , CoArbitrary v
        , Eq v
        , Function k
        , Function v
        , Monoid v
        , Ord k
        , Show k
        , Show v
        , Typeable k
        , Typeable v
        )
    => Proxy k
    -> Proxy v
    -> Spec
specFor keyType valueType = do

    let description = mconcat
            [ "RecoveredMap ("
            , show (typeRep keyType)
            , ") ("
            , show (typeRep valueType)
            , ")"
            ]

    let property :: Testable t => t -> Property
        property = checkCoverage . QC.property

    describe description $ do

        describe "Class laws" $ do
            testLawsMany @(RMap.Map k v)
                [ eqLaws
                , monoidLaws
                , semigroupLaws
                , semigroupMonoidLaws
                ]
            testLawsMany @(RMap.Map k)
                [ functorLaws
                ]

        describe "Conversion to and from lists" $ do
            it "prop_fromList_toList" $
                prop_fromList_toList
                    @k @v & property

        describe "Empty" $ do
            it "prop_empty_keysSet" $
                prop_empty_keysSet
                    @k & property
            it "prop_empty_lookup" $
                prop_empty_lookup
                    @k @v & property
            it "prop_empty_show" $
                prop_empty_show
                    @k @v & property
            it "prop_empty_toList" $
                prop_empty_toList
                    @k @v & property

        describe "Singleton" $ do
            it "prop_singleton_keysSet" $
                prop_singleton_keysSet
                    @k @v & property
            it "prop_singleton_lookup" $
                prop_singleton_lookup
                    @k @v & property
            it "prop_singleton_show" $
                prop_singleton_show
                    @k @v & property
            it "prop_singleton_toList" $
                prop_singleton_toList
                    @k @v & property

        describe "Append" $ do
            it "prop_append_toList" $
                prop_append_toList
                    @k @v & property

        describe "Times" $ do
            it "prop_stimes_toList" $
                prop_stimes_toList
                    @k @v & property

        describe "Delete" $ do
            it "prop_delete_lookup" $
                prop_delete_lookup
                    @k @v & property
            it "prop_delete_member" $
                prop_delete_member
                    @k @v & property
            it "prop_delete_toList" $
                prop_delete_toList
                    @k @v & property

        describe "Insert" $ do
            it "prop_insert_lookup" $
                prop_insert_lookup
                    @k @v & property
            it "prop_insert_member" $
                prop_insert_member
                    @k @v & property
            it "prop_insert_toList" $
                prop_insert_toList
                    @k @v & property

        describe "Map" $ do
            it "prop_map" $
                prop_map
                    @k @v @v & property
            it "prop_map_mempty" $
                prop_map_mempty
                    @k @v @v & property
            it "prop_mapWithKey" $
                prop_mapWithKey
                    @k @v @v & property

        describe "MapAccumL" $ do
            it "prop_mapAccumL @Int" $
                prop_mapAccumL @Int
                    @k @v @v & property
            it "prop_mapAccumL @String" $
                prop_mapAccumL @String
                    @k @v @v & property

        describe "MapAccumR" $ do
            it "prop_mapAccumR @Int" $
                prop_mapAccumR @Int
                    @k @v @v & property
            it "prop_mapAccumR @String" $
                prop_mapAccumR @String
                    @k @v @v & property

        describe "MapAccumWithKeyL" $ do
            it "prop_mapAccumLWithKey @Int" $
                prop_mapAccumLWithKey @Int
                    @k @v @v & property
            it "prop_mapAccumLWithKey @String" $
                prop_mapAccumLWithKey @String
                    @k @v @v & property

        describe "MapAccumWithKeyR" $ do
            it "prop_mapAccumRWithKey @Int" $
                prop_mapAccumRWithKey @Int
                    @k @v @v & property
            it "prop_mapAccumRWithKey @String" $
                prop_mapAccumRWithKey @String
                    @k @v @v & property

--------------------------------------------------------------------------------
-- Conversion to and from lists
--------------------------------------------------------------------------------

prop_fromList_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => [(k, v)]
    -> Property
prop_fromList_toList kvs =
    (===)
        (RMap.toList (RMap.fromList kvs))
        (OMap.toList (OMap.fromList kvs))
    & cover 10
        (length kvs > 1 && length (nubBy ((==) `on` fst) kvs) /= length kvs)
        "length kvs > 1 && length (nubBy ((==) `on` fst) kvs) /= length kvs"
    & cover 10
        (length kvs > 1 && length (nubBy ((==) `on` fst) kvs) == length kvs)
        "length kvs > 1 && length (nubBy ((==) `on` fst) kvs) == length kvs"

--------------------------------------------------------------------------------
-- Empty
--------------------------------------------------------------------------------

prop_empty_keysSet
    :: forall k. (Eq k, Show k)
    => Property
prop_empty_keysSet =
    (===)
        (RMap.keysSet (RMap.empty @k))
        (OMap.keysSet (OMap.empty @k))

prop_empty_lookup
    :: forall k v. (Ord k, Eq v, Show v)
    => k
    -> Property
prop_empty_lookup k =
    (===)
        (RMap.lookup k (RMap.empty @k @v))
        (OMap.lookup k (OMap.empty @k @v))

prop_empty_show
    :: forall k v. (Show k, Show v)
    => Property
prop_empty_show =
    (===)
        (show (RMap.empty @k @v))
        (show (OMap.empty @k @v))

prop_empty_toList
    :: forall k v. (Eq k, Show k, Eq v, Show v)
    => Property
prop_empty_toList =
    (===)
        (RMap.toList (RMap.empty @k @v))
        (OMap.toList (OMap.empty @k @v))

--------------------------------------------------------------------------------
-- Singleton
--------------------------------------------------------------------------------

prop_singleton_keysSet
    :: forall k v. (Ord k, Show k)
    => k
    -> v
    -> Property
prop_singleton_keysSet k v =
    (===)
        (RMap.keysSet (RMap.singleton k v))
        (OMap.keysSet (OMap.singleton k v))

prop_singleton_lookup
    :: forall k v. (Ord k, Eq v, Show v)
    => k
    -> v
    -> Property
prop_singleton_lookup k v =
    (===)
        (RMap.lookup k (RMap.singleton k v))
        (OMap.lookup k (OMap.singleton k v))

prop_singleton_show
    :: forall k v. (Ord k, Show k, Show v)
    => k
    -> v
    -> Property
prop_singleton_show k v =
    (===)
        (show (RMap.singleton k v))
        (show (OMap.singleton k v))

prop_singleton_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => k
    -> v
    -> Property
prop_singleton_toList k v =
    (===)
        (RMap.toList (RMap.singleton k v))
        (OMap.toList (OMap.singleton k v))

--------------------------------------------------------------------------------
-- Append
--------------------------------------------------------------------------------

prop_append_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => [(k, v)]
    -> [(k, v)]
    -> Property
prop_append_toList kvs1 kvs2 =
    (===)
        (RMap.toList (RMap.fromList kvs1 <> RMap.fromList kvs2))
        (OMap.toList (OMap.fromList kvs1 <> OMap.fromList kvs2))
    & cover 10
        (ks1 `Set.disjoint` ks2)
        "ks1 `Set.disjoint` ks2"
    & cover 10
        (not (ks1 `Set.disjoint` ks2))
        "not (ks1 `Set.disjoint` ks2)"
  where
    ks1 = Set.fromList (fst <$> kvs1)
    ks2 = Set.fromList (fst <$> kvs2)

--------------------------------------------------------------------------------
-- Times
--------------------------------------------------------------------------------

prop_stimes_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => [(k, v)]
    -> NonNegative Int
    -> Property
prop_stimes_toList kvs (NonNegative n) =
    (===)
        (RMap.toList (stimes n (RMap.fromList kvs)))
        (OMap.toList (stimes n (OMap.fromList kvs)))
    & cover 1
        (n == 0)
        "n == 0"
    & cover 1
        (n == 1)
        "n == 1"
    & cover 10
        (n >= 2)
        "n >= 2"

--------------------------------------------------------------------------------
-- Delete
--------------------------------------------------------------------------------

prop_delete_lookup
    :: forall k v. (Ord k, Eq v, Show v)
    => [(k, v)]
    -> k
    -> Property
prop_delete_lookup kvs k =
    (===)
        (RMap.lookup k (RMap.delete k (RMap.fromList kvs)))
        (OMap.lookup k (OMap.delete k (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"

prop_delete_member
    :: forall k v. (Ord k, Eq v)
    => [(k, v)]
    -> k
    -> Property
prop_delete_member kvs k =
    (===)
        (RMap.member k (RMap.delete k (RMap.fromList kvs)))
        (OMap.member k (OMap.delete k (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"

prop_delete_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => [(k, v)]
    -> k
    -> Property
prop_delete_toList kvs k =
    (===)
        (RMap.toList (RMap.delete k (RMap.fromList kvs)))
        (OMap.toList (OMap.delete k (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"

--------------------------------------------------------------------------------
-- Insert
--------------------------------------------------------------------------------

prop_insert_lookup
    :: forall k v. (Ord k, Eq v, Show v)
    => [(k, v)]
    -> k
    -> v
    -> Property
prop_insert_lookup kvs k v =
    (===)
        (RMap.lookup k (RMap.insert k v (RMap.fromList kvs)))
        (OMap.lookup k (OMap.insert k v (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"

prop_insert_member
    :: forall k v. (Ord k, Eq v)
    => [(k, v)]
    -> k
    -> v
    -> Property
prop_insert_member kvs k v =
    (===)
        (RMap.member k (RMap.insert k v (RMap.fromList kvs)))
        (OMap.member k (OMap.insert k v (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"

prop_insert_toList
    :: forall k v. (Ord k, Show k, Eq v, Show v)
    => [(k, v)]
    -> k
    -> v
    -> Property
prop_insert_toList kvs k v =
    (===)
        (RMap.toList (RMap.insert k v (RMap.fromList kvs)))
        (OMap.toList (OMap.insert k v (OMap.fromList kvs)))
    & cover 10
        (filter ((== k) . fst) kvs == [])
        "filter ((== k) . fst) kvs == []"
    & cover 10
        (filter ((== k) . fst) kvs /= [])
        "filter ((== k) . fst) kvs /= []"

--------------------------------------------------------------------------------
-- Map
--------------------------------------------------------------------------------

prop_map
    :: (Ord k, Show k, Eq v2, Show v2)
    => [(k, v1)]
    -> Fun v1 v2
    -> Property
prop_map kvs (applyFun -> f) =
    (===)
        (RMap.toList (RMap.map f (RMap.fromList kvs)))
        (OMap.toList (OMap.map f (OMap.fromList kvs)))

prop_map_mempty
    :: forall k v1 v2. (Ord k, Show k, Eq v2, Monoid v2, Show v2)
    => [(k, v1)]
    -> Property
prop_map_mempty kvs =
    (===)
        (RMap.toList (RMap.map (const (mempty @v2)) (RMap.fromList kvs)))
        (OMap.toList (OMap.map (const (mempty @v2)) (OMap.fromList kvs)))

prop_mapWithKey
    :: (Ord k, Show k, Eq v2, Show v2)
    => [(k, v1)]
    -> Fun (k, v1) v2
    -> Property
prop_mapWithKey kvs (applyFun2 -> f) =
    (===)
        (RMap.toList (RMap.mapWithKey f (RMap.fromList kvs)))
        (OMap.toList (OMap.mapWithKey f (OMap.fromList kvs)))

--------------------------------------------------------------------------------
-- MapAccum
--------------------------------------------------------------------------------

prop_mapAccumL
    :: forall s k v1 v2. (Eq s, Eq v2, Ord k, Show k, Show s, Show v2)
    => Fun (s, v1) (s, v2)
    -> s
    -> [(k, v1)]
    -> Property
prop_mapAccumL (applyFun2 -> f) s0 kvs =
    (===)
        (RMap.toList <$> rmapAccumL f s0 (RMap.fromList kvs))
        (OMap.toList <$> omapAccumL f s0 (OMap.fromList kvs))
  where
    rmapAccumL = RMap.mapAccumL
    omapAccumL = OMap.mapAccum

prop_mapAccumR
    :: forall s k v1 v2. (Eq s, Eq v2, Ord k, Show k, Show s, Show v2)
    => Fun (s, v1) (s, v2)
    -> s
    -> [(k, v1)]
    -> Property
prop_mapAccumR (applyFun2 -> f) s0 kvs =
    (===)
        (RMap.toList <$> rmapAccumR f s0 (RMap.fromList kvs))
        (OMap.toList <$> omapAccumR f s0 (OMap.fromList kvs))
  where
    rmapAccumR   = RMap.mapAccumR
    omapAccumR g = OMap.mapAccumRWithKey (\s _ v -> g s v)

--------------------------------------------------------------------------------
-- MapAccumWithKey
--------------------------------------------------------------------------------

prop_mapAccumLWithKey
    :: forall s k v1 v2. (Eq s, Eq v2, Ord k, Show k, Show s, Show v2)
    => Fun (s, k, v1) (s, v2)
    -> s
    -> [(k, v1)]
    -> Property
prop_mapAccumLWithKey (applyFun3 -> f) s0 kvs =
    (===)
        (RMap.toList <$> rmapAccumLWithKey f s0 (RMap.fromList kvs))
        (OMap.toList <$> omapAccumLWithKey f s0 (OMap.fromList kvs))
  where
    rmapAccumLWithKey = RMap.mapAccumLWithKey
    omapAccumLWithKey = OMap.mapAccumWithKey

prop_mapAccumRWithKey
    :: forall s k v1 v2. (Eq s, Eq v2, Ord k, Show k, Show s, Show v2)
    => Fun (s, k, v1) (s, v2)
    -> s
    -> [(k, v1)]
    -> Property
prop_mapAccumRWithKey (applyFun3 -> f) s0 kvs =
    (===)
        (RMap.toList <$> rmapAccumRWithKey f s0 (RMap.fromList kvs))
        (OMap.toList <$> omapAccumRWithKey f s0 (OMap.fromList kvs))
  where
    rmapAccumRWithKey = RMap.mapAccumRWithKey
    omapAccumRWithKey = OMap.mapAccumRWithKey

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance (Arbitrary k, Ord k, Arbitrary v) =>
    Arbitrary (RMap.Map k v)
  where
    arbitrary = RMap.fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)
    shrink = shrinkMapBy RMap.fromList RMap.toList shrink
