{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.ConversionSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.MonoidMap.Internal
    ( MonoidMap, nonNullCount )
import Data.Proxy
    ( Proxy (..) )
import Data.Set
    ( Set )
import Test.Common
    ( Key
    , Test
    , TestValueType (TestValueType)
    , makeSpec
    , property
    , testValueTypesAll
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..), Property, applyFun, applyFun2, cover, (===) )

import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Monoid.Null as Null
import qualified Data.MonoidMap.Internal as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Conversions" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    describe "Conversion to and from lists" $ do
        it "prop_fromList_get" $
            prop_fromList_get
                @k @v & property
        it "prop_fromList_toMap" $
            prop_fromList_toMap
                @k @v & property
        it "prop_fromList_toList" $
            prop_fromList_toList
                @k @v & property
        it "prop_toList_fromList" $
            prop_toList_fromList
                @k @v & property
        it "prop_toList_sort" $
            prop_toList_sort
                @k @v & property
        it "prop_fromListWith_get" $
            prop_fromListWith_get
                @k @v & property

    describe "Conversion to and from ordinary maps" $ do
        it "prop_fromMap_get" $
            prop_fromMap_get
                @k @v & property
        it "prop_fromMap_toMap" $
            prop_fromMap_toMap
                @k @v & property
        it "prop_fromMapWith_fromMap" $
            prop_fromMapWith_fromMap
                @k @v & property
        it "prop_fromMapWith_get" $
            prop_fromMapWith_get
                @k @v & property
        it "prop_toMap_fromMap" $
            prop_toMap_fromMap
                @k @v & property

    describe "Conversion from sets" $ do
        it "prop_fromSet_get" $
            prop_fromSet_get
                @k @v & property

--------------------------------------------------------------------------------
-- Conversion to and from lists
--------------------------------------------------------------------------------

prop_fromList_get
    :: Test k v => [(k, v)] -> k -> Property
prop_fromList_get kvs k =
    MonoidMap.get k (MonoidMap.fromList kvs)
        ===
        F.foldMap snd (filter ((== k) . fst) kvs)
    & cover 2
        (matchingKeyCount == 0)
        "matchingKeyCount == 0"
    & cover 2
        (matchingKeyCount == 1)
        "matchingKeyCount == 1"
    & cover 2
        (matchingKeyCount == 2)
        "matchingKeyCount == 2"
    & cover 2
        (matchingKeyCount >= 3)
        "matchingKeyCount >= 3"
  where
    matchingKeyCount =
        length $ filter ((== k) . fst) kvs

prop_fromList_toMap
    :: Test k v => [(k, v)] -> Property
prop_fromList_toMap kvs =
    MonoidMap.toMap m === Map.filter (/= mempty) o
    & cover 2
        (MonoidMap.nonNull m && nonNullCount m /= Map.size o)
        "MonoidMap.nonNull m && nonNullCount m /= Map.size o"
    & cover 2
        (MonoidMap.nonNull m && nonNullCount m == Map.size o)
        "MonoidMap.nonNull m && nonNullCount m == Map.size o"
  where
    m = MonoidMap.fromList kvs
    o = Map.fromListWith (flip (<>)) kvs

prop_fromList_toList
    :: Test k v => [(k, v)] -> Property
prop_fromList_toList kvs =
    MonoidMap.toList m === Map.toList (Map.filter (/= mempty) o)
    & cover 2
        (MonoidMap.nonNull m && nonNullCount m /= Map.size o)
        "MonoidMap.nonNull m && nonNullCount m /= Map.size o"
    & cover 2
        (MonoidMap.nonNull m && nonNullCount m == Map.size o)
        "MonoidMap.nonNull m && nonNullCount m == Map.size o"
  where
    m = MonoidMap.fromList kvs
    o = Map.fromListWith (flip (<>)) kvs

prop_toList_fromList
    :: Test k v => MonoidMap k v -> Property
prop_toList_fromList m =
    MonoidMap.fromList (MonoidMap.toList m) === m
    & cover 2
        (MonoidMap.nonNull m)
        "MonoidMap.nonNull m"

prop_toList_sort
    :: Test k v => MonoidMap k v -> Property
prop_toList_sort m =
    List.sortOn fst (MonoidMap.toList m) === MonoidMap.toList m
    & cover 2
        (MonoidMap.nonNull m)
        "MonoidMap.nonNull m"

prop_fromListWith_get
    :: Test k v => Fun (v, v) v -> [(k, v)] -> k -> Property
prop_fromListWith_get (applyFun2 -> f) kvs k =
    MonoidMap.get k (MonoidMap.fromListWith f kvs)
        ===
        maybe mempty
            (F.foldl1 f)
            (NE.nonEmpty (snd <$> filter ((== k) . fst) kvs))
    & cover 2
        (matchingKeyCount == 0)
        "matchingKeyCount == 0"
    & cover 2
        (matchingKeyCount == 1)
        "matchingKeyCount == 1"
    & cover 2
        (matchingKeyCount == 2)
        "matchingKeyCount == 2"
    & cover 2
        (matchingKeyCount >= 3)
        "matchingKeyCount >= 3"
  where
    matchingKeyCount =
        length $ filter ((== k) . fst) kvs

--------------------------------------------------------------------------------
-- Conversion to and from ordinary maps
--------------------------------------------------------------------------------

prop_fromMap_get
    :: Test k v => Map k v -> k -> Property
prop_fromMap_get m k =
    MonoidMap.get k (MonoidMap.fromMap m) === Map.findWithDefault mempty k m
    & cover 2
        (MonoidMap.get k (MonoidMap.fromMap m) /= mempty)
        "MonoidMap.get k (MonoidMap.fromMap m) /= mempty"
    & cover 0.1
        (MonoidMap.get k (MonoidMap.fromMap m) == mempty && Map.member k m)
        "MonoidMap.get k (MonoidMap.fromMap m) == mempty && Map.member k m"

prop_fromMap_toMap
    :: Test k v => Map k v -> Property
prop_fromMap_toMap o =
    MonoidMap.toMap m === Map.filter (/= mempty) o
    & cover 2
        (MonoidMap.nonNull m && nonNullCount m /= Map.size o)
        "MonoidMap.nonNull m && nonNullCount m /= Map.size o"
    & cover 2
        (MonoidMap.nonNull m && nonNullCount m == Map.size o)
        "MonoidMap.nonNull m && nonNullCount m == Map.size o"
  where
    m = MonoidMap.fromMap o

prop_fromMapWith_fromMap
    :: Test k v => Map k v -> Property
prop_fromMapWith_fromMap m =
    MonoidMap.fromMapWith id m === MonoidMap.fromMap m
    & cover 2
        (MonoidMap.nonNull (MonoidMap.fromMap m))
        "MonoidMap.nonNull (MonoidMap.fromMap m)"

prop_fromMapWith_get
    :: Test k v => Fun v v -> Map k v -> k -> Property
prop_fromMapWith_get (applyFun -> f) m k =
    MonoidMap.get k (MonoidMap.fromMapWith f m)
        === maybe mempty f (Map.lookup k m)
    & cover 2
        (MonoidMap.nonNullKey k (MonoidMap.fromMapWith f m))
        "MonoidMap.nonNullKey k (MonoidMap.fromMapWith f m)"
    & cover 0.01
        (MonoidMap.nullKey k (MonoidMap.fromMapWith f m) && Map.member k m)
        "MonoidMap.nullKey k (MonoidMap.fromMapWith f m) && Map.member k m"

prop_toMap_fromMap
    :: Test k v => MonoidMap k v -> Property
prop_toMap_fromMap m =
    MonoidMap.fromMap (MonoidMap.toMap m) === m

--------------------------------------------------------------------------------
-- Conversion from sets
--------------------------------------------------------------------------------

prop_fromSet_get
    :: Test k v => Fun k v -> Set k -> k -> Property
prop_fromSet_get (applyFun -> f) ks k =
    MonoidMap.get k (MonoidMap.fromSet f ks)
        ===
        (if Set.member k ks then f k else mempty)
    & cover 0.2
        (Set.member k ks && Null.null (f k))
        "Set.member k ks && Null.null (f k)"
    & cover 8.0
        (Set.member k ks && not (Null.null (f k)))
        "Set.member k ks && not (Null.null (f k))"
    & cover 0.2
        (not (Set.member k ks) && Null.null (f k))
        "not (Set.member k ks) && Null.null (f k)"
    & cover 8.0
        (not (Set.member k ks) && not (Null.null (f k)))
        "not (Set.member k ks) && not (Null.null (f k))"
