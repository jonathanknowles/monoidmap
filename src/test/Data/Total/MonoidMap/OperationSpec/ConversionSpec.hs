{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.ConversionSpec
    ( spec
    ) where

import Prelude

import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
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
    ( MonoidMap, nonNullCount )
import Data.Typeable
    ( typeRep )
import Numeric.Natural
    ( Natural )
import Test.Common
    ( Key, TestConstraints, property )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..), Property, applyFun2, cover, (===) )

import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Conversions" $ do

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
        it "prop_fromMap_toMap" $
            prop_fromMap_toMap
                @k @v & property
        it "prop_toMap_fromMap" $
            prop_toMap_fromMap
                @k @v & property

--------------------------------------------------------------------------------
-- Conversion to and from lists
--------------------------------------------------------------------------------

prop_fromList_get
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => [(k, v)]
    -> k
    -> Property
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
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => [(k, v)]
    -> Property
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
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => [(k, v)]
    -> Property
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
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => MonoidMap k v
    -> Property
prop_toList_fromList m =
    MonoidMap.fromList (MonoidMap.toList m) === m
    & cover 2
        (MonoidMap.nonNull m)
        "MonoidMap.nonNull m"

prop_toList_sort
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => MonoidMap k v
    -> Property
prop_toList_sort m =
    List.sortOn fst (MonoidMap.toList m) === MonoidMap.toList m
    & cover 2
        (MonoidMap.nonNull m)
        "MonoidMap.nonNull m"

prop_fromListWith_get
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun (v, v) v
    -> [(k, v)]
    -> k
    -> Property
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

prop_fromMap_toMap
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Map k v
    -> Property
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

prop_toMap_fromMap
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => MonoidMap k v
    -> Property
prop_toMap_fromMap m =
    MonoidMap.fromMap (MonoidMap.toMap m) === m
