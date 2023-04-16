{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.FilterSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Proxy
    ( Proxy (..) )
import Data.Total.MonoidMap
    ( MonoidMap, nonNullCount )
import Data.Typeable
    ( typeRep )
import GHC.Exts
    ( IsList (..) )
import Test.Common
    ( Key
    , TestConstraints
    , TestInstance (TestInstance)
    , property
    , testInstancesMonoidNull
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun (..), Property, applyFun, applyFun2, cover, (===) )

import qualified Data.List as List
import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Filtering" $ do

    forM_ testInstancesMonoidNull $ \(TestInstance p) -> specFor (Proxy @Key) p

specFor
    :: forall k v. TestConstraints k v
    => Proxy k
    -> Proxy v
    -> Spec
specFor _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do

    it "prop_filter_get" $
        prop_filter_get
            @k @v & property
    it "prop_filter_asList" $
        prop_filter_asList
            @k @v & property
    it "prop_filterKeys_get" $
        prop_filterKeys_get
            @k @v & property
    it "prop_filterKeys_asList" $
        prop_filterKeys_asList
            @k @v & property
    it "prop_filterWithKey_get" $
        prop_filterWithKey_get
            @k @v & property
    it "prop_filterWithKey_asList" $
        prop_filterWithKey_asList
            @k @v & property

prop_filter_get
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun v Bool
    -> k
    -> MonoidMap k v
    -> Property
prop_filter_get (applyFun -> f) k m =
    MonoidMap.get k (MonoidMap.filter f m)
        ===
        (MonoidMap.get k m & \v -> if f v then v else mempty)
    & cover 2
        (MonoidMap.nullKey k m && f (MonoidMap.get k m))
        "MonoidMap.nullKey k m && f (MonoidMap.get k m)"
    & cover 2
        (MonoidMap.nullKey k m && not (f (MonoidMap.get k m)))
        "MonoidMap.nullKey k m && not (f (MonoidMap.get k m))"
    & cover 2
        (MonoidMap.nonNullKey k m && f (MonoidMap.get k m))
        "MonoidMap.nonNullKey k m && f (MonoidMap.get k m)"
    & cover 2
        (MonoidMap.nonNullKey k m && not (f (MonoidMap.get k m)))
        "MonoidMap.nonNullKey k m && not (f (MonoidMap.get k m))"

prop_filter_asList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun v Bool
    -> MonoidMap k v
    -> Property
prop_filter_asList (applyFun -> f) m =
    n === fromList (List.filter (f . snd) (toList m))
    & cover 2
        (MonoidMap.nonNull n && nonNullCount n == nonNullCount m)
        "MonoidMap.nonNull n && nonNullCount n == nonNullCount m"
    & cover 2
        (MonoidMap.nonNull n && nonNullCount n /= nonNullCount m)
        "MonoidMap.nonNull n && nonNullCount n /= nonNullCount m"
  where
    n = MonoidMap.filter f m

prop_filterKeys_get
    :: (Ord k, Show k, Eq v, Monoid v, Show v)
    => Fun k Bool
    -> k
    -> MonoidMap k v
    -> Property
prop_filterKeys_get (applyFun -> f) k m =
    MonoidMap.get k (MonoidMap.filterKeys f m)
        ===
        (if f k then MonoidMap.get k m else mempty)
    & cover 2
        (MonoidMap.nullKey k m && f k)
        "MonoidMap.nullKey k m && f k"
    & cover 2
        (MonoidMap.nullKey k m && not (f k))
        "MonoidMap.nullKey k m && not (f k)"
    & cover 2
        (MonoidMap.nonNullKey k m && f k)
        "MonoidMap.nonNullKey k m && f k"
    & cover 2
        (MonoidMap.nonNullKey k m && not (f k))
        "MonoidMap.nonNullKey k m && not (f k)"

prop_filterKeys_asList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun k Bool
    -> MonoidMap k v
    -> Property
prop_filterKeys_asList (applyFun -> f) m =
    n === MonoidMap.fromList (List.filter (f . fst) (toList m))
    & cover 2
        (MonoidMap.nonNull n && nonNullCount n == nonNullCount m)
        "MonoidMap.nonNull n && nonNullCount n == nonNullCount m"
    & cover 2
        (MonoidMap.nonNull n && nonNullCount n /= nonNullCount m)
        "MonoidMap.nonNull n && nonNullCount n /= nonNullCount m"
  where
    n = MonoidMap.filterKeys f m

prop_filterWithKey_get
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun (k, v) Bool
    -> k
    -> MonoidMap k v
    -> Property
prop_filterWithKey_get (applyFun2 -> f) k m =
    MonoidMap.get k (MonoidMap.filterWithKey f m)
        ===
        (MonoidMap.get k m & \v -> if f k v then v else mempty)
    & cover 2
        (MonoidMap.nullKey k m && f k (MonoidMap.get k m))
        "MonoidMap.nullKey k m && f k (MonoidMap.get k m)"
    & cover 2
        (MonoidMap.nullKey k m && not (f k (MonoidMap.get k m)))
        "MonoidMap.nullKey k m && not (f k (MonoidMap.get k m))"
    & cover 2
        (MonoidMap.nonNullKey k m && f k (MonoidMap.get k m))
        "MonoidMap.nonNullKey k m && f k (MonoidMap.get k m)"
    & cover 2
        (MonoidMap.nonNullKey k m && not (f k (MonoidMap.get k m)))
        "MonoidMap.nonNullKey k m && not (f k (MonoidMap.get k m))"

prop_filterWithKey_asList
    :: (Ord k, Show k, Eq v, MonoidNull v, Show v)
    => Fun (k, v) Bool
    -> MonoidMap k v
    -> Property
prop_filterWithKey_asList (applyFun2 -> f) m =
    n === MonoidMap.fromList (List.filter (uncurry f) (toList m))
    & cover 2
        (MonoidMap.nonNull n && nonNullCount n == nonNullCount m)
        "MonoidMap.nonNull n && nonNullCount n == nonNullCount m"
    & cover 2
        (MonoidMap.nonNull n && nonNullCount n /= nonNullCount m)
        "MonoidMap.nonNull n && nonNullCount n /= nonNullCount m"
  where
    n = MonoidMap.filterWithKey f m
