{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- An ordinary left-biased map similar to 'Map', implemented in terms of
-- 'MonoidMap'.
--
module Examples.RecoveredMap where

import Prelude hiding (null)

import Control.DeepSeq
    ( NFData (..) )
import Data.Maybe
    ( mapMaybe )
import Data.Monoid.Null
    ( MonoidNull (..) )
import Data.Set
    ( Set )
import Data.Total.MonoidMap
    ( MonoidMap )

import qualified Data.Total.MonoidMap as MonoidMap

newtype Map k v = Map
    {unMap :: MonoidMap k (First v)}
    deriving stock Eq
    deriving newtype (NFData, Semigroup, Monoid)

data First a
    = FirstNull
    | FirstJust !a
    deriving (Eq, Functor)

firstToMaybe :: First a -> Maybe a
firstToMaybe = \case
    FirstNull -> Nothing
    FirstJust a -> Just a

instance Applicative First where
    pure a = FirstJust a
    FirstNull <*> _ = FirstNull
    FirstJust f <*> something = fmap f something

instance Semigroup (First a) where
    FirstNull <> a = a
    FirstJust a <> _ = FirstJust a

instance Monoid (First a) where
    mempty = FirstNull

instance MonoidNull (First a) where
    null FirstNull = True
    null _ = False

instance NFData a => NFData (First a) where
    rnf FirstNull = ()
    rnf (FirstJust a) = rnf a

instance (Show k, Show v) => Show (Map k v) where
    show = ("fromList " <>) . show . toList

empty :: Map k v
empty = Map MonoidMap.empty

singleton :: Ord k => k -> v -> Map k v
singleton k = Map . MonoidMap.singleton k . pure

fromList :: Ord k => [(k, v)] -> Map k v
fromList = Map . MonoidMap.fromListWith (const id) . fmap (fmap pure)

toList :: Map k v -> [(k, v)]
toList = mapMaybe (firstToMaybe . sequenceA) . MonoidMap.toList . unMap

delete :: Ord k => k -> Map k v -> Map k v
delete k = Map . MonoidMap.nullify k . unMap

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v = Map . MonoidMap.set k (pure v) . unMap

keysSet :: Map k v -> Set k
keysSet = MonoidMap.nonNullKeys . unMap

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k = firstToMaybe . MonoidMap.get k . unMap

member :: Ord k => k -> Map k v -> Bool
member k = MonoidMap.nonNullKey k . unMap
