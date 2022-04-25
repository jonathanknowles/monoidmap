{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal
    (
--  * Type
      MonoidMap

--  * Construction
    , empty

--  * Deconstruction
    , toMap

--  * Operations
    , get
    , set
    )
    where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import GHC.Generics
    ( Generic )

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype MonoidMap k v = MonoidMap
    { unMonoidMap :: Map k v }
    deriving (Eq, Generic)
    deriving newtype (Read, Show)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

empty :: MonoidMap k v
empty = MonoidMap Map.empty

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

toMap :: MonoidMap k v -> Map k v
toMap = unMonoidMap

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

get :: (Ord k, Monoid v) => MonoidMap k v -> k -> v
get m k = fromMaybe mempty $ Map.lookup k $ toMap m

set :: (Ord k, Eq v, Monoid v) => MonoidMap k v -> k -> v -> MonoidMap k v
set m k v
    | v == mempty = MonoidMap $ Map.delete k   $ unMonoidMap m
    | otherwise   = MonoidMap $ Map.insert k v $ unMonoidMap m
