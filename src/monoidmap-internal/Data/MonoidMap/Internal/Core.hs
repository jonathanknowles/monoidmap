-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.Core
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

import Prelude hiding
    ( null )

import Control.DeepSeq
    ( NFData )
import Data.Bifoldable
    ( Bifoldable )
import Data.Functor.Classes
    ( Eq1, Eq2, Show1, Show2 )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Monoid.Null
    ( MonoidNull (..) )

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype MonoidMap k v = MonoidMap
    { unMonoidMap :: Map k v }
    deriving newtype
        (Bifoldable, Eq, Eq1, Eq2, Foldable, NFData, Show, Show1, Show2)

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

set :: (Ord k, MonoidNull v) => MonoidMap k v -> k -> v -> MonoidMap k v
set m k v
    | null v    = MonoidMap $ Map.delete k   $ unMonoidMap m
    | otherwise = MonoidMap $ Map.insert k v $ unMonoidMap m