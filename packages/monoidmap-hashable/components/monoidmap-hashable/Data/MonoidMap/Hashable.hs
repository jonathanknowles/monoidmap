{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2025–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Hashable
    (
    -- * Introduction
    -- $_introduction
    )
    where

import Data.Bool
    ( Bool
    )
import Data.Eq
    ( Eq ((==))
    )
import Data.Hashable
    ( Hashable (hash, hashWithSalt)
    )
import Data.Map.Strict
    ( Map
    )
import Data.MonoidMap
    ( MonoidMap
    , toMap
    )
import Prelude
    ( undefined
    , (.)
    )

-- $_introduction
-- #_introduction#
--
-- This module provides a 'Hashable' instance for 'MonoidMap'.
--
-- This instance behaves identically to the 'Hashable' instance for 'Map',
-- satisfying the following the laws:
--
-- @
-- 'hash'           '==' 'hash'           '.' 'MonoidMap'.'toMap'
-- 'hashWithSalt' i '==' 'hashWithSalt' i '.' 'MonoidMap'.'toMap'
-- @
--

_importsRequiredForDocumentation :: ()
_importsRequiredForDocumentation = ()
  where
    _Map :: Map () ()
    _Map = undefined

    _equals :: () -> () -> Bool
    _equals = (==)

instance (Hashable k, Hashable v) => Hashable (MonoidMap k v)
  where
    hash           = hash           . toMap
    hashWithSalt i = hashWithSalt i . toMap
