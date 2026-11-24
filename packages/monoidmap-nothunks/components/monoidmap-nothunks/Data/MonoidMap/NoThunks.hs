{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2023–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.NoThunks
    (
    -- * Introduction
    -- $_introduction
    )
    where

import Data.Map.Strict
    ( Map )
import Data.MonoidMap
    ( MonoidMap, toMap )
import NoThunks.Class
    ( NoThunks (showTypeOf, wNoThunks) )
import Prelude
    ( undefined, (.) )

-- $_introduction
-- #_introduction#
--
-- This module provides a 'NoThunks' instance for 'MonoidMap'.
--
-- This instance behaves like the 'NoThunks' instance for 'Map', except that
-- when it finds a thunk, it names 'MonoidMap' instead of 'Map' in the context
-- that it reports.
--
-- To bring this instance into scope, import this module:
--
-- @
-- import "Data.MonoidMap.NoThunks" ()
-- @
--

_importsRequiredForDocumentation :: ()
_importsRequiredForDocumentation = ()
  where
    _Map :: Map () ()
    _Map = undefined

-- | @since 0.0.0.0
instance (NoThunks k, NoThunks v) => NoThunks (MonoidMap k v) where
    showTypeOf _ = "MonoidMap"
    {-# INLINE showTypeOf #-}
    wNoThunks ctx = wNoThunks ctx . toMap
    {-# INLINE wNoThunks #-}
