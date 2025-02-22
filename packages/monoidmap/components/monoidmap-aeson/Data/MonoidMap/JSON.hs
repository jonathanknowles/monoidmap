{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides instances of 'ToJSON' and 'FromJSON' for 'MonoidMap'.
--
-- These instances provide objects of type 'MonoidMap' __@k@__ __@v@__ with a
-- JSON encoding identical to that of 'Map' __@k@__ __@v@__, serialised as
-- either JSON /objects/ or /arrays/ depending on the key type __@k@__.
--
-- = Examples
--
-- === Encoding as JSON objects
--
-- @
-- >>> 'encode' '$' 'fromList' [("abc", 'Sum' 1), ("def", 'Sum' 2)]
-- "{\\"abc\\":1,\\"def\\":2}"
-- @
--
-- === Encoding as JSON arrays
--
-- @
-- >>> 'encode' '$' 'fromList' [((1,2), 'Sum' 3), ((2,3), 'Sum' 5)]
-- "[[[1,2],3],[[2,3],5]]"
-- @
--
-- = Laws
--
-- == Decoding from JSON
--
-- The 'FromJSON' instance satisfies the following law:
--
-- @
-- 'parseJSON' '==' 'fmap' ('fmap' 'MonoidMap'.'fromMap') 'parseJSON'
-- @
--
-- Mappings from keys to values that decode to 'mempty' are __not__ included in
-- decoded 'MonoidMap' objects.
--
-- == Encoding to JSON
--
-- The 'ToJSON' instance satisfies the following laws:
--
-- @
-- 'toEncoding' '==' 'toEncoding' '.' 'MonoidMap'.'toMap'
-- 'toJSON'     '==' 'toJSON'     '.' 'MonoidMap'.'toMap'
-- @
--
module Data.MonoidMap.JSON
    ()
    where

import Prelude

import Data.Aeson
    ( FromJSON (parseJSON)
    , FromJSONKey
    , ToJSON (toEncoding, toJSON)
    , ToJSONKey
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.MonoidMap
    ( MonoidMap
    )

import qualified Data.MonoidMap as MonoidMap

instance
    ( ToJSONKey k
    , ToJSON v
    )
    => ToJSON (MonoidMap k v)
  where
    toEncoding = toEncoding . MonoidMap.toMap
    toJSON = toJSON . MonoidMap.toMap

instance
    ( FromJSONKey k
    , Ord k
    , FromJSON v
    , MonoidNull v
    )
    => FromJSON (MonoidMap k v)
  where
    parseJSON = fmap (fmap MonoidMap.fromMap) parseJSON
