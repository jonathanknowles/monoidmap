{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.JSON
    (
    -- * Introduction
    -- $_introduction

    -- * Examples
    -- $_examples

    -- * Laws
    -- $_laws
    )
    where

import Data.Aeson
    ( FromJSON (parseJSON)
    , FromJSONKey
    , ToJSON (toEncoding, toJSON)
    , ToJSONKey
    , decode
    , encode
    )
import Data.Bool
    ( Bool
    )
import Data.Eq
    ( Eq ((==))
    )
import Data.Functor
    ( Functor (fmap)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( Maybe
    )
import Data.Monoid
    ( Monoid (mempty)
    , Sum
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.MonoidMap
    ( MonoidMap
    , fromList
    , fromMap
    , toMap
    )
import Data.Ord
    ( Ord
    )
import Prelude
    ( undefined
    , ($)
    , (.)
    )

-- $_introduction
-- #_introduction#
--
-- This module provides instances of 'ToJSON' and 'FromJSON' for 'MonoidMap'.
--
-- These instances provide objects of type 'MonoidMap' __@k@__ __@v@__ with a
-- JSON encoding that is /identical/ to objects of type 'Map' __@k@__ __@v@__,
-- which are serialised as either JSON /objects/ or /arrays/ depending on the
-- key type __@k@__.

-- $_examples
-- #_examples#
--
-- === Encoding as JSON objects
--
-- @
-- >>> 'encode' '$' 'MonoidMap'.'fromList' [("abc", 'Sum' 1), ("def", 'Sum' 2)]
--
-- "{\\"abc\\":1,\\"def\\":2}"
-- @
--
-- === Encoding as JSON arrays
--
-- @
-- >>> 'encode' '$' 'MonoidMap'.'fromList' [((1,2), 'Sum' 3), ((2,3), 'Sum' 5)]
--
-- "[[[1,2],3],[[2,3],5]]"
-- @

-- $_laws
-- #_laws#
--
-- === Encoding to JSON
--
-- The 'ToJSON' instance satisfies the following laws:
--
-- @
-- 'toEncoding' '==' 'toEncoding' '.' 'MonoidMap'.'toMap'
-- 'toJSON'     '==' 'toJSON'     '.' 'MonoidMap'.'toMap'
-- @
--
-- === Decoding from JSON
--
-- The 'FromJSON' instance satisfies the following law:
--
-- @
-- 'parseJSON' '==' 'fmap' ('fmap' 'MonoidMap'.'fromMap') 'parseJSON'
-- @
--
-- Mappings from keys to values that decode to 'mempty' are __not__ included in
-- decoded 'MonoidMap' objects.

_importsRequiredForDocumentation :: ()
_importsRequiredForDocumentation = ()
  where
    _decodeEncode :: Maybe ()
    _decodeEncode = decode $ encode ()

    _equals :: (Eq a) => a -> a -> Bool
    _equals = (==)

    _fromList :: (Ord k, MonoidNull v) => [(k, v)] -> MonoidMap k v
    _fromList = fromList

    _fromMap :: (Ord k, MonoidNull v) => Map k v -> MonoidMap k v
    _fromMap = fromMap

    _mempty :: (Monoid a) => a
    _mempty = mempty

    _toMap :: (Ord k, MonoidNull v) => MonoidMap k v -> Map k v
    _toMap = toMap

    _Sum :: Sum ()
    _Sum = undefined

instance
    ( ToJSONKey k
    , ToJSON v
    )
    => ToJSON (MonoidMap k v)
  where
    toEncoding = toEncoding . toMap
    toJSON = toJSON . toMap

instance
    ( FromJSONKey k
    , Ord k
    , FromJSON v
    , MonoidNull v
    )
    => FromJSON (MonoidMap k v)
  where
    parseJSON = fmap (fmap fromMap) parseJSON
