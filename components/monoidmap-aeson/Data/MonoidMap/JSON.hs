{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.JSON where

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
