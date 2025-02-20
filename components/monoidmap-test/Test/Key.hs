{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- Quasi-unique keys.
--
module Test.Key
    ( Key1
    , Key2
    , Key4
    , Key8
    )
where

import Prelude

import Data.Aeson.Types
    ( FromJSON (parseJSON)
    , FromJSONKey (fromJSONKey)
    , ToJSON (toEncoding, toJSON)
    , ToJSONKey (toJSONKey)
    , toJSONKeyText
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )
import GHC.TypeLits
    ( Nat
    )
import Test.QuickCheck
    ( Arbitrary
    , CoArbitrary
    , Function
    )
import Test.QuickCheck.Quid
    ( Latin (Latin)
    , Quid
    , Size (Size)
    )

import qualified Data.Text as Text

newtype Key (size :: Nat) = Key Quid
    deriving stock (Eq, Generic, Ord)
    deriving (Read, Show) via Latin Quid
    deriving (Arbitrary) via Size size Quid
    deriving (CoArbitrary) via Quid
    deriving anyclass (Function)

type Key1 = Key 1
type Key2 = Key 2
type Key4 = Key 4
type Key8 = Key 8

instance ToJSON (Key size) where
    toEncoding = toEncoding . toUnquotedText
    toJSON = toJSON . toUnquotedText

instance ToJSONKey (Key size) where
    toJSONKey = toJSONKeyText toUnquotedText

instance FromJSON (Key size) where
    parseJSON = fmap (fmap unsafeFromUnquotedText) parseJSON

instance FromJSONKey (Key size) where
    fromJSONKey = fmap unsafeFromUnquotedText fromJSONKey

toUnquotedText :: Key size -> Text
toUnquotedText = Text.dropAround (== '\"') . Text.pack . show

unsafeFromUnquotedText :: Text -> Key size
unsafeFromUnquotedText = read . show . Text.unpack
