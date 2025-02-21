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
    , FromJSONKeyFunction (FromJSONKeyTextParser)
    , Parser
    , ToJSON (toEncoding, toJSON)
    , ToJSONKey (toJSONKey)
    , toJSONKeyText
    , withText
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
import Text.Read
    ( readMaybe
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
    toEncoding = toEncoding . toText
    toJSON = toJSON . toText

instance ToJSONKey (Key size) where
    toJSONKey = toJSONKeyText toText

instance FromJSON (Key size) where
    parseJSON = withText "Key" parseFromText

instance FromJSONKey (Key size) where
    fromJSONKey = FromJSONKeyTextParser parseFromText

toText :: Key size -> Text
toText = Text.dropAround (== '\"') . Text.pack . show

maybeFromText :: Text -> Maybe (Key size)
maybeFromText = readMaybe . show . Text.unpack

parseFromText :: Text -> Parser (Key size)
parseFromText =
    maybe (fail failureMessage) pure . maybeFromText
  where
    failureMessage = "Failed to parse key from JSON"
