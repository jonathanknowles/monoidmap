{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.JSONSpec
    ( spec
    )
where

import Prelude

import Control.Monad
    ( forM_
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap.JSON
    (
    )
import Data.Proxy
    ( Proxy (Proxy)
    )
import Data.Text
    ( Text
    )
import Test.Aeson.Internal.GoldenSpecs
    ( goldenSpecs
    )
import Test.Common
    ( Test
    , TestKey
    , TestValueType (TestValueType)
    , makeSpec
    , testValueTypesAll
    )
import Test.Hspec
    ( Spec
    , describe
    )
import Test.QuickCheck.Classes
    ( jsonLaws
    )
import Test.QuickCheck.Classes.Hspec
    ( testLaws
    )

import qualified Test.Aeson.Internal.Utils as Golden

spec :: Spec
spec = do
    describe "JSON"
        $ forM_ testKeyValueTypes
        $ \(TestKeyType k, TestValueType v) -> specForTypes k v
  where
    testKeyValueTypes =
        [(kt, vt) | kt <- testKeyTypes, vt <- testValueTypesAll]

specForTypes :: forall k v. (Test k v) => Proxy k -> Proxy v -> Spec
specForTypes = makeSpec $ do
    testLaws @(MonoidMap k v) jsonLaws
    goldenSpecs goldenSettings (Proxy @(MonoidMap k v))

goldenSettings :: Golden.Settings
goldenSettings =
    Golden.defaultSettings
        { Golden.goldenDirectoryOption =
            Golden.CustomDirectoryName "golden"
        , Golden.comparisonFile =
            Golden.OverwriteGoldenFile
        , Golden.randomMismatchOption =
            Golden.RandomMismatchError
        , Golden.useModuleNameAsSubDirectory =
            False
        , Golden.sampleSize =
            10
        }

data TestKeyType = forall k. (TestKey k) => TestKeyType (Proxy k)

testKeyTypes :: [TestKeyType]
testKeyTypes =
    mconcat [testKeyTypes_textual, testKeyTypes_nonTextual]
  where
    -- A selection of key types for which keys are encoded as JSON strings.
    -- For these types, 'MonoidMap' objects are encoded as JSON objects.
    testKeyTypes_textual =
        [ TestKeyType (Proxy @Int)
        , TestKeyType (Proxy @Text)
        ]
    -- A selection of key types for which keys are NOT encoded as JSON strings.
    -- For these types, 'MonoidMap' objects are encoded as JSON arrays.
    testKeyTypes_nonTextual =
        [ TestKeyType (Proxy @[Int])
        , TestKeyType (Proxy @(Int, Int))
        ]
