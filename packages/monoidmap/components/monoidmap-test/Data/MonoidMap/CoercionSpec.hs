{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Tests for coercions of 'MonoidMap' values.
--
module Data.MonoidMap.CoercionSpec
    ( spec
    ) where

import Prelude

import Data.Coerce
    ( coerce )
import Data.Monoid
    ( Product (..), Sum (..) )
import Data.MonoidMap
    ( MonoidMap )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Hspec.TypeError
    ( shouldFailToTypecheckWith )

import qualified Data.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Coercion" $ do

    let source :: MonoidMap Char (Sum Int)
        source = MonoidMap.singleton 'a' (Sum 1)

    it "permits coercion to the same value type" $
        (coerce source :: MonoidMap Char (Sum Int))
            `shouldBe` source

    it "forbids coercion to a different value type" $
        (coerce source :: MonoidMap Char (Product Int))
            `shouldFailToTypecheckWith`
            ["Couldn't match type", "Sum Int", "Product Int", "coerce"]
