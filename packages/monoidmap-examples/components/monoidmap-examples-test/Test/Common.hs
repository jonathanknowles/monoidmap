{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Test.Common
    () where

import Prelude

import Data.Text
    ( Text )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Function (..)
    , arbitrarySizedNatural
    , coarbitraryIntegral
    , coarbitraryShow
    , frequency
    , functionIntegral
    , functionShow
    , listOf
    , shrinkIntegral
    )

import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

#if !MIN_VERSION_QuickCheck(2,17,0)
instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral
#endif

#if !MIN_VERSION_QuickCheck(2,18,0)
instance CoArbitrary Natural where
    coarbitrary = coarbitraryIntegral
#endif

#if !MIN_VERSION_QuickCheck(2,18,0)
instance Function Natural where
    function = functionIntegral
#endif

instance Arbitrary Text where
    arbitrary = Text.pack <$> listOf genChar
      where
        genChar = frequency
            [ (64, pure 'a')
            , (16, pure 'b')
            , ( 4, pure 'c')
            , ( 1, pure 'd')
            ]

instance CoArbitrary Text where
    coarbitrary = coarbitraryShow

instance Function Text where
    function = functionShow
