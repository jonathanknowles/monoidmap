-- |
-- Copyright: © 2025 Jonathan Knowles
-- License: Apache-2.0
module Data.MonoidMap.QuickCheck.Instances.CoArbitrary ()
where

import Data.Function
    ( (.)
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Test.QuickCheck
    ( CoArbitrary (coarbitrary)
    )

import qualified Data.MonoidMap as MonoidMap

instance
    ( CoArbitrary k
    , CoArbitrary v
    )
    => CoArbitrary (MonoidMap k v)
  where
    coarbitrary = coarbitrary . MonoidMap.toMap
