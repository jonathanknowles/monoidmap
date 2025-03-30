-- |
-- Copyright: © 2025 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.QuickCheck.Instances.Function ()
where

import Data.Monoid.Null
    ( MonoidNull
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.Ord
    ( Ord
    )
import Test.QuickCheck
    ( Function (function)
    , functionMap
    )

import qualified Data.MonoidMap as MonoidMap

instance
    ( Function k
    , Function v
    , Ord k
    , MonoidNull v
    )
    => Function (MonoidMap k v)
  where
    function = functionMap MonoidMap.toMap MonoidMap.fromMap
