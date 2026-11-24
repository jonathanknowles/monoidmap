{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2023–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.NoThunks
    ()
    where

import Data.MonoidMap
    ( MonoidMap, toMap )
import NoThunks.Class
    ( NoThunks (showTypeOf, wNoThunks) )
import Prelude
    ( (.) )

-- | @since 0.0.0.0
instance (NoThunks k, NoThunks v) => NoThunks (MonoidMap k v) where
    showTypeOf _ = "MonoidMap"
    {-# INLINE showTypeOf #-}
    wNoThunks ctx = wNoThunks ctx . toMap
    {-# INLINE wNoThunks #-}
