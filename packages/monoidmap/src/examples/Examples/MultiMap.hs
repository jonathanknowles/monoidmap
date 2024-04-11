{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Copyright: © 2022–2024 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides the 'MultiMap' class, which models a total relation from unique
-- keys to sets of values.
--
-- = Implementations
--
-- The following example implementations are provided:
--
-- +----------------+------------------------+---------+
-- | Implementation | Types used             | Lawful? |
-- +================+=============+==========+=========+
-- | 'MultiMap1'    | 'Map'       | 'Set'    | 💥 No    |
-- +----------------+-------------+----------+---------+
-- | 'MultiMap2'    | 'Map'       | 'Set'    | ✅ Yes   |
-- +----------------+-------------+----------+---------+
-- | 'MultiMap3'    | 'Map'       | 'NESet'  | ✅ Yes   |
-- +----------------+-------------+----------+---------+
-- | 'MultiMap4'    | 'MonoidMap' | 'Set'    | ✅ Yes   |
-- +----------------+-------------+----------+---------+
--
module Examples.MultiMap
    ( MultiMap (..)
    ) where

import Data.Map.Strict
    ( Map )
import Data.MonoidMap
    ( MonoidMap )
import Data.Set
    ( Set )
import Data.Set.NonEmpty
    ( NESet )
import Examples.MultiMap.Class
    ( MultiMap (..) )
import Examples.MultiMap.Instances.MultiMap1
    ( MultiMap1 )
import Examples.MultiMap.Instances.MultiMap2
    ( MultiMap2 )
import Examples.MultiMap.Instances.MultiMap3
    ( MultiMap3 )
import Examples.MultiMap.Instances.MultiMap4
    ( MultiMap4 )
