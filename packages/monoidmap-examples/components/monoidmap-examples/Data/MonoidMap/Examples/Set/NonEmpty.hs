-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- A minimal non-empty variant of the 'Set' data type.
--
module Data.MonoidMap.Examples.Set.NonEmpty
    ( NESet
    , nonEmptySet
    , toSet
    , isSubsetOf
    , union
    , intersection
    ) where

import Prelude

import Data.Coerce
    ( coerce )
import Data.Set
    ( Set )

import qualified Data.Set as Set

newtype NESet v = NESet (Set v)
    deriving stock Eq
    deriving newtype (Semigroup, Show)

nonEmptySet :: Set v -> Maybe (NESet v)
nonEmptySet s
    | Set.null s = Nothing
    | otherwise = Just (NESet s)

toSet :: NESet v -> Set v
toSet = coerce

isSubsetOf :: Ord v => NESet v -> NESet v -> Bool
isSubsetOf = coerce Set.isSubsetOf

union :: Ord v => NESet v -> NESet v -> NESet v
union = coerce Set.union

intersection :: Ord v => NESet v -> NESet v -> Set v
intersection = coerce Set.intersection
