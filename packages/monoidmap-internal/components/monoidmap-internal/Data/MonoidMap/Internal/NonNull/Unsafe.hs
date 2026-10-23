{-# LANGUAGE RoleAnnotations #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Defines the 'NonNull' type, together with the primitive operations on
-- 'NonNull' values that need access to its data constructor.
--
module Data.MonoidMap.Internal.NonNull.Unsafe
    ( NonNull (UnsafeNonNull)
    , nonNull
    , unwrap
    )
    where

import Prelude hiding
    ( null )

import Control.DeepSeq
    ( NFData )
import Data.Monoid.Null
    ( MonoidNull (null) )

-- | A value that is not 'null'.
--
newtype NonNull v = UnsafeNonNull v
    deriving newtype NFData

-- Without an explicit type role annotation, the following role would be
-- inferred:
--
--     type role NonNull representational
--
-- However, the validity of a 'NonNull' value depends on a property of its
-- type: its 'mempty' value. A representational role would allow users to
-- coerce between types with the same representation but different 'mempty'
-- values, even without access to the 'NonNull' constructor.
--
-- For example, users could coerce a 'NonNull' ('Sum' 'Int') to a 'NonNull'
-- ('Product' 'Int'), and thereby construct a 'NonNull' value that is 'mempty'.
--
-- So we must instead choose a nominal role.
--
type role NonNull nominal

-- | Converts a value into a 'NonNull' value, returning 'Nothing' if the
--   value is 'null'.
--
nonNull :: MonoidNull v => v -> Maybe (NonNull v)
nonNull !v
    | null v    = Nothing
    | otherwise = Just (UnsafeNonNull v)
{-# INLINE nonNull #-}

-- | Extracts the value from a 'NonNull' value.
--
unwrap :: NonNull v -> v
unwrap (UnsafeNonNull v) = v
{-# INLINE unwrap #-}
