-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- An example multimap class.
--
module Examples.MultiMap where

import Prelude hiding
    ( lookup )

import Data.Set
    ( Set )

-- | Specifies a multimap from unique keys to sets of values.
--
class (Eq (m k v), Ord k, Ord v) => MultiMap m k v where

    -- | Constructs an empty multimap.
    --
    -- @
    -- ∀ k. lookup k empty == Set.empty
    -- @
    --
    empty :: m k v

    -- | Constructs a multimap from a list.
    --
    -- @
    -- fromList (toList m) == m
    -- @
    --
    fromList :: [(k, Set v)] -> m k v

    -- | Converts a multimap to a list.
    --
    -- @
    -- fromList (toList m) == m
    -- @
    --
    toList :: m k v -> [(k, Set v)]

    -- | Returns the set of values associated with a given key.
    --
    -- @
    -- lookup k (fromList [(k, vs)]) == vs
    -- @
    --
    lookup :: k -> m k v -> Set v

    -- | Updates the set of values associated with a given key.
    --
    -- @
    -- lookup k (update k vs m) == vs
    -- @
    --
    update :: k -> Set v -> m k v -> m k v

    -- | Returns 'True' iff. the given key is not associated with the empty set.
    --
    -- @
    -- nonNullKey k m ==> (lookup k m /= Set.empty)
    -- @
    --
    nonNullKey :: k -> m k v -> Bool

    -- | Returns the set of keys that are not associated with empty sets.
    --
    -- @
    -- all (\k -> lookup k m /= Set.empty) (nonNullKeys m)
    -- @
    --
    nonNullKeys :: m k v -> Set k

    -- | Indicates how many keys are associated with non-empty sets.
    --
    -- @
    -- nonNullCount m == Set.size (nonNullKeys m)
    -- @
    --
    nonNullCount :: m k v -> Int

    -- | Indicates whether or not a multimap is empty.
    --
    -- @
    -- null m ==> (∀ k. lookup k m == Set.empty)
    -- @
    --
    null :: m k v -> Bool

    -- | Inserts values into the set of values associated with a given key.
    --
    -- @
    -- lookup k (insert k vs m) == lookup k m `Set.union` vs
    -- @
    --
    insert :: k -> Set v -> m k v -> m k v

    -- | Removes values from the set of values associated with a given key.
    --
    -- @
    -- lookup k (remove k vs m) == lookup k m `Set.difference` vs
    -- @
    --
    remove :: k -> Set v -> m k v -> m k v

    -- | Computes the union of two indices.
    --
    -- @
    -- lookup k (m1 `union` m2)
    -- ==
    -- lookup k m1 `Set.union` lookup k m2
    -- @
    --
    union :: m k v -> m k v -> m k v

    -- | Computes the intersection of two indices.
    --
    -- @
    -- lookup k (m1 `intersection` m2)
    -- ==
    -- lookup k m1 `Set.intersection` lookup k m2
    -- @
    --
    intersection :: m k v -> m k v -> m k v

    -- | Indicates whether or not the first multimap is a sub-map of the second.
    --
    -- @
    -- m1 `isSubMultiMapOf` m2 ==> lookup k m1 `Set.isSubsetOf` lookup k m2
    -- @
    --
    isSubMultiMapOf :: m k v -> m k v -> Bool
