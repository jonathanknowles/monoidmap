-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Test.Combinators.NonZero
    ( NonZero
    , genNonZero
    , getNonZero
    , maybeNonZero
    , shrinkNonZero
    )
    where

import Prelude

import Data.Group
    ( Group )
import Data.Maybe
    ( mapMaybe )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Semigroup.Cancellative
    ( Commutative )
import Test.QuickCheck
    ( Gen, suchThatMap )

-- | A combinator for non-zero values.
newtype NonZero a = NonZero a
    deriving newtype (Eq, Num, Read, Show)
    deriving newtype (Semigroup, Commutative, Monoid, MonoidNull, Group)

genNonZero :: (Eq a, Num a) => Gen a -> Gen (NonZero a)
genNonZero genA = suchThatMap genA maybeNonZero

getNonZero :: NonZero a -> a
getNonZero (NonZero a) = a

maybeNonZero :: (Eq a, Num a) => a -> Maybe (NonZero a)
maybeNonZero p
    | p == 0 = Nothing
    | otherwise = Just (NonZero p)

shrinkNonZero :: (Eq a, Num a) => (a -> [a]) -> NonZero a -> [NonZero a]
shrinkNonZero shrinkA = mapMaybe maybeNonZero . shrinkA . getNonZero
