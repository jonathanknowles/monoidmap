-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- A multiset type, implemented in terms of 'MonoidMap'.
--
-- See: https://en.wikipedia.org/wiki/Multiset
--
module Data.MonoidMap.Examples.MultiSet
    ( fromList
    , toList
    , null
    , member
    , multiplicity
    , root
    , cardinality
    , dimension
    , height
    , isSubsetOf
    , intersection
    , union
    , disjointUnion
    , add
    , subtract
    , subtractMaybe
    )
    where

import Prelude hiding
    ( null, subtract )

import Data.Function
    ( on )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.GCD
    ( DistributiveGCDMonoid
    , GCDMonoid
    , LeftDistributiveGCDMonoid
    , LeftGCDMonoid
    , OverlappingGCDMonoid
    , RightDistributiveGCDMonoid
    , RightGCDMonoid
    )
import Data.Monoid.LCM
    ( DistributiveLCMMonoid, LCMMonoid )
import Data.Monoid.Monus
    ( Monus ((<\>)) )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
import Data.MonoidMap
    ( MonoidMap )
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive
    , Reductive ((</>))
    , RightCancellative
    , RightReductive
    )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Text.Read
    ( Read (..) )

import qualified Data.Foldable as F
import qualified Data.MonoidMap as MonoidMap

newtype MultiSet a = MultiSet
    { unMultiSet :: MonoidMap a (Sum Natural)
    }
    deriving newtype
        ( Eq
        , Semigroup
        , Commutative
        , Monoid
        , MonoidNull
        , PositiveMonoid
        , LeftReductive
        , LeftCancellative
        , LeftGCDMonoid
        , LeftDistributiveGCDMonoid
        , RightReductive
        , RightCancellative
        , RightGCDMonoid
        , RightDistributiveGCDMonoid
        , Reductive
        , Cancellative
        , GCDMonoid
        , LCMMonoid
        , DistributiveGCDMonoid
        , DistributiveLCMMonoid
        , OverlappingGCDMonoid
        , Monus
        )

instance (Ord a, Read a) => Read (MultiSet a) where
    readPrec = fromList <$> readPrec

instance Show a => Show (MultiSet a) where
    show = show . toList

fromList :: Ord a => [(a, Natural)] -> MultiSet a
fromList = MultiSet . MonoidMap.fromList . fmap (fmap Sum)

toList :: MultiSet a -> [(a, Natural)]
toList = fmap (fmap getSum) . MonoidMap.toList . unMultiSet

null :: MultiSet a -> Bool
null = MonoidMap.null . unMultiSet

member :: Ord a => a -> MultiSet a -> Bool
member a = MonoidMap.nonNullKey a . unMultiSet

multiplicity :: Ord a => a -> MultiSet a -> Natural
multiplicity a = getSum . MonoidMap.get a . unMultiSet

root :: Ord a => MultiSet a -> Set a
root = MonoidMap.nonNullKeys . unMultiSet

cardinality :: MultiSet a -> Natural
cardinality = getSum . F.fold . unMultiSet

dimension :: MultiSet a -> Natural
dimension = fromIntegral . MonoidMap.nonNullCount . unMultiSet

height :: Ord a => MultiSet a -> Natural
height s
    | null s = 0
    | otherwise = getSum $ F.maximum $ unMultiSet s

isSubsetOf :: Ord a => MultiSet a -> MultiSet a -> Bool
isSubsetOf = MonoidMap.isSubmapOf `on` unMultiSet

intersection :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersection (MultiSet s1) (MultiSet s2) =
    MultiSet (MonoidMap.intersection s1 s2)

union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet s1) (MultiSet s2) =
    MultiSet (MonoidMap.union s1 s2)

disjointUnion :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
disjointUnion m1 m2 = (m1 <\> m2) <> (m2 <\> m1)

add :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
add = (<>)

subtract :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
subtract = (<\>)

subtractMaybe :: Ord a => MultiSet a -> MultiSet a -> Maybe (MultiSet a)
subtractMaybe = (</>)
