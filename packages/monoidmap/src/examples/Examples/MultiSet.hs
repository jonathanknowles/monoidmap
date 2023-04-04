-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- A multiset type, implemented in terms of 'MonoidMap'.
--
module Examples.MultiSet
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
    , difference
    , subtract
    , subtractMaybe
    )
    where

import Prelude hiding
    ( null, subtract )

import Data.List
    ( genericReplicate )
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
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive (isPrefixOf)
    , Reductive ((</>))
    , RightCancellative
    , RightReductive
    )
import Data.Set
    ( Set )
import Data.Total.MonoidMap
    ( MonoidMap )
import Numeric.Natural
    ( Natural )
import Text.Read
    ( Read (..) )

import qualified Data.Foldable as F
import qualified Data.Monoid.GCD as GCDMonoid
import qualified Data.Monoid.LCM as LCMMonoid
import qualified Data.Total.MonoidMap as MonoidMap

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

fromList :: Ord a => [a] -> MultiSet a
fromList = MultiSet . MonoidMap.fromList . fmap (, Sum 1)

toList :: MultiSet a -> [a]
toList (MultiSet m) =
    [a | (k, Sum v) <- MonoidMap.toList m, a <- genericReplicate v k]

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
isSubsetOf = isPrefixOf

intersection :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersection = GCDMonoid.gcd

union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union = LCMMonoid.lcm

difference :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
difference m1 m2 = (m1 <\> m2) <> (m2 <\> m1)

subtract :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
subtract = (<\>)

subtractMaybe :: Ord a => MultiSet a -> MultiSet a -> Maybe (MultiSet a)
subtractMaybe = (</>)
