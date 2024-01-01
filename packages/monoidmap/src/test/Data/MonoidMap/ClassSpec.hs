{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.ClassSpec
    where

import Prelude

import Data.Group
    ( Group )
import Data.Maybe
    ( mapMaybe )
import Data.Monoid
    ( Product (..), Sum (..) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap
    ( MonoidMap )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( Commutative )
import Data.Set
    ( Set )
import Data.Typeable
    ( Typeable, typeRep )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe )
import Test.QuickCheck
    ( Arbitrary (..), listOf, scale, shrinkMapBy, suchThatMap )
import Test.QuickCheck.Classes
    ( eqLaws
    , isListLaws
    , monoidLaws
    , semigroupLaws
    , semigroupMonoidLaws
    , showReadLaws
    )
import Test.QuickCheck.Classes.Group
    ( groupLaws )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Classes.Monoid.GCD
    ( distributiveGCDMonoidLaws
    , gcdMonoidLaws
    , leftDistributiveGCDMonoidLaws
    , leftGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , rightDistributiveGCDMonoidLaws
    , rightGCDMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.LCM
    ( distributiveLCMMonoidLaws, lcmMonoidLaws )
import Test.QuickCheck.Classes.Monoid.Monus
    ( monusLaws )
import Test.QuickCheck.Classes.Monoid.Null
    ( monoidNullLaws, positiveMonoidLaws )
import Test.QuickCheck.Classes.Semigroup.Cancellative
    ( cancellativeLaws
    , commutativeLaws
    , leftCancellativeLaws
    , leftReductiveLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    )
import Test.QuickCheck.Instances.Natural
    ()
import Test.QuickCheck.Instances.Text
    ()

import qualified Data.MonoidMap as MonoidMap

spec :: Spec
spec = do
    describe "Class laws" $ do
        -- Test against a variety of key types, in ascending order of
        -- cardinality:
        specLawsFor (Proxy @Bool)
        specLawsFor (Proxy @Ordering)
        specLawsFor (Proxy @Int)
        specLawsFor (Proxy @Integer)

specLawsFor
    :: forall k. () =>
        ( Arbitrary k
        , Ord k
        , Read k
        , Show k
        , Typeable k
        )
    => Proxy k
    -> Spec
specLawsFor keyType = do
    let description = "Class laws for key type " <> show (typeRep keyType)
    describe description $ do
        testLawsMany @(MonoidMap k String)
            [ eqLaws
            , isListLaws
            , leftCancellativeLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , rightCancellativeLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (Product Integer))
            [ commutativeLaws
            , eqLaws
            , isListLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , reductiveLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (Product Natural))
            [ commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , eqLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
            , isListLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (NonZero (Product Rational)))
            [ commutativeLaws
            , eqLaws
            , groupLaws
            , isListLaws
            , monoidLaws
            , monoidNullLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (Sum Integer))
            [ cancellativeLaws
            , commutativeLaws
            , eqLaws
            , groupLaws
            , isListLaws
            , leftCancellativeLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (Sum Natural))
            [ cancellativeLaws
            , commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , eqLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
            , isListLaws
            , leftCancellativeLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (Set ()))
            [ commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , eqLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
            , isListLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (Set k))
            [ commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , eqLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
            , isListLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (Set Ordering))
            [ commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , eqLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
            , isListLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (Set Int))
            [ commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , eqLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
            , isListLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]
        testLawsMany @(MonoidMap k (MonoidMap k (Sum Natural)))
            [ cancellativeLaws
            , commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , eqLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
            , isListLaws
            , leftCancellativeLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

newtype NonZero a = NonZero a
    deriving newtype (Eq, Num, Read, Show, IsList)
    deriving newtype (Semigroup, Commutative, Monoid, MonoidNull, Group)

instance (Arbitrary a, Eq a, Num a) => Arbitrary (NonZero a) where
    -- Here we restrict the generator and shrinker so that they can never
    -- produce zero values, to avoid running into cases of ArithException
    -- caused by operations that may produce zero demoninators:
    arbitrary = NonZero <$> suchThatMap arbitrary maybeNonZero
    shrink = mapMaybe maybeNonZero . shrink

instance (Arbitrary k, Ord k, Arbitrary v, MonoidNull v) =>
    Arbitrary (MonoidMap k v)
  where
    arbitrary =
        fromList <$> scale (`mod` 16) (listOf ((,) <$> arbitrary <*> arbitrary))
    shrink =
        shrinkMapBy MonoidMap.fromMap MonoidMap.toMap shrink

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

maybeNonZero :: (Eq a, Num a) => a -> Maybe a
maybeNonZero p
    | p == 0 = Nothing
    | otherwise = Just p
