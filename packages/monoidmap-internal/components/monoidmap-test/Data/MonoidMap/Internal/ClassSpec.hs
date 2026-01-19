{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.ClassSpec
    where

import Prelude

import Data.Monoid
    ( Product (..), Sum (..) )
import Data.MonoidMap.Internal
    ( MonoidMap )
import Data.Proxy
    ( Proxy (..) )
import Data.Set
    ( Set )
import Data.Typeable
    ( Typeable, typeRep )
import Numeric.Natural
    ( Natural )
import Test.Combinators.NonZero
    ( NonZero, genNonZero, shrinkNonZero )
import Test.Common ()
import Test.Hspec
    ( Spec, describe )
import Test.Key
    ( Key1, Key2, Key4, Key8 )
import Test.QuickCheck
    ( Arbitrary (..) )
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

spec :: Spec
spec = do
    describe "Class laws" $ do
        -- Test against a variety of key sizes:
        specLawsFor (Proxy @Key1)
        specLawsFor (Proxy @Key2)
        specLawsFor (Proxy @Key4)
        specLawsFor (Proxy @Key8)

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
        -- Here we restrict the generator and shrinker so that they can never
        -- produce zero values, to avoid running into cases of ArithException
        -- caused by operations that may produce zero demoninators:
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

instance (Arbitrary a, Eq a, Num a) => Arbitrary (NonZero a) where
    arbitrary = genNonZero arbitrary
    shrink = shrinkNonZero shrink
