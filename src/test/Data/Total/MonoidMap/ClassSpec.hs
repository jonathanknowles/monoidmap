{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.ClassSpec
    where

import Prelude

import Data.Monoid
    ( Product (..), Sum (..) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Set
    ( Set )
import Data.Total.MonoidMap
    ( MonoidMap )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe )
import Test.QuickCheck
    ( Arbitrary (..), listOf, scale, shrinkMapBy )
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
    ( cancellativeGCDMonoidLaws
    , gcdMonoidLaws
    , leftGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , rightGCDMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.LCM
    ( lcmMonoidLaws )
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

import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Class laws" $ do

    testLawsMany @(MonoidMap Bool String)
        [ eqLaws
        , isListLaws
        , leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Bool (Product Integer))
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
    testLawsMany @(MonoidMap Bool (Product Natural))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Bool (Product Rational))
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
    testLawsMany @(MonoidMap Bool (Sum Integer))
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
    testLawsMany @(MonoidMap Bool (Sum Natural))
        [ cancellativeGCDMonoidLaws
        , cancellativeLaws
        , commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Bool (Set ()))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Bool (Set Bool))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Bool (Set Ordering))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Bool (Set Int))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Bool (MonoidMap Bool (Sum Natural)))
        [ cancellativeGCDMonoidLaws
        , cancellativeLaws
        , commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]

    testLawsMany @(MonoidMap Int String)
        [ eqLaws
        , isListLaws
        , leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Product Integer))
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
    testLawsMany @(MonoidMap Int (Product Natural))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Product Rational))
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
    testLawsMany @(MonoidMap Int (Sum Integer))
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
    testLawsMany @(MonoidMap Int (Sum Natural))
        [ cancellativeGCDMonoidLaws
        , cancellativeLaws
        , commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Set ()))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Set Bool))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Set Ordering))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (Set Int))
        [ commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]
    testLawsMany @(MonoidMap Int (MonoidMap Int (Sum Natural)))
        [ cancellativeGCDMonoidLaws
        , cancellativeLaws
        , commutativeLaws
        , eqLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , isListLaws
        , leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , semigroupLaws
        , semigroupMonoidLaws
        , showReadLaws
        ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance (Arbitrary k, Ord k, Arbitrary v, MonoidNull v) =>
    Arbitrary (MonoidMap k v)
  where
    arbitrary =
        fromList <$> scale (`mod` 16) (listOf ((,) <$> arbitrary <*> arbitrary))
    shrink =
        shrinkMapBy MonoidMap.fromMap MonoidMap.toMap shrink
