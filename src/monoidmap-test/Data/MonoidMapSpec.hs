{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMapSpec
    where

import Prelude

import Data.Monoid
    ( Sum (..) )
import Data.MonoidMap
    ( MonoidMap )
import Data.Set
    ( Set )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, parallel )
import Test.QuickCheck
    ( Arbitrary (..), listOf, shrinkMapBy )
import Test.QuickCheck.Classes
    ( eqLaws
    , isListLaws
    , monoidLaws
    , semigroupLaws
    , semigroupMonoidLaws
    , showReadLaws
    )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Classes.Semigroup
    ( cancellativeGCDMonoidLaws
    , cancellativeLaws
    , commutativeLaws
    , gcdMonoidLaws
    , leftCancellativeLaws
    , leftGCDMonoidLaws
    , leftReductiveLaws
    , monoidNullLaws
    , monusLaws
    , overlappingGCDMonoidLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightGCDMonoidLaws
    , rightReductiveLaws
    )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Data.MonoidMap as MonoidMap

spec :: Spec
spec =
    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(MonoidMap Int String)
            [ eqLaws
            , isListLaws
            , leftCancellativeLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , overlappingGCDMonoidLaws
            , rightCancellativeLaws
            , rightGCDMonoidLaws
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
            , isListLaws
            , leftCancellativeLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , reductiveLaws
            , rightCancellativeLaws
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
            , isListLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
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
            , isListLaws
            , leftCancellativeLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]

instance (Arbitrary k, Ord k, Arbitrary v, Eq v, Monoid v) =>
    Arbitrary (MonoidMap k v)
  where
    arbitrary = fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)
    shrink = shrinkMapBy MonoidMap.fromMap MonoidMap.toMap shrink
