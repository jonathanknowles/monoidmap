{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{- HLINT ignore "Eta reduce" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.NFDataSpec
    ( spec
    ) where

import Prelude hiding
    ( null )

import Control.DeepSeq
    ( NFData (rnf), NFData1 (liftRnf), NFData2 (liftRnf2), rwhnf )
import Control.Exception
    ( ErrorCall (ErrorCall), evaluate, throwIO, try )
import Data.Function
    ( on, (&) )
import Data.Monoid
    ( Sum )
import Data.Monoid.Null
    ( MonoidNull (null) )
import Data.MonoidMap
    ( MonoidMap )
import GHC.Generics
    ( Generic )
import Test.Common
    ( property )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (arbitrary, shrink)
    , Gen
    , Property
    , cover
    , frequency
    , ioProperty
    , (===)
    )

import qualified Data.MonoidMap as MonoidMap

--------------------------------------------------------------------------------
-- Type: Poison
--------------------------------------------------------------------------------

data Poison
    deriving stock Generic
    deriving anyclass NFData

somePoison :: Poison
somePoison = error poisonMessage

poisonMessage :: String
poisonMessage = "💀 Poison 💀"

--------------------------------------------------------------------------------
-- Type: Box
--------------------------------------------------------------------------------

-- | A box whose contents may be accompanied by 'Poison'.
--
-- The poison lies beneath weak head normal form, and is released only if the
-- box is fully evaluated.
--
-- Placing boxes inside a structure lets us check that a function evaluates
-- every part of that structure, and evaluates each part deeply.
--
-- Apart from 'NFData', no instance for 'Box' forces the poison. This means
-- that boxes can be safely compared, combined, shown, and stored within
-- structures without releasing the poison.
--
data Box a
    = HarmlessBox a
    | PoisonedBox a Poison
    deriving stock (Generic, Functor, Foldable, Traversable)
    deriving anyclass NFData

instance Eq a => Eq (Box a) where
    (==) = (==) `on` safelyUnbox

instance Ord a => Ord (Box a) where
    (<=) = (<=) `on` safelyUnbox

instance Show a => Show (Box a) where
    showsPrec d = \case
        HarmlessBox a   -> showsApp "harmlessBox" a
        PoisonedBox a _ -> showsApp "poisonedBox" a
      where
        showsApp f a = showParen (d > 10) $
            showString f . showString " " . showsPrec 11 a

instance Semigroup a => Semigroup (Box a) where
    HarmlessBox x   <> HarmlessBox y   = harmlessBox (x <> y)
    HarmlessBox x   <> PoisonedBox y _ = poisonedBox (x <> y)
    PoisonedBox x _ <> HarmlessBox y   = poisonedBox (x <> y)
    PoisonedBox x _ <> PoisonedBox y _ = poisonedBox (x <> y)

instance Monoid a => Monoid (Box a) where
    mempty = harmlessBox mempty

instance MonoidNull a => MonoidNull (Box a) where
    null = null . safelyUnbox

instance Arbitrary a => Arbitrary (Box a) where
    arbitrary = genBox <*> genVal
      where
        genBox :: Gen (a -> Box a)
        genBox = frequency
            [ (1, pure poisonedBox)
            , (7, pure harmlessBox)
            ]
        genVal :: Gen a
        genVal = arbitrary
    shrink b =
        shrinkBox <> shrinkVal
      where
        shrinkBox = [removePoisonFromBox b | isPoisonedBox b]
        shrinkVal = traverse shrink b

safelyUnbox :: Box a -> a
safelyUnbox = \case
    HarmlessBox a   -> a
    PoisonedBox a _ -> a

harmlessBox :: a -> Box a
harmlessBox a = HarmlessBox a

poisonedBox :: a -> Box a
poisonedBox a = PoisonedBox a somePoison

isHarmlessBox :: Box a -> Bool
isHarmlessBox = \case
    HarmlessBox {} -> True
    PoisonedBox {} -> False

isPoisonedBox :: Box a -> Bool
isPoisonedBox = \case
    HarmlessBox {} -> False
    PoisonedBox {} -> True

insertPoisonIntoBox :: Box a -> Box a
insertPoisonIntoBox = poisonedBox . safelyUnbox

removePoisonFromBox :: Box a -> Box a
removePoisonFromBox = harmlessBox . safelyUnbox

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = do

    describe "Box" $ do
        it "prop_Box_rnf" $
            prop_Box_rnf
                @TestValue & property
        it "prop_Box_rwhnf" $
            prop_Box_rwhnf
                @TestValue & property

    describe "MonoidMap" $ do
        it "prop_MonoidMap_rnf" $
            prop_MonoidMap_rnf
                @TestKey @TestValue & property
        it "prop_MonoidMap_liftRnf_rnf" $
            prop_MonoidMap_liftRnf_rnf
                @TestKey @TestValue & property
        it "prop_MonoidMap_liftRnf_ignore" $
            prop_MonoidMap_liftRnf_ignore
                @TestKey @TestValue & property
        it "prop_MonoidMap_liftRnf2_rnf_rnf" $
            prop_MonoidMap_liftRnf2_rnf_rnf
                @TestKey @TestValue & property
        it "prop_MonoidMap_liftRnf2_rnf_ignore" $
            prop_MonoidMap_liftRnf2_rnf_ignore
                @TestKey @TestValue & property
        it "prop_MonoidMap_liftRnf2_ignore_rnf" $
            prop_MonoidMap_liftRnf2_ignore_rnf
                @TestKey @TestValue & property
        it "prop_MonoidMap_liftRnf2_ignore_ignore" $
            prop_MonoidMap_liftRnf2_ignore_ignore
                @TestKey @TestValue & property

--------------------------------------------------------------------------------
-- Properties: Box
--------------------------------------------------------------------------------

prop_Box_rnf :: NFData a => Box a -> Property
prop_Box_rnf m =
    rnf m `isPoisonousIff` isPoisonedBox m
    & cover 2
        (isPoisonedBox m)
        "isPoisonedBox m"
    & cover 2
        (not (isPoisonedBox m))
        "not (isPoisonedBox m)"

-- Poison lies beneath weak head normal form, so that a shallow evaluation
-- cannot be mistaken for a deep one.
--
prop_Box_rwhnf :: Box a -> Property
prop_Box_rwhnf m =
    rwhnf m `isPoisonousIff` False
    & cover 2
        (isPoisonedBox m)
        "isPoisonedBox m"
    & cover 2
        (not (isPoisonedBox m))
        "not (isPoisonedBox m)"

--------------------------------------------------------------------------------
-- Properties: MonoidMap
--------------------------------------------------------------------------------

prop_MonoidMap_rnf
    :: (NFData k, NFData v) => TestMap k v -> Property
prop_MonoidMap_rnf m =
    rnf m `isPoisonousIff` (mapHasPoisonedKey m || mapHasPoisonedValue m)
    & coverMapPoisoning m

prop_MonoidMap_liftRnf_rnf
    :: (NFData k, NFData v) => TestMap k v -> Property
prop_MonoidMap_liftRnf_rnf m =
    liftRnf rnf m
        `isPoisonousIff` (mapHasPoisonedKey m || mapHasPoisonedValue m)
    & coverMapPoisoning m

prop_MonoidMap_liftRnf_ignore
    :: NFData k => TestMap k v -> Property
prop_MonoidMap_liftRnf_ignore m =
    liftRnf ignore m
        `isPoisonousIff` mapHasPoisonedKey m
    & coverMapPoisoning m

prop_MonoidMap_liftRnf2_rnf_rnf
    :: (NFData k, NFData v) => TestMap k v -> Property
prop_MonoidMap_liftRnf2_rnf_rnf m =
    liftRnf2 rnf rnf m
        `isPoisonousIff` (mapHasPoisonedKey m || mapHasPoisonedValue m)
    & coverMapPoisoning m

prop_MonoidMap_liftRnf2_rnf_ignore
    :: NFData k => TestMap k v -> Property
prop_MonoidMap_liftRnf2_rnf_ignore m =
    liftRnf2 rnf ignore m
        `isPoisonousIff` mapHasPoisonedKey m
    & coverMapPoisoning m

prop_MonoidMap_liftRnf2_ignore_rnf
    :: forall k v. NFData v => TestMap k v -> Property
prop_MonoidMap_liftRnf2_ignore_rnf m =
    liftRnf2 ignore rnf m
        `isPoisonousIff` mapHasPoisonedValue m
    & coverMapPoisoning m

prop_MonoidMap_liftRnf2_ignore_ignore
    :: TestMap k v -> Property
prop_MonoidMap_liftRnf2_ignore_ignore m =
    liftRnf2 ignore ignore m
        `isPoisonousIff` False
    & coverMapPoisoning m

--------------------------------------------------------------------------------
-- Test types
--------------------------------------------------------------------------------

type TestMap k v = MonoidMap (Box k) (Box v)
type TestKey = Int
type TestValue = Sum Int

--------------------------------------------------------------------------------
-- Test functions
--------------------------------------------------------------------------------

ignore :: a -> ()
ignore = const ()

mapHasPoisonedKey :: TestMap k v -> Bool
mapHasPoisonedKey = any (isPoisonedBox . fst) . MonoidMap.toList

mapHasPoisonedValue :: TestMap k v -> Bool
mapHasPoisonedValue = any (isPoisonedBox . snd) . MonoidMap.toList

coverMapPoisoning :: TestMap k v -> Property -> Property
coverMapPoisoning m p = p
    & cover 10
        (not (mapHasPoisonedKey m) && not (mapHasPoisonedValue m) && nonNull)
        "no poison (non-null map)"
    & cover 10
        (mapHasPoisonedKey m && not (mapHasPoisonedValue m))
        "poisoned keys only"
    & cover 10
        (not (mapHasPoisonedKey m) && mapHasPoisonedValue m)
        "poisoned values only"
    & cover 10
        (mapHasPoisonedKey m && mapHasPoisonedValue m)
        "poisoned keys and values"
  where
    nonNull = MonoidMap.nonNull m

-- | Reports whether or not evaluating the given expression releases poison.
--
-- Any other exception is rethrown, so that it cannot be mistaken for poison.
--
isPoisonous :: () -> IO Bool
isPoisonous u =
    try (evaluate u) >>= \case
        Right () ->
            pure False
        Left e@(ErrorCall message)
            | message == poisonMessage -> pure True
            | otherwise -> throwIO e

isPoisonousIff :: () -> Bool -> Property
isPoisonousIff u expected = ioProperty $ (=== expected) <$> isPoisonous u
