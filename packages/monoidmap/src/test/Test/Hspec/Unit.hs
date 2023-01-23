{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Test.Hspec.Unit where

import Prelude

import Data.Functor
    ( (<&>) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( counterexample, property )
import Text.Show.Pretty
    ( ppShow )

import qualified Data.Foldable as F

class IsUnitTestDatum d f r | d -> f, d -> r where
    params :: d -> [String]
    resultActual :: f -> d -> r
    resultExpected :: d -> r

data UnitTestDatum1 p1 r = UnitTestDatum1 p1 r
data UnitTestDatum2 p1 p2 r = UnitTestDatum2 p1 p2 r
data UnitTestDatum3 p1 p2 p3 r = UnitTestDatum3 p1 p2 p3 r
data UnitTestDatum4 p1 p2 p3 p4 r = UnitTestDatum4 p1 p2 p3 p4 r

type UnitTestData1 p1 r = [UnitTestDatum1 p1 r]
type UnitTestData2 p1 p2 r = [UnitTestDatum2 p1 p2 r]
type UnitTestData3 p1 p2 p3 r = [UnitTestDatum3 p1 p2 p3 r]
type UnitTestData4 p1 p2 p3 p4 r = [UnitTestDatum4 p1 p2 p3 p4 r]

unitTestDatum1 :: (p1, r) -> UnitTestDatum1 p1 r
unitTestDatum1 (p1, r) = UnitTestDatum1 p1 r
unitTestDatum2 :: (p1, p2, r) -> UnitTestDatum2 p1 p2 r
unitTestDatum2 (p1, p2, r) = UnitTestDatum2 p1 p2 r
unitTestDatum3 :: (p1, p2, p3, r) -> UnitTestDatum3 p1 p2 p3 r
unitTestDatum3 (p1, p2, p3, r) = UnitTestDatum3 p1 p2 p3 r
unitTestDatum4 :: (p1, p2, p3, p4, r) -> UnitTestDatum4 p1 p2 p3 p4 r
unitTestDatum4 (p1, p2, p3, p4, r) = UnitTestDatum4 p1 p2 p3 p4 r

unitTestData1 :: [(p1, r)] -> UnitTestData1 p1 r
unitTestData1 = fmap unitTestDatum1
unitTestData2 :: [(p1, p2, r)] -> UnitTestData2 p1 p2 r
unitTestData2 = fmap unitTestDatum2
unitTestData3 :: [(p1, p2, p3, r)] -> UnitTestData3 p1 p2 p3 r
unitTestData3 = fmap unitTestDatum3
unitTestData4 :: [(p1, p2, p3, p4, r)] -> UnitTestData4 p1 p2 p3 p4 r
unitTestData4 = fmap unitTestDatum4

instance Show p1 =>
    IsUnitTestDatum (UnitTestDatum1 p1 r) (p1 -> r) r
  where
    params (UnitTestDatum1 p1 _) = [show p1]
    resultActual f (UnitTestDatum1 p1 _) = f p1
    resultExpected (UnitTestDatum1 _ r) = r

instance (Show p1, Show p2) =>
    IsUnitTestDatum (UnitTestDatum2 p1 p2 r) (p1 -> p2 -> r) r
  where
    params (UnitTestDatum2 p1 p2 _) = [show p1, show p2]
    resultActual f (UnitTestDatum2 p1 p2 _) = f p1 p2
    resultExpected (UnitTestDatum2 _ _ r) = r

instance (Show p1, Show p2, Show p3) =>
    IsUnitTestDatum (UnitTestDatum3 p1 p2 p3 r) (p1 -> p2 -> p3 -> r) r
  where
    params (UnitTestDatum3 p1 p2 p3 _) = [show p1, show p2, show p3]
    resultActual f (UnitTestDatum3 p1 p2 p3 _) = f p1 p2 p3
    resultExpected (UnitTestDatum3 _ _ _ r) = r

instance (Show p1, Show p2, Show p3, Show p4) =>
    IsUnitTestDatum (UnitTestDatum4 p1 p2 p3 p4 r) (p1 -> p2 -> p3 -> p4 -> r) r
  where
    params (UnitTestDatum4 p1 p2 p3 p4 _) = [show p1, show p2, show p3, show p4]
    resultActual f (UnitTestDatum4 p1 p2 p3 p4 _) = f p1 p2 p3 p4
    resultExpected (UnitTestDatum4 _ _ _ _ r) = r

unitTestSpec
    :: forall d f r. (IsUnitTestDatum d f r, Eq r, Show r)
    => String
    -> String
    -> f
    -> [d]
    -> Spec
unitTestSpec specDescription functionName function =
    describe specDescription . mapM_ unitTest
  where
    unitTest :: d -> Spec
    unitTest d = it description
        $ property
        $ counterexample counterexampleText
        $ resultExpected d == resultActual function d
      where
        counterexampleText = unlines
            [ ""
            , "expected"
            , "/="
            , "actual"
            , ""
            , showWrap (resultExpected d)
            , "/="
            , showWrap (resultActual function d)
            ]
        description = unwords
            [ functionName
            , unwords (params d <&> \s -> "(" <> s <> ")")
            ]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

showWrap :: Show a => a -> String
showWrap x
    | singleLineMaxLengthExceeded =
        multiLine
    | otherwise =
        singleLine
  where
    multiLine = ppShow x
    singleLine = show x
    singleLineMaxLength = 80
    singleLineMaxLengthExceeded = F.length singleLine > singleLineMaxLength
