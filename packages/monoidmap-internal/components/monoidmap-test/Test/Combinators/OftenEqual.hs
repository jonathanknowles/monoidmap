-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Test.Combinators.OftenEqual
    ( OftenEqual (OftenEqual)
    , genOftenEqual
    , shrinkOftenEqual
    )
    where

import Prelude

import Test.QuickCheck
    ( Arbitrary (arbitrary, shrink)
    , Arbitrary2 (liftShrink2)
    , Gen
    , oneof
    )

-- | A pair of values that are equal at least half of the time.
data OftenEqual a = OftenEqual !a !a
    deriving (Eq, Show)

genOftenEqual :: Gen a -> Gen (OftenEqual a)
genOftenEqual genA = do
    a1 <- genA
    a2 <- oneof [pure a1, genA]
    pure $ OftenEqual a1 a2

shrinkOftenEqual :: Eq a => (a -> [a]) -> OftenEqual a -> [OftenEqual a]
shrinkOftenEqual shrinkA (OftenEqual a1 a2)
    | a1 == a2  = [OftenEqual a a | a <- shrinkA a1]
    | otherwise = uncurry OftenEqual <$> liftShrink2 shrinkA shrinkA (a1, a2)

instance (Arbitrary a, Eq a) => Arbitrary (OftenEqual a) where
    arbitrary = genOftenEqual arbitrary
    shrink = shrinkOftenEqual shrink
