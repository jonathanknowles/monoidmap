{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.SliceSpec
    ( spec
    , Slice (..)
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Bifunctor
    ( Bifunctor (bimap) )
import Data.Function
    ( (&) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap.Internal
    ( MonoidMap, nonNullCount )
import Data.Proxy
    ( Proxy (..) )
import GHC.Exts
    ( IsList (..) )
import Test.Common
    ( Key
    , Test
    , TestValueType (TestValueType)
    , makeSpec
    , property
    , testValueTypesAll
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Gen, Property, choose, cover, oneof, (===) )

import qualified Data.MonoidMap.Internal as MonoidMap

spec :: Spec
spec = describe "Slicing" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    it "prop_take_toList_fromList" $
        prop_take_toList_fromList
            @k @v & property
    it "prop_drop_toList_fromList" $
        prop_drop_toList_fromList
            @k @v & property
    it "prop_splitAt_toList_fromList" $
        prop_splitAt_toList_fromList
            @k @v & property

data Slice k v = Slice Int (MonoidMap k v)
    deriving (Eq, Show)

instance (Arbitrary k, Arbitrary v, MonoidNull v, Ord k) =>
    Arbitrary (Slice k v)
  where
    arbitrary = do
        m <- genMap
        i <- genIndex m
        pure $ Slice i m
      where
        genMap :: Gen (MonoidMap k v)
        genMap = arbitrary

        genIndex :: MonoidMap k v -> Gen Int
        genIndex m = oneof
            [ choose (negate (length m), -1)
            , pure 0
            , choose (1, length m - 1)
            , pure (length m)
            , choose (length m + 1, 2 * length m)
            ]

prop_take_toList_fromList
    :: Test k v => Slice k v -> Property
prop_take_toList_fromList (Slice i m) =
    MonoidMap.take i m
        === (fromList . Prelude.take i . toList) m
    & cover 2
        (i == 0 && 0 < nonNullCount m)
        "i == 0 && 0 < nonNullCount m"
    & cover 2
        (0 < i && i < nonNullCount m)
        "0 < i && i < nonNullCount m"
    & cover 2
        (0 < nonNullCount m && nonNullCount m == i)
        "0 < nonNullCount m && nonNullCount m == i"
    & cover 2
        (0 < nonNullCount m && nonNullCount m < i)
        "0 < nonNullCount m && nonNullCount m < i"

prop_drop_toList_fromList
    :: Test k v => Slice k v -> Property
prop_drop_toList_fromList (Slice i m) =
    MonoidMap.drop i m
        === (fromList . Prelude.drop i . toList) m
    & cover 2
        (i == 0 && 0 < nonNullCount m)
        "i == 0 && 0 < nonNullCount m"
    & cover 2
        (0 < i && i < nonNullCount m)
        "0 < i && i < nonNullCount m"
    & cover 2
        (0 < nonNullCount m && nonNullCount m == i)
        "0 < nonNullCount m && nonNullCount m == i"
    & cover 2
        (0 < nonNullCount m && nonNullCount m < i)
        "0 < nonNullCount m && nonNullCount m < i"

prop_splitAt_toList_fromList
    :: Test k v => Slice k v -> Property
prop_splitAt_toList_fromList (Slice i m) =
    MonoidMap.splitAt i m
        === (bimap fromList fromList . Prelude.splitAt i . toList) m
    & cover 2
        (i == 0 && 0 < nonNullCount m)
        "i == 0 && 0 < nonNullCount m"
    & cover 2
        (0 < i && i < nonNullCount m)
        "0 < i && i < nonNullCount m"
    & cover 2
        (0 < nonNullCount m && nonNullCount m == i)
        "0 < nonNullCount m && nonNullCount m == i"
    & cover 2
        (0 < nonNullCount m && nonNullCount m < i)
        "0 < nonNullCount m && nonNullCount m < i"
