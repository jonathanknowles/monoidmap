-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.FoldSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.MonoidMap.Internal
    ( MonoidMap )
import Data.Proxy
    ( Proxy (..) )
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
    ( Fun (..), Property, applyFun2, applyFun3, (===) )

import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap.Internal as MonoidMap

spec :: Spec
spec = describe "Folding" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    describe "Lazy" $ do

        it "prop_equivalence_foldl" $
            prop_equivalence_foldl
                @k @v & property
        it "prop_equivalence_foldr" $
            prop_equivalence_foldr
                @k @v & property
        it "prop_equivalence_foldlWithKey" $
            prop_equivalence_foldlWithKey
                @k @v & property
        it "prop_equivalence_foldrWithKey" $
            prop_equivalence_foldrWithKey
                @k @v & property
        it "prop_equivalence_foldMapWithKey" $
            prop_equivalence_foldMapWithKey
                @k @v & property

    describe "Strict" $ do

        it "prop_equivalence_foldl'" $
            prop_equivalence_foldl'
                @k @v & property
        it "prop_equivalence_foldr'" $
            prop_equivalence_foldr'
                @k @v & property
        it "prop_equivalence_foldlWithKey'" $
            prop_equivalence_foldlWithKey'
                @k @v & property
        it "prop_equivalence_foldrWithKey'" $
            prop_equivalence_foldrWithKey'
                @k @v & property
        it "prop_equivalence_foldMapWithKey'" $
            prop_equivalence_foldMapWithKey'
                @k @v & property

--------------------------------------------------------------------------------
-- Lazy folding
--------------------------------------------------------------------------------

prop_equivalence_foldl
    :: Test k v
    => r ~ v
    => Fun (r, v) r
    -> r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldl (applyFun2 -> f) r m =
    MonoidMap.foldl f r m
      === Map.foldl f r (MonoidMap.toMap m)

prop_equivalence_foldr
    :: Test k v
    => r ~ v
    => Fun (v, r) r
    -> r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldr (applyFun2 -> f) r m =
    MonoidMap.foldr f r m
      === Map.foldr f r (MonoidMap.toMap m)

prop_equivalence_foldlWithKey
    :: Test k v
    => r ~ v
    => Fun (r, k, v) r
    -> r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldlWithKey (applyFun3 -> f) r m =
    MonoidMap.foldlWithKey f r m
      === Map.foldlWithKey f r (MonoidMap.toMap m)

prop_equivalence_foldrWithKey
    :: Test k v
    => r ~ v
    => Fun (k, v, r) r
    -> r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldrWithKey (applyFun3 -> f) r m =
    MonoidMap.foldrWithKey f r m
      === Map.foldrWithKey f r (MonoidMap.toMap m)

prop_equivalence_foldMapWithKey
    :: Test k v
    => r ~ v
    => Fun (k, v) r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldMapWithKey (applyFun2 -> f) m =
    MonoidMap.foldMapWithKey f m
      === Map.foldMapWithKey f (MonoidMap.toMap m)

--------------------------------------------------------------------------------
-- Strict folding
--------------------------------------------------------------------------------

prop_equivalence_foldl'
    :: Test k v
    => r ~ v
    => Fun (r, v) r
    -> r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldl' (applyFun2 -> f) r m =
    MonoidMap.foldl' f r m ===
    MonoidMap.foldl  f r m

prop_equivalence_foldr'
    :: Test k v
    => r ~ v
    => Fun (v, r) r
    -> r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldr' (applyFun2 -> f) r m =
    MonoidMap.foldr' f r m ===
    MonoidMap.foldr  f r m

prop_equivalence_foldlWithKey'
    :: Test k v
    => r ~ v
    => Fun (r, k, v) r
    -> r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldlWithKey' (applyFun3 -> f) r m =
    MonoidMap.foldlWithKey' f r m ===
    MonoidMap.foldlWithKey  f r m

prop_equivalence_foldrWithKey'
    :: Test k v
    => r ~ v
    => Fun (k, v, r) r
    -> r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldrWithKey' (applyFun3 -> f) r m =
    MonoidMap.foldrWithKey' f r m ===
    MonoidMap.foldrWithKey  f r m

prop_equivalence_foldMapWithKey'
    :: Test k v
    => r ~ v
    => Fun (k, v) r
    -> MonoidMap k v
    -> Property
prop_equivalence_foldMapWithKey' (applyFun2 -> f) m =
    MonoidMap.foldMapWithKey' f m ===
    MonoidMap.foldMapWithKey  f m
