{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.TraversalSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.MonoidMap
    ( MonoidMap )
import Data.Proxy
    ( Proxy (..) )
import Test.Common
    ( Key, Test, TestType (TestType), makeSpec, property, testTypesMonoidNull )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Fun (..), Property, applyFun, applyFun2, (===) )
import Data.Semigroup
    ( First (..), Last (..) )

import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Traversal" $ do

    forM_ testTypesMonoidNull $ \(TestType p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor = makeSpec $ do

    describe "traverse" $ do

        it "prop_traverse_@Identity" $
            prop_traverse @Identity
                @k @v & property
        it "prop_traverse_@Maybe" $
            prop_traverse @Maybe
                @k @v & property
        it "prop_traverse_@First" $
            prop_traverse @First
                @k @v & property
        it "prop_traverse_@Last" $
            prop_traverse @Last
                @k @v & property

    describe "traverseWithKey" $ do

        it "prop_traverseWithKey_@Identity" $
            prop_traverseWithKey @Identity
                @k @v & property
        it "prop_traverseWithKey_@Maybe" $
            prop_traverseWithKey @Maybe
                @k @v & property
        it "prop_traverseWithKey_@First" $
            prop_traverseWithKey @First
                @k @v & property
        it "prop_traverseWithKey_@Last" $
            prop_traverseWithKey @Last
                @k @v & property

prop_traverse
    :: forall t k v. Test k v
    => (Applicative t, Eq (t (MonoidMap k v)), Show (t (MonoidMap k v)))
    => Fun v (t v)
    -> MonoidMap k v
    -> Property
prop_traverse (applyFun -> f) m =
    MonoidMap.traverse f m
    ===
    fmap MonoidMap.fromMap (Prelude.traverse f (MonoidMap.toMap m))

prop_traverseWithKey
    :: forall t k v. Test k v
    => (Applicative t, Eq (t (MonoidMap k v)), Show (t (MonoidMap k v)))
    => Fun (k, v) (t v)
    -> MonoidMap k v
    -> Property
prop_traverseWithKey (applyFun2 -> f) m =
    MonoidMap.traverseWithKey f m
    ===
    fmap MonoidMap.fromMap (Map.traverseWithKey f (MonoidMap.toMap m))

deriving newtype instance Arbitrary a => Arbitrary (First a)
deriving newtype instance Arbitrary a => Arbitrary (Last a)
