{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.TraversalSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
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
    ( Arbitrary (..)
    , Fun (..)
    , Property
    , applyFun
    , applyFun2
    , applyFun3
    , (===)
    )
import Data.Semigroup
    ( First (..), Last (..) )

import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap.Internal as MonoidMap
import qualified Data.Traversable as Traversable

spec :: Spec
spec = describe "Traversal" $ do

    forM_ testValueTypesAll $
        \(TestValueType p) -> specFor (Proxy @Key) p

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

    describe "mapAccumL" $ do

        it "prop_mapAccumL_@Int" $
            prop_mapAccumL @Int
                @k @v & property
        it "prop_mapAccumL_@String" $
            prop_mapAccumL @String
                @k @v & property

    describe "mapAccumR" $ do

        it "prop_mapAccumR_@Int" $
            prop_mapAccumR @Int
                @k @v & property
        it "prop_mapAccumR_@String" $
            prop_mapAccumR @String
                @k @v & property

    describe "mapAccumLWithKey" $ do

        it "prop_mapAccumLWithKey_@Int" $
            prop_mapAccumLWithKey @Int
                @k @v & property
        it "prop_mapAccumLWithKey_@String" $
            prop_mapAccumLWithKey @String
                @k @v & property

    describe "mapAccumRWithKey" $ do

        it "prop_mapAccumRWithKey_@Int" $
            prop_mapAccumRWithKey @Int
                @k @v & property
        it "prop_mapAccumRWithKey_@String" $
            prop_mapAccumRWithKey @String
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
    fmap MonoidMap.fromMap (Traversable.traverse f (MonoidMap.toMap m))

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

prop_mapAccumL
    :: forall s k v. (Test k v, Eq s, Show s)
    => Fun (s, v) (s, v)
    -> s
    -> MonoidMap k v
    -> Property
prop_mapAccumL (applyFun2 -> f) s m =
    MonoidMap.mapAccumL f s m
    ===
    fmap MonoidMap.fromMap (Traversable.mapAccumL f s (MonoidMap.toMap m))

prop_mapAccumR
    :: forall s k v. (Test k v, Eq s, Show s)
    => Fun (s, v) (s, v)
    -> s
    -> MonoidMap k v
    -> Property
prop_mapAccumR (applyFun2 -> f) s m =
    MonoidMap.mapAccumR f s m
    ===
    fmap MonoidMap.fromMap (Traversable.mapAccumR f s (MonoidMap.toMap m))

prop_mapAccumLWithKey
    :: forall s k v. (Test k v, Eq s, Show s)
    => Fun (s, k, v) (s, v)
    -> s
    -> MonoidMap k v
    -> Property
prop_mapAccumLWithKey (applyFun3 -> f) s m =
    MonoidMap.mapAccumLWithKey f s m
    ===
    fmap MonoidMap.fromMap (Map.mapAccumWithKey f s (MonoidMap.toMap m))

prop_mapAccumRWithKey
    :: forall s k v. (Test k v, Eq s, Show s)
    => Fun (s, k, v) (s, v)
    -> s
    -> MonoidMap k v
    -> Property
prop_mapAccumRWithKey (applyFun3 -> f) s m =
    MonoidMap.mapAccumRWithKey f s m
    ===
    fmap MonoidMap.fromMap (Map.mapAccumRWithKey f s (MonoidMap.toMap m))

deriving newtype instance Arbitrary a => Arbitrary (First a)
deriving newtype instance Arbitrary a => Arbitrary (Last a)
