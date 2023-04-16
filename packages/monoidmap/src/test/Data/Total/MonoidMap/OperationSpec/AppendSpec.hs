{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Total.MonoidMap.OperationSpec.AppendSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Total.MonoidMap
    ( MonoidMap )
import Data.Typeable
    ( typeRep )
import Test.Common
    ( Key, Test, TestInstance (..), property, testInstancesMonoidNull )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, (===) )

import qualified Data.Total.MonoidMap as MonoidMap

spec :: Spec
spec = describe "Appending" $ do

    forM_ testInstancesMonoidNull $ \(TestInstance p) -> specFor (Proxy @Key) p

specFor :: forall k v. Test k v => Proxy k -> Proxy v -> Spec
specFor _k _v = describe (show $ typeRep (Proxy @(MonoidMap k v))) $ do

    it "prop_append_get" $
        prop_append_get
            @k @v & property

prop_append_get
    :: Test k v => MonoidMap k v -> MonoidMap k v -> k -> Property
prop_append_get m1 m2 k =
    MonoidMap.get k (m1 <> m2) === MonoidMap.get k m1 <> MonoidMap.get k m2
