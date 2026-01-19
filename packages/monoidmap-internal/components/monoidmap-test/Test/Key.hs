{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Quasi-unique keys.
--
module Test.Key
    ( Key1
    , Key2
    , Key4
    , Key8
    )
where

import Prelude

import GHC.Generics
    ( Generic
    )
import GHC.TypeLits
    ( Nat
    )
import Test.QuickCheck
    ( Arbitrary
    , CoArbitrary
    , Function
    )
import Test.QuickCheck.Quid
    ( Latin (Latin)
    , Quid
    , Size (Size)
    )

newtype Key (size :: Nat) = Key Quid
    deriving stock (Eq, Generic, Ord)
    deriving (Read, Show) via Latin Quid
    deriving (Arbitrary) via Size size Quid
    deriving (CoArbitrary) via Quid
    deriving anyclass (Function)

type Key1 = Key 1
type Key2 = Key 2
type Key4 = Key 4
type Key8 = Key 8
