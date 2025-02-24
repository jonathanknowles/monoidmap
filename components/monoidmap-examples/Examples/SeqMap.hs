{-# LANGUAGE DeriveTraversable #-}

module Examples.SeqMap where

import Prelude

import Data.Monoid.Null
    ( MonoidNull
    , PositiveMonoid
    )
import Data.MonoidMapF
    ( MonoidMapF
    )
import Data.Sequence
    ( Seq
    )

newtype SeqMap k v = SeqMap (MonoidMapF k Seq v)
    deriving newtype
        ( Eq
        , Monoid
        , MonoidNull
        , PositiveMonoid
        , Read
        , Semigroup
        , Show
        )
    deriving stock
        ( Foldable
        , Functor
        , Traversable
        )
