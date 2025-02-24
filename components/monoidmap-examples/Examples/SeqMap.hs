{-# LANGUAGE StandaloneDeriving #-}

module Examples.SeqMap where

import Prelude

import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMapF
    ( MonoidMapF (MonoidMapF)
    )
import Data.Sequence
    ( Seq
    )

newtype SeqMap k v = SeqMap (MonoidMap k (Seq v))

deriving via MonoidMapF k Seq instance Foldable (SeqMap k)
deriving via MonoidMapF k Seq instance Functor (SeqMap k)
