{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.MonoidMapF where

import Prelude

import Data.Monoid
    ( First
    , Last
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.Sequence
    ( Seq
    )

import qualified Data.MonoidMap as MonoidMap

-- | The class of monoidal functors that preserve nullity.
--
-- Instances must satisfy the following law:
--
-- @
-- 'null' a '==' 'null' ('fmap' f a)
-- @
class (Functor f, forall a. MonoidNull (f a)) => MonoidNullStableFunctor f

instance MonoidNullStableFunctor []
instance MonoidNullStableFunctor Seq
instance MonoidNullStableFunctor First
instance MonoidNullStableFunctor Last
instance (forall a. MonoidNull (Maybe a)) => MonoidNullStableFunctor Maybe

newtype MonoidMapF k f v = MonoidMapF (MonoidMap k (f v))

instance (Foldable f) => Foldable (MonoidMapF k f) where
    foldMap f (MonoidMapF m) = foldMap (foldMap f) m

instance (MonoidNullStableFunctor f) => Functor (MonoidMapF k f) where
    fmap f (MonoidMapF m) = MonoidMapF $ MonoidMap.map (fmap f) m

instance
    (MonoidNullStableFunctor f, Traversable f)
    => Traversable (MonoidMapF k f)
  where
    traverse f (MonoidMapF m) = MonoidMapF <$> MonoidMap.traverse (traverse f) m
