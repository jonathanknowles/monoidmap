-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Benchmark for the `MonoidMap` type.
--
-- Instead of benchmarking functions for the `MonoidMap` type directly, we
-- benchmark functions for the `RecoveredMap` type, a newtype wrapper around
-- the `MonoidMap` type designed to provide the same semantics as `Map`.
--
module Main where

import Control.DeepSeq
    ( rnf )
import Control.Exception
    ( evaluate )
import Data.Eq
    ( Eq )
import Data.Function
    ( flip, ($) )
import Data.Int
    ( Int )
import Data.List
    ( foldl', zip )
import Data.Maybe
    ( Maybe, fromMaybe )
import Data.Ord
    ( Ord )
import Data.Semigroup
    ( Semigroup ((<>)), stimes )
import Prelude
    ( Integer, Num, (^), (+) )
import System.IO
    ( IO )
import Test.Tasty.Bench
    ( bench, bgroup, defaultMain, nf )

import qualified Data.Map.Strict as OMap
import qualified Data.MonoidMap.Internal.RecoveredMap as RMap

main :: IO ()
main = do

    let om_natural = fromList elems_natural :: OMap.Map Int Int
        om_even    = fromList elems_even    :: OMap.Map Int Int
        om_odd     = fromList elems_odd     :: OMap.Map Int Int

        rm_natural = fromList elems_natural :: RMap.Map Int Int
        rm_even    = fromList elems_even    :: RMap.Map Int Int
        rm_odd     = fromList elems_odd     :: RMap.Map Int Int

    evaluate $ rnf [om_natural, om_even, om_odd]
    evaluate $ rnf [rm_natural, rm_even, rm_odd]

    defaultMain
        [ bgroup "delete"
            [ bgroup "absent"
                [ bench "Data.Map.Strict" $
                    nf (deleteMany evens) om_odd
                , bench "RecoveredMap" $
                    nf (deleteMany evens) rm_odd
                ]
            , bgroup "present"
                [ bench "Data.Map.Strict" $
                    nf (deleteMany evens) om_even
                , bench "RecoveredMap" $
                    nf (deleteMany evens) rm_even
                ]
            ]
        , bgroup "insert"
            [ bgroup "absent"
                [ bench "Data.Map.Strict" $
                    nf (insertMany elems_even) om_odd
                , bench "RecoveredMap" $
                    nf (insertMany elems_even) rm_odd
                ]
            , bgroup "present"
                [ bench "Data.Map.Strict" $
                    nf (insertMany elems_even) om_even
                , bench "RecoveredMap" $
                    nf (insertMany elems_even) rm_even
                ]
            ]
        , bgroup "lookup"
            [ bgroup "absent"
                [ bench "Data.Map.Strict" $
                    nf (lookupMany evens) om_odd
                , bench "RecoveredMap" $
                    nf (lookupMany evens) rm_odd
                ]
            , bgroup "present"
                [ bench "Data.Map.Strict" $
                    nf (lookupMany evens) om_even
                , bench "RecoveredMap" $
                    nf (lookupMany evens) rm_even
                ]
            ]
        , bgroup "mappend"
            [ bgroup "disjoint"
                [ bench "Data.Map.Strict" $
                    nf (<> om_even) om_odd
                , bench "RecoveredMap" $
                    nf (<> rm_even) rm_odd
                ]
            , bgroup "identical"
                [ bench "Data.Map.Strict" $
                    nf (<> om_even) om_even
                , bench "RecoveredMap" $
                    nf (<> rm_even) rm_even
                ]
            ]
        , bgroup "stimes"
            [ bench "Data.Map.Strict" $
                nf (stimes ten_power_24) om_natural
            , bench "RecoveredMap" $
                nf (stimes ten_power_24) rm_natural
            ]
        , bgroup "mapAccumL"
            [ bench "Data.Map.Strict" $
                nf (mapAccumL (\s v -> (s + v, v)) 0) om_natural
            , bench "RecoveredMap" $
                nf (mapAccumL (\s v -> (s + v, v)) 0) rm_natural
            ]
        , bgroup "mapAccumR"
            [ bench "Data.Map.Strict" $
                nf (mapAccumR (\s v -> (s + v, v)) 0) om_natural
            , bench "RecoveredMap" $
                nf (mapAccumR (\s v -> (s + v, v)) 0) rm_natural
            ]
        , bgroup "mapAccumLWithKey"
            [ bench "Data.Map.Strict" $
                nf (mapAccumL (\s v -> (s + v, v)) 0) om_natural
            , bench "RecoveredMap" $
                nf (mapAccumL (\s v -> (s + v, v)) 0) rm_natural
            ]
        , bgroup "mapAccumRWithKey"
            [ bench "Data.Map.Strict" $
                nf (mapAccumRWithKey (\s k v -> (s + k + v, v)) 0) om_natural
            , bench "RecoveredMap" $
                nf (mapAccumRWithKey (\s k v -> (s + k + v, v)) 0) rm_natural
            ]
        ]
  where
    bound :: Int
    bound = 2 ^ (16 :: Int)

    elems_natural :: [(Int, Int)]
    elems_natural = zip naturals naturals

    elems_even :: [(Int, Int)]
    elems_even = zip evens evens

    elems_odd :: [(Int, Int)]
    elems_odd = zip odds odds

    naturals :: [Int]
    naturals = [1 .. bound]

    evens :: [Int]
    evens = [2, 4 .. bound]

    odds :: [Int]
    odds = [1, 3 .. bound]

    ten_power_24 :: Integer
    ten_power_24 = 1_000_000_000_000_000_000_000_000

class Ord k => Map m k v where
    fromList :: [(k, v)] -> m k v
    delete :: k -> m k v -> m k v
    insert :: k -> v -> m k v -> m k v
    lookup :: k -> m k v -> Maybe v
    mapAccumL :: (s -> v -> (s, v)) -> s -> m k v -> (s, m k v)
    mapAccumR :: (s -> v -> (s, v)) -> s -> m k v -> (s, m k v)
    mapAccumLWithKey :: (s -> k -> v -> (s, v)) -> s -> m k v -> (s, m k v)
    mapAccumRWithKey :: (s -> k -> v -> (s, v)) -> s -> m k v -> (s, m k v)

instance Ord k => Map OMap.Map k v where
    fromList = OMap.fromList
    delete = OMap.delete
    insert = OMap.insert
    lookup = OMap.lookup
    mapAccumL = OMap.mapAccum
    mapAccumR f = OMap.mapAccumRWithKey (\s _ v -> f s v)
    mapAccumLWithKey = OMap.mapAccumWithKey
    mapAccumRWithKey = OMap.mapAccumRWithKey

instance (Ord k, Eq v) => Map RMap.Map k v where
    fromList = RMap.fromList
    delete = RMap.delete
    insert = RMap.insert
    lookup = RMap.lookup
    mapAccumL = RMap.mapAccumL
    mapAccumR = RMap.mapAccumR
    mapAccumLWithKey = RMap.mapAccumLWithKey
    mapAccumRWithKey = RMap.mapAccumRWithKey

deleteMany :: (Map m k v, Num v) => [k] -> m k v -> m k v
deleteMany xs m = foldl' (flip delete) m xs

insertMany :: (Map m k v, Num v) => [(k, v)] -> m k v -> m k v
insertMany xs m = foldl' (\m' (k, v) -> insert k v m') m xs

lookupMany :: (Map m k v, Num v) => [k] -> m k v -> v
lookupMany xs m = foldl' (\n k -> fromMaybe n (lookup k m)) 0 xs
