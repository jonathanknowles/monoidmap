-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Benchmark for the 'MonoidMap' type and its functions.
--
-- When a 'MonoidMap' function has a direct counterpart in 'Map' (such as the
-- 'fromList' function), we benchmark 'Map' against 'RecoveredMap': a newtype
-- wrapper around 'MonoidMap' with functions that have the same semantics as
-- those of 'Map', so that both sides of each comparison behave identically.
--
-- When a 'MonoidMap' function has no direct counterpart in 'Map' (such as the
-- 'fromMap' function), we benchmark the 'MonoidMap' function directly.
--
module Main where

import Control.DeepSeq
    ( NFData, rnf )
import Control.Exception
    ( evaluate )
import Data.Bool
    ( Bool (False, True) )
import Data.Eq
    ( Eq ((==)) )
import Data.Function
    ( const, flip, ($) )
import Data.Int
    ( Int )
import Data.List
    ( filter, foldl', zip )
import Data.Maybe
    ( Maybe, fromMaybe )
import Data.Monoid
    ( Monoid (mempty), Sum (Sum) )
import Data.Ord
    ( Ord )
import Data.Semigroup
    ( Semigroup ((<>)), stimes )
import Data.String
    ( String )
import Prelude
    ( Integer, Num, even, odd, (^), (+) )
import System.IO
    ( IO )
import Test.Tasty.Bench
    ( Benchmark, bcompare, bench, bgroup, defaultMain, nf )

import qualified Data.Map.Strict as OMap
import qualified Data.MonoidMap.Internal as MMap
import qualified Data.MonoidMap.Internal.RecoveredMap as RMap

main :: IO ()
main = do

    let om_natural = fromList elems_natural :: OMap.Map Int Int
        om_even    = fromList elems_even    :: OMap.Map Int Int
        om_odd     = fromList elems_odd     :: OMap.Map Int Int

        rm_natural = fromList elems_natural :: RMap.Map Int Int
        rm_even    = fromList elems_even    :: RMap.Map Int Int
        rm_odd     = fromList elems_odd     :: RMap.Map Int Int

        -- Maps in which different subsets of keys have 'null' values:
        om_null_none = mapWithNullIf (const False)   :: OMap.Map Int (Sum Int)
        om_null_min  = mapWithNullIf (== minNatural) :: OMap.Map Int (Sum Int)
        om_null_max  = mapWithNullIf (== maxNatural) :: OMap.Map Int (Sum Int)
        om_null_even = mapWithNullIf even            :: OMap.Map Int (Sum Int)
        om_null_odd  = mapWithNullIf odd             :: OMap.Map Int (Sum Int)
        om_null_all  = mapWithNullIf (const True)    :: OMap.Map Int (Sum Int)

    evaluate $ rnf [om_natural, om_even, om_odd]
    evaluate $ rnf [rm_natural, rm_even, rm_odd]
    evaluate $ rnf
        [ om_null_none
        , om_null_min
        , om_null_max
        , om_null_even
        , om_null_odd
        , om_null_all
        ]

    let fromMapCases =
            [ ("no nulls",           om_null_none)
            , ("one null (min key)", om_null_min)
            , ("one null (max key)", om_null_max)
            , ("even nulls",         om_null_even)
            , ("odd nulls",          om_null_odd)
            , ("all nulls",          om_null_all)
            ]

    defaultMain
        [ compareImplementations
            ("fromMap", MMap.fromMap) fromMapCases
            [ ( "fromMap_filter"
              , MMap.fromMap_filter
              )
            , ( "fromMap_filter_if_any_null"
              , MMap.fromMap_filter_if_any_null
              )
            , ( "fromMap_mapMaybe"
              , MMap.fromMap_mapMaybe
              )
            ]
        , compareImplementations
            ("fromMapWith", MMap.fromMapWith double) fromMapCases
            [ ( "fromMapWith_mapMaybe"
              , MMap.fromMapWith_mapMaybe double
              )
            , ( "fromMapWith_map_filter"
              , MMap.fromMapWith_map_filter double
              )
            , ( "fromMapWith_map_filter_if_any_null"
              , MMap.fromMapWith_map_filter_if_any_null double
              )
            ]
        , bgroup "delete"
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
    minNatural :: Int
    minNatural = 1

    maxNatural :: Int
    maxNatural = 2 ^ (16 :: Int)

    elems_natural :: [(Int, Int)]
    elems_natural = zip naturals naturals

    elems_even :: [(Int, Int)]
    elems_even = zip evens evens

    elems_odd :: [(Int, Int)]
    elems_odd = zip odds odds

    naturals :: [Int]
    naturals = [minNatural .. maxNatural]

    evens :: [Int]
    evens = filter even naturals

    odds :: [Int]
    odds = filter odd naturals

    ten_power_24 :: Integer
    ten_power_24 = 1_000_000_000_000_000_000_000_000

    -- A map from keys [minNatural .. maxNatural], with a null value for each
    -- key that satisfies the given predicate, and a non-null value otherwise.
    mapWithNullIf :: (Int -> Bool) -> OMap.Map Int (Sum Int)
    mapWithNullIf hasNullValue = OMap.fromList
        [ (k, if hasNullValue k then mempty else Sum 1)
        | k <- [minNatural .. maxNatural]
        ]

    -- A simple function that replaces every value with a new value, while:
    -- - preserving existing null values; and
    -- - not introducing any new null values.
    double :: Semigroup m => m -> m
    double m = m <> m

-- | Compares implementations of a public function with the public function
--   itself, for each of the given cases.
--
-- For each case, the time taken by each implementation is reported relative
-- to the time taken by the public function.
--
compareImplementations
    :: NFData r
    => (String, i -> r)
    -- ^ The public function, with its name (also used as the group name).
    -> [(String, i)]
    -- ^ Cases (inputs) to benchmark, each with a label.
    -> [(String, i -> r)]
    -- ^ Implementations of the public function, each with a name.
    -> Benchmark
compareImplementations (publicName, public) cases implementations =
    bgroup publicName
        [ bgroup label $
            bench publicName (nf public input) :
            [ bcompare (relativeToPublic label) $ bench name $ nf f input
            | (name, f) <- implementations
            ]
        | (label, input) <- cases
        ]
  where
    -- Matches the benchmark of the public function for the given case (and
    -- nothing else):
    relativeToPublic label =
        "$NF == \"" <> publicName <> "\""
        <> " && $(NF-1) == \"" <> label <> "\""
        <> " && $(NF-2) == \"" <> publicName <> "\""

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
