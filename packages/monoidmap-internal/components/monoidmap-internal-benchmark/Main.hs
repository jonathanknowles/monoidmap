-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Benchmark for internal implementations of 'MonoidMap' functions.
--
-- When a 'MonoidMap' function has alternative internal implementations (such
-- as the 'fromMap' function), we benchmark each alternative implementation
-- against the public function.
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
    ( const, ($) )
import Data.Int
    ( Int )
import Data.Monoid
    ( Monoid (mempty), Sum (Sum) )
import Data.Semigroup
    ( Semigroup ((<>)) )
import Data.String
    ( String )
import Prelude
    ( even, odd, (^) )
import System.IO
    ( IO )
import Test.Tasty.Bench
    ( Benchmark, bcompare, bench, bgroup, defaultMain, nf )

import qualified Data.Map.Strict as OMap
import qualified Data.MonoidMap.Internal.NonNullMap as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.FromMap as MMap

main :: IO ()
main = do

    let -- Maps in which different subsets of keys have 'null' values:
        om_null_none = mapWithNullIf (const False)   :: OMap.Map Int (Sum Int)
        om_null_min  = mapWithNullIf (== minNatural) :: OMap.Map Int (Sum Int)
        om_null_max  = mapWithNullIf (== maxNatural) :: OMap.Map Int (Sum Int)
        om_null_even = mapWithNullIf even            :: OMap.Map Int (Sum Int)
        om_null_odd  = mapWithNullIf odd             :: OMap.Map Int (Sum Int)
        om_null_all  = mapWithNullIf (const True)    :: OMap.Map Int (Sum Int)

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
        ]
  where
    minNatural :: Int
    minNatural = 1

    maxNatural :: Int
    maxNatural = 2 ^ (16 :: Int)

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
