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
import Data.Char
    ( Char )
import Data.Eq
    ( Eq ((==)) )
import Data.Function
    ( const, ($) )
import Data.Int
    ( Int )
import Data.List
    ( filter, foldl', replicate )
import Data.Monoid
    ( Monoid (mempty), Sum (Sum) )
import Data.Semigroup
    ( Semigroup ((<>)) )
import Data.String
    ( String )
import Prelude
    ( even, odd, (^) )
import Numeric.Natural
    ( Natural )
import System.IO
    ( IO )
import Test.Tasty.Bench
    ( Benchmark, bcompare, bench, bgroup, defaultMain, nf )

import qualified Data.Map.Strict as OMap
import qualified Data.MonoidMap.Internal.NonNullMap as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.FromMap as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.FromMapWith as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.Get as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.StripCommonPrefix as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.StripCommonSuffix as MMap

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

    let -- Map with keys that are all even:
        mm_keys_even = MMap.fromMap om_null_odd
        -- Map with keys that are all odd:
        mm_keys_odd  = MMap.fromMap om_null_even

        keys_even = filter even [minNatural .. maxNatural]
        keys_odd  = filter odd  [minNatural .. maxNatural]

    evaluate $ rnf
        [ mm_keys_even
        , mm_keys_odd
        ]
    evaluate $ rnf
        [ keys_even
        , keys_odd
        ]

    let fromMapCases =
            [ ("no nulls",           om_null_none)
            , ("one null (min key)", om_null_min)
            , ("one null (max key)", om_null_max)
            , ("even nulls",         om_null_even)
            , ("odd nulls",          om_null_odd)
            , ("all nulls",          om_null_all)
            ]

    let getCases =
            [ ("all keys present (even)", (keys_even, mm_keys_even))
            , ("all keys present (odd)",  (keys_odd,  mm_keys_odd))
            , ("no keys present (even)",  (keys_even, mm_keys_odd))
            , ("no keys present (odd)",   (keys_odd,  mm_keys_even))
            ]

    let -- Maps with 'Sum Natural' values, for which operations on individual
        -- values are cheap:
        mn_2_all  = sumMap 2 (const True)
        mn_3_all  = sumMap 3 (const True)
        mn_2_even = sumMap 2 even
        mn_3_odd  = sumMap 3 odd

        -- Maps with 'String' values, for which operations on individual
        -- values are relatively expensive:
        ms_x_all  = stringMap 'x' (const True)
        ms_y_all  = stringMap 'y' (const True)
        ms_x_even = stringMap 'x' even
        ms_y_odd  = stringMap 'y' odd

    evaluate $ rnf
        [ mn_2_all
        , mn_3_all
        , mn_2_even
        , mn_3_odd
        ]
    evaluate $ rnf
        [ ms_x_all
        , ms_y_all
        , ms_x_even
        , ms_y_odd
        ]

    -- Implementations of 'stripCommonPrefix' and 'stripCommonSuffix' can
    -- handle keys present in only one map quite differently from keys
    -- present in both maps, so we benchmark both kinds of key:
    --
    let stripCasesSum =
            [ ("Sum Natural, same keys",     (mn_2_all,  mn_3_all))
            , ("Sum Natural, disjoint keys", (mn_2_even, mn_3_odd))
            ]

    let stripCasesString =
            [ ("String, same keys",     (ms_x_all,  ms_y_all))
            , ("String, disjoint keys", (ms_x_even, ms_y_odd))
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
        , compareImplementations
            ("get", getAll MMap.get) getCases
            [ ( "get_findWithDefault"
              , getAll MMap.get_findWithDefault
              )
            , ( "get_lookup"
              , getAll MMap.get_lookup
              )
            ]
        , compareImplementations
            ("stripCommonPrefix", applyPair MMap.stripCommonPrefix)
            stripCasesSum
            [ ( "stripCommonPrefix_default"
              , applyPair MMap.stripCommonPrefix_default
              )
            , ( "stripCommonPrefix_triples"
              , applyPair MMap.stripCommonPrefix_triples
              )
            , ( "stripCommonPrefix_triples_matched"
              , applyPair MMap.stripCommonPrefix_triples_matched
              )
            , ( "stripCommonPrefix_lists"
              , applyPair MMap.stripCommonPrefix_lists
              )
            , ( "stripCommonPrefix_merge3"
              , applyPair MMap.stripCommonPrefix_merge3
              )
            ]
        , compareImplementations
            ("stripCommonPrefix", applyPair MMap.stripCommonPrefix)
            stripCasesString
            [ ( "stripCommonPrefix_default"
              , applyPair MMap.stripCommonPrefix_default
              )
            , ( "stripCommonPrefix_triples"
              , applyPair MMap.stripCommonPrefix_triples
              )
            , ( "stripCommonPrefix_triples_matched"
              , applyPair MMap.stripCommonPrefix_triples_matched
              )
            , ( "stripCommonPrefix_lists"
              , applyPair MMap.stripCommonPrefix_lists
              )
            , ( "stripCommonPrefix_merge3"
              , applyPair MMap.stripCommonPrefix_merge3
              )
            ]
        , compareImplementations
            ("stripCommonSuffix", applyPair MMap.stripCommonSuffix)
            stripCasesSum
            [ ( "stripCommonSuffix_default"
              , applyPair MMap.stripCommonSuffix_default
              )
            , ( "stripCommonSuffix_triples"
              , applyPair MMap.stripCommonSuffix_triples
              )
            , ( "stripCommonSuffix_triples_matched"
              , applyPair MMap.stripCommonSuffix_triples_matched
              )
            , ( "stripCommonSuffix_lists"
              , applyPair MMap.stripCommonSuffix_lists
              )
            , ( "stripCommonSuffix_merge3"
              , applyPair MMap.stripCommonSuffix_merge3
              )
            ]
        , compareImplementations
            ("stripCommonSuffix", applyPair MMap.stripCommonSuffix)
            stripCasesString
            [ ( "stripCommonSuffix_default"
              , applyPair MMap.stripCommonSuffix_default
              )
            , ( "stripCommonSuffix_triples"
              , applyPair MMap.stripCommonSuffix_triples
              )
            , ( "stripCommonSuffix_triples_matched"
              , applyPair MMap.stripCommonSuffix_triples_matched
              )
            , ( "stripCommonSuffix_lists"
              , applyPair MMap.stripCommonSuffix_lists
              )
            , ( "stripCommonSuffix_merge3"
              , applyPair MMap.stripCommonSuffix_merge3
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

    -- Gets the values associated with all of the given keys, and combines them
    -- into a single value.
    --
    -- This function is marked with `NOINLINE` in order to ensure that every
    -- implementation of 'get' is called from exactly the same loop.
    --
    -- Otherwise, the compiler could compile a separate copy of the loop for
    -- each implementation, and differences between the copies could outweigh
    -- differences between the implementations themselves.
    --
    getAll
        :: (Int -> MMap.NonNullMap Int (Sum Int) -> Sum Int)
        -> ([Int], MMap.NonNullMap Int (Sum Int))
        -> Sum Int
    getAll get (ks, m) = foldl' (\v k -> v <> get k m) mempty ks
    {-# NOINLINE getAll #-}

    -- A map from keys [minNatural .. maxNatural] that satisfy the given
    -- predicate, with the given value for every key.
    sumMap :: Natural -> (Int -> Bool) -> MMap.NonNullMap Int (Sum Natural)
    sumMap n p = MMap.fromMap $ OMap.fromList
        [(k, Sum n) | k <- [minNatural .. maxNatural], p k]

    -- A map from keys [1 .. 2 ^ 12] that satisfy the given predicate, where
    -- the value for every key has the given character in the middle, with a
    -- long common prefix and suffix on either side.
    stringMap :: Char -> (Int -> Bool) -> MMap.NonNullMap Int String
    stringMap c p = MMap.fromMap $ OMap.fromList
        [(k, padding <> [c] <> padding) | k <- [1 .. 2 ^ (12 :: Int)], p k]
      where
        padding = replicate 32 'a'

    -- Applies the given function to both maps in the given pair.
    --
    -- This function is marked with `NOINLINE` in order to ensure that every
    -- implementation is called in exactly the same way, for the same reason
    -- as 'getAll'.
    --
    applyPair :: (m -> m -> r) -> (m, m) -> r
    applyPair f (m1, m2) = f m1 m2
    {-# NOINLINE applyPair #-}

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
