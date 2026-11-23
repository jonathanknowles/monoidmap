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
    ( const, flip, ($) )
import Data.Int
    ( Int )
import Data.List
    ( concat, filter, foldl', transpose )
import Data.Monoid
    ( Monoid (mempty), Sum (Sum) )
import Data.Semigroup
    ( Semigroup ((<>)) )
import Data.String
    ( String )
import Prelude
    ( Num ((+), (-)), even, odd, (^) )
import System.IO
    ( IO )
import Test.Tasty.Bench
    ( Benchmark, bcompare, bench, bgroup, defaultMain, nf )

import qualified Data.Map.Strict as OMap
import qualified Data.MonoidMap.Internal.NonNullMap as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.Adjust as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.FromMap as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.FromMapWith as MMap
import qualified Data.MonoidMap.Internal.NonNullMap.Get as MMap

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

    -- Cases for 'adjust', which together cover every kind of transition that
    -- a single call can make, from an old value v0 to a new value v1:
    --
    -- +----------+----------+-----------+-----------------------------+
    -- | Old      | New      | Map       | Detectable?                 |
    -- | value    | value    | changed?  |                             |
    -- | v0       | v1       |           |                             |
    -- +----------+----------+-----------+-----------------------------+
    -- |     null |     null | unchanged | yes (with 'null')           |
    -- |     null | non-null |   changed | yes (with 'null')           |
    -- | non-null |     null |   changed | yes (with 'null')           |
    -- | non-null | v1 is v0 | unchanged | yes (with pointer equality) |
    -- | non-null | v1 == v0 | unchanged | no  (no `Eq` context)       |
    -- | non-null | v1 /= v0 |   changed | no  (no `Eq` context)       |
    -- +----------+----------+-----------+-----------------------------+
    --
    -- In each case, every key is paired with a 'Change', which determines the
    -- transition.
    --
    let -- The input that produces each transition in the table above, for the
        -- given keys, given:
        --
        -- m0: a map in which those keys are associated with 'Sum 0'; and
        -- m1: a map in which those keys are associated with 'Sum 1':
        --
        transitions ks m0 m1 =
            [ ("null to null",          (withChange ks Identity, m0))
            , ("null to non-null",      (withChange ks (Add 1) , m0))
            , ("non-null to null",      (withChange ks (Sub 1) , m1))
            , ("non-null to identical", (withChange ks Identity, m1))
            , ("non-null to equal",     (withChange ks (Add 0) , m1))
            , ("non-null to unequal",   (withChange ks (Add 1) , m1))
            ]

    let adjustCases = concat $ transpose
            [ withSuffix " (even)" adjustCasesEven
            , withSuffix " (odd)"  adjustCasesOdd
            ]
          where
            adjustCasesEven = transitions keys_even mm_keys_odd  mm_keys_even
            adjustCasesOdd  = transitions keys_odd  mm_keys_even mm_keys_odd

    evaluate $ rnf adjustCases

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

    defaultMain
        [ compareImplementations
            ("adjust", adjustWith MMap.adjust) adjustCases
            [ ( "adjust_alter"
              , adjustWith MMap.adjust_alter
              )
            ]
        , compareImplementations
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

    -- Pairs every one of the given keys with the given change.
    withChange :: [Int] -> Change -> [(Int, Change)]
    withChange ks c = [(k, c) | k <- ks]

    -- Appends the given suffix to the label of every case.
    withSuffix :: String -> [(String, i)] -> [(String, i)]
    withSuffix s cases = [(label <> s, input) | (label, input) <- cases]

    -- Makes the given change to a value.
    change :: Change -> Sum Int -> Sum Int
    change Identity v = v
    change (Add n)  v = v + Sum n
    change (Sub n)  v = v - Sum n

    -- Adjusts the values associated with all of the given keys in turn, with
    -- the given implementation of 'adjust', by making the change that
    -- accompanies each key.
    --
    adjustWith
        :: ((Sum Int -> Sum Int)
            -> Int
            -> MMap.NonNullMap Int (Sum Int)
            -> MMap.NonNullMap Int (Sum Int))
        -> ([(Int, Change)], MMap.NonNullMap Int (Sum Int))
        -> MMap.NonNullMap Int (Sum Int)
    adjustWith adjust = adjustAll (\(k, c) m -> adjust (change c) k m)
    {-# INLINE adjustWith #-}

    -- Applies the given function to each of the given keys in turn, together
    -- with its change, threading the map through each call.
    --
    adjustAll
        :: ((Int, Change)
            -> MMap.NonNullMap Int (Sum Int)
            -> MMap.NonNullMap Int (Sum Int))
        -> ([(Int, Change)], MMap.NonNullMap Int (Sum Int))
        -> MMap.NonNullMap Int (Sum Int)
    adjustAll step (kcs, m) = foldl' (flip step) m kcs
    {-# NOINLINE adjustAll #-}

-- | A change to make to a value, with the function passed to 'adjust'.
--
-- The change is chosen for each key separately, and is only known at run time.
-- So the function passed to 'adjust' captures a value that can differ from one
-- call to the next, as in typical use (for example, with `(x :)`). And the
-- compiler can't simplify 'Add 0' to 'Identity': 'Add' always builds a new
-- value.
--
data Change
    = Identity
      -- ^ Returns the value itself.
    | Add Int
      -- ^ Adds to the value.
    | Sub Int
      -- ^ Subtracts from the value.

instance NFData Change where
    rnf Identity = ()
    rnf (Add n)  = rnf n
    rnf (Sub n)  = rnf n

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
