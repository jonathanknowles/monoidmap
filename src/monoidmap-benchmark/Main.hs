{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Prelude hiding
    ( lookup )

import Control.DeepSeq
    ( rnf )
import Control.Exception
    ( evaluate )
import Data.List
    ( foldl' )
import Data.Maybe
    ( fromMaybe )
import Test.Tasty.Bench
    ( bench, bgroup, defaultMain, nf )

import qualified Data.Map.Strict as OMap
import qualified Data.MonoidMap.Internal.RecoveredMap as RMap

main :: IO ()
main = do

    let om_even = fromList elems_even :: OMap.Map Int Int
        om_odd  = fromList elems_odd  :: OMap.Map Int Int

        rm_even = fromList elems_even :: RMap.Map Int Int
        rm_odd  = fromList elems_odd  :: RMap.Map Int Int

    evaluate $ rnf [om_even, om_odd]
    evaluate $ rnf [rm_even, rm_odd]

    defaultMain
        [ bgroup "lookup (absent)"
            [ bench "Data.Map.Strict" $
                nf (lookupMany evens) om_odd
            , bench "Data.MonoidMap" $
                nf (lookupMany evens) rm_odd
            ]
        , bgroup "lookup (present)"
            [ bench "Data.Map.Strict" $
                nf (lookupMany evens) om_even
            , bench "Data.MonoidMap" $
                nf (lookupMany evens) rm_even
            ]
        ]
  where
    bound :: Int
    bound = 2 ^ (16 :: Int)

    elems_even :: [(Int, Int)]
    elems_even = zip evens evens

    elems_odd :: [(Int, Int)]
    elems_odd = zip odds odds

    evens :: [Int]
    evens = [2, 4 .. bound]

    odds :: [Int]
    odds = [1, 3 .. bound]

class Ord k => Map m k v where
    fromList :: [(k, v)] -> m k v
    lookup :: k -> m k v -> Maybe v

instance Ord k => Map OMap.Map k v where
    fromList = OMap.fromList
    lookup = OMap.lookup

instance Ord k => Map RMap.Map k v where
    fromList = RMap.fromList
    lookup = RMap.lookup

lookupMany :: (Map m k v, Num v) => [k] -> m k v -> v
lookupMany xs m = foldl' (\n k -> fromMaybe n (lookup k m)) 0 xs
