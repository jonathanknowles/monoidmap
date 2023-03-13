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
import qualified Examples.RecoveredMap as RMap

main :: IO ()
main = do

    let om_even = fromList elems_even :: OMap.Map Int Int
        om_odd  = fromList elems_odd  :: OMap.Map Int Int

        rm_even = fromList elems_even :: RMap.Map Int Int
        rm_odd  = fromList elems_odd  :: RMap.Map Int Int

    evaluate $ rnf [om_even, om_odd]
    evaluate $ rnf [rm_even, rm_odd]

    defaultMain
        [ bgroup "delete"
            [ bgroup "absent"
                [ bench "Data.Map.Strict" $
                    nf (deleteMany evens) om_odd
                , bench "Data.MonoidMap" $
                    nf (deleteMany evens) rm_odd
                ]
            , bgroup "present"
                [ bench "Data.Map.Strict" $
                    nf (deleteMany evens) om_even
                , bench "Data.MonoidMap" $
                    nf (deleteMany evens) rm_even
                ]
            ]
        , bgroup "insert"
            [ bgroup "absent"
                [ bench "Data.Map.Strict" $
                    nf (insertMany elems_even) om_odd
                , bench "Data.MonoidMap" $
                    nf (insertMany elems_even) rm_odd
                ]
            , bgroup "present"
                [ bench "Data.Map.Strict" $
                    nf (insertMany elems_even) om_even
                , bench "Data.MonoidMap" $
                    nf (insertMany elems_even) rm_even
                ]
            ]
        , bgroup "lookup"
            [ bgroup "absent"
                [ bench "Data.Map.Strict" $
                    nf (lookupMany evens) om_odd
                , bench "Data.MonoidMap" $
                    nf (lookupMany evens) rm_odd
                ]
            , bgroup "present"
                [ bench "Data.Map.Strict" $
                    nf (lookupMany evens) om_even
                , bench "Data.MonoidMap" $
                    nf (lookupMany evens) rm_even
                ]
            ]
        , bgroup "mappend"
            [ bgroup "disjoint"
                [ bench "Data.Map.Strict" $
                    nf (<> om_even) om_odd
                , bench "Data.MonoidMap" $
                    nf (<> rm_even) rm_odd
                ]
            , bgroup "identical"
                [ bench "Data.Map.Strict" $
                    nf (<> om_even) om_even
                , bench "Data.MonoidMap" $
                    nf (<> rm_even) rm_even
                ]
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
    delete :: k -> m k v -> m k v
    insert :: k -> v -> m k v -> m k v
    lookup :: k -> m k v -> Maybe v

instance Ord k => Map OMap.Map k v where
    fromList = OMap.fromList
    delete = OMap.delete
    insert = OMap.insert
    lookup = OMap.lookup

instance (Ord k, Eq v) => Map RMap.Map k v where
    fromList = RMap.fromList
    delete = RMap.delete
    insert = RMap.insert
    lookup = RMap.lookup

deleteMany :: (Map m k v, Num v) => [k] -> m k v -> m k v
deleteMany xs m = foldl' (flip delete) m xs

insertMany :: (Map m k v, Num v) => [(k, v)] -> m k v -> m k v
insertMany xs m = foldl' (\m' (k, v) -> insert k v m') m xs

lookupMany :: (Map m k v, Num v) => [k] -> m k v -> v
lookupMany xs m = foldl' (\n k -> fromMaybe n (lookup k m)) 0 xs
