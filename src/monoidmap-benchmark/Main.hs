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
    ( bench, bgroup, defaultMain, whnf )

import qualified Data.Map.Strict as OMap
import qualified Data.MonoidMap.Internal.RecoveredMap as RMap

main :: IO ()
main = do

    let om_even = OMap.fromList elems_even :: OMap.Map Int Int
        om_odd  = OMap.fromList elems_odd  :: OMap.Map Int Int

        rm_even = RMap.fromList elems_even :: RMap.Map Int Int
        rm_odd  = RMap.fromList elems_odd  :: RMap.Map Int Int

    evaluate $ rnf [om_even, om_odd]
    evaluate $ rnf [rm_even, rm_odd]

    defaultMain
        [ bgroup "lookup (absent)"
            [ bench "Data.Map.Strict" $
                whnf (olookup evens) om_odd
            , bench "Data.MonoidMap" $
                whnf (rlookup evens) rm_odd
            ]
        , bgroup "lookup (present)"
            [ bench "Data.Map.Strict" $
                whnf (olookup evens) om_even
            , bench "Data.MonoidMap" $
                whnf (rlookup evens) rm_even
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

olookup :: [Int] -> OMap.Map Int Int -> Int
olookup xs m = foldl' (\n k -> fromMaybe n (OMap.lookup k m)) 0 xs

rlookup :: [Int] -> RMap.Map Int Int -> Int
rlookup xs m = foldl' (\n k -> fromMaybe n (RMap.lookup k m)) 0 xs
