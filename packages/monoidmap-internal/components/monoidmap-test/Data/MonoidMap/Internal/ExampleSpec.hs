{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MonoidMap.Internal.ExampleSpec
    where

import Prelude hiding
    ( gcd, lcm )

import Data.Data
    ( Data, cast, gmapT )
import Data.Function
    ( (&) )
import Data.Group
    ( Group (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Monoid
    ( Product (..), Sum (..) )
import Data.Monoid.GCD
    ( GCDMonoid (..), LeftGCDMonoid (..), RightGCDMonoid (..) )
import Data.Monoid.LCM
    ( LCMMonoid (..) )
import Data.Monoid.Monus
    ( OverlappingGCDMonoid (..), (<\>) )
import Data.MonoidMap.Internal
    ( MonoidMap )
import Data.Ratio
    ( (%) )
import Data.Semigroup.Cancellative
    ( LeftReductive (..), RightReductive (..) )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Test.Common
    ()
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.Unit
    ( UnitTestData1
    , UnitTestData2
    , unitTestData1
    , unitTestData2
    , unitTestSpec
    )

import qualified Data.MonoidMap.Internal as MonoidMap
import qualified Data.Set as Set

spec :: Spec
spec = describe "Examples" $ do

    describe "Conversion" $ do

        exampleSpec_fromList_String
        exampleSpec_toList_String

    describe "Comparison" $ do

        exampleSpec_isSubmapOf_Sum_Natural
        exampleSpec_disjoint_Product_Natural
        exampleSpec_disjoint_Sum_Natural
        exampleSpec_disjoint_Set_Natural

    describe "Data" $ do

        exampleSpec_gmapT_keys_id
        exampleSpec_gmapT_keys_div_2
        exampleSpec_gmapT_keys_mod_2
        exampleSpec_gmapT_keys_mul_2

        exampleSpec_gmapT_values_id
        exampleSpec_gmapT_values_div_2
        exampleSpec_gmapT_values_mod_2
        exampleSpec_gmapT_values_mul_2

    describe "Intersection" $ do

        exampleSpec_intersectionWith_min_Sum_Natural

    describe "Union" $ do

        exampleSpec_unionWith_max_Sum_Natural

    describe "Semigroup" $ do

        exampleSpec_Semigroup_mappend_String
        exampleSpec_Semigroup_mappend_Sum_Natural

    describe "Group" $ do

        exampleSpec_Group_invert_Product_Rational
        exampleSpec_Group_invert_Sum_Integer
        exampleSpec_Group_pow_Product_Rational
        exampleSpec_Group_pow_Sum_Integer
        exampleSpec_Group_subtract_Product_Rational
        exampleSpec_Group_subtract_Sum_Integer

    describe "Reductive" $ do

        exampleSpec_Reductive_isPrefixOf_String
        exampleSpec_Reductive_isPrefixOf_Sum_Natural
        exampleSpec_Reductive_isSuffixOf_String
        exampleSpec_Reductive_isSuffixOf_Sum_Natural
        exampleSpec_Reductive_stripPrefix_String
        exampleSpec_Reductive_stripPrefix_Sum_Natural
        exampleSpec_Reductive_stripSuffix_String
        exampleSpec_Reductive_stripSuffix_Sum_Natural

    describe "LeftGCDMonoid" $ do

        exampleSpec_LeftGCDMonoid_commonPrefix_String
        exampleSpec_LeftGCDMonoid_commonPrefix_Sum_Natural
        exampleSpec_LeftGCDMonoid_stripCommonPrefix_String
        exampleSpec_LeftGCDMonoid_stripCommonPrefix_Sum_Natural

    describe "RightGCDMonoid" $ do

        exampleSpec_RightGCDMonoid_commonSuffix_String
        exampleSpec_RightGCDMonoid_commonSuffix_Sum_Natural
        exampleSpec_RightGCDMonoid_stripCommonSuffix_String
        exampleSpec_RightGCDMonoid_stripCommonSuffix_Sum_Natural

    describe "OverlappingGCDMonoid" $ do

        exampleSpec_OverlappingGCDMonoid_overlap_String
        exampleSpec_OverlappingGCDMonoid_overlap_Sum_Natural
        exampleSpec_OverlappingGCDMonoid_stripPrefixOverlap_String
        exampleSpec_OverlappingGCDMonoid_stripPrefixOverlap_Sum_Natural
        exampleSpec_OverlappingGCDMonoid_stripSuffixOverlap_String
        exampleSpec_OverlappingGCDMonoid_stripSuffixOverlap_Sum_Natural

    describe "GCDMonoid" $ do

        exampleSpec_GCDMonoid_gcd_Product_Natural
        exampleSpec_GCDMonoid_gcd_Sum_Natural
        exampleSpec_GCDMonoid_gcd_Set_Natural

    describe "LCMMonoid" $ do

        exampleSpec_LCMMonoid_lcm_Product_Natural
        exampleSpec_LCMMonoid_lcm_Sum_Natural
        exampleSpec_LCMMonoid_lcm_Set_Natural

    describe "Monus" $ do

        exampleSpec_Monus_monus_Set_Natural
        exampleSpec_Monus_monus_Sum_Natural

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

exampleSpec_fromList_String :: Spec
exampleSpec_fromList_String = unitTestSpec
    "MonoidMap.fromList (String)"
    "MonoidMap.fromList"
    (MonoidMap.fromList)
    (exampleData_fromList_String)

exampleData_fromList_String :: UnitTestData1
    [(Int, String)]
    (MonoidMap Int String)
exampleData_fromList_String = unitTestData1
    [ ( [(1, "a"), (2, "x"), (1, "b"), (2, "y"), (1, "c"), (2, "z")]
      , [(1, "abc"), (2, "xyz")]
      )
    ]

exampleSpec_toList_String :: Spec
exampleSpec_toList_String = unitTestSpec
    "MonoidMap.toList (String)"
    "MonoidMap.toList"
    (MonoidMap.toList)
    (exampleData_toList_String)

exampleData_toList_String :: UnitTestData1
    (MonoidMap Int String)
    [(Int, String)]
exampleData_toList_String = unitTestData1
    [ ( [(3, "z"), (2, "y"), (1, "x")]
      , [(1, "x"), (2, "y"), (3, "z")]
      )
    ]

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

exampleSpec_isSubmapOf_Sum_Natural :: Spec
exampleSpec_isSubmapOf_Sum_Natural = unitTestSpec
    "MonoidMap.isSubmapOf (Sum Natural)"
    "MonoidMap.isSubmapOf"
    (MonoidMap.isSubmapOf)
    (exampleData_isSubmapOf_Sum_Natural)

exampleData_isSubmapOf_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (Bool)
exampleData_isSubmapOf_Sum_Natural = unitTestData2
    [ ( m [0, 1, 2, 3]
      , m [4, 4, 4, 4]
      , True
      )
    , ( m [0, 1, 2, 3]
      , m [0, 4, 4, 4]
      , True
      )
    , ( m [0, 1, 2, 3]
      , m [0, 1, 4, 4]
      , True
      )
    , ( m [0, 1, 2, 3]
      , m [0, 1, 2, 4]
      , True
      )
    , ( m [0, 1, 2, 3]
      , m [0, 1, 2, 3]
      , True
      )
    , ( m [0, 1, 2, 3]
      , m [0, 0, 2, 3]
      , False
      )
    , ( m [0, 1, 2, 3]
      , m [0, 1, 1, 3]
      , False
      )
    , ( m [0, 1, 2, 3]
      , m [0, 1, 2, 2]
      , False
      )
    , ( m [0, 1, 2, 3]
      , m [0, 0, 0, 0]
      , False
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_disjoint_Product_Natural :: Spec
exampleSpec_disjoint_Product_Natural = unitTestSpec
    "MonoidMap.disjoint (Product Natural)"
    "MonoidMap.disjoint"
    (MonoidMap.disjoint)
    (exampleData_disjoint_Product_Natural)

exampleData_disjoint_Product_Natural :: UnitTestData2
    (MonoidMap LatinChar (Product Natural))
    (MonoidMap LatinChar (Product Natural))
    (Bool)
exampleData_disjoint_Product_Natural = unitTestData2
    [ ( m []
      , m []
      , True
      )
    , ( m [2, 3, 5, 7]
      , m [3, 5, 7, 2]
      , True
      )
    , ( m [2 * 3, 5 * 7]
      , m [5 * 7, 2 * 3]
      , True
      )
    , ( m [2 * 3    , 3 * 5    ]
      , m [    3 * 5,     5 * 7]
      , False
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_disjoint_Sum_Natural :: Spec
exampleSpec_disjoint_Sum_Natural = unitTestSpec
    "MonoidMap.disjoint (Sum Natural)"
    "MonoidMap.disjoint"
    (MonoidMap.disjoint)
    (exampleData_disjoint_Sum_Natural)

exampleData_disjoint_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (Bool)
exampleData_disjoint_Sum_Natural = unitTestData2
    [ ( m []
      , m []
      , True
      )
    , ( m [0, 1, 0, 1]
      , m [1, 0, 1, 0]
      , True
      )
    , ( m [0, 8, 0, 8]
      , m [8, 0, 8, 0]
      , True
      )
    , ( m [0, 8, 0, 8]
      , m [8, 0, 8, 1]
      , False
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_disjoint_Set_Natural :: Spec
exampleSpec_disjoint_Set_Natural = unitTestSpec
    "MonoidMap.disjoint (Set Natural)"
    "MonoidMap.disjoint"
    (MonoidMap.disjoint)
    (exampleData_disjoint_Set_Natural)

exampleData_disjoint_Set_Natural :: UnitTestData2
    (MonoidMap LatinChar (Set Natural))
    (MonoidMap LatinChar (Set Natural))
    (Bool)
exampleData_disjoint_Set_Natural = unitTestData2
    [ ( m []
      , m []
      , True
      )
    , ( m [[1], [2], [3], [4]]
      , m [[5], [6], [7], [8]]
      , True
      )
    , ( m [[1, 2], [3, 4]]
      , m [[5, 6], [7, 8]]
      , True
      )
    , ( m [[1, 2   ], [3, 4   ]]
      , m [[   2, 3], [   4, 5]]
      , False
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Set.fromList

--------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------

exampleSpec_gmapT_keys_id :: Spec
exampleSpec_gmapT_keys_id =
    unitTestSpec "gmapT_keys" "id"
    (mapNaturals id)
    (unitTestData1
      [ ( [(1, "a"), (2, "b"), (3, "c"), (4, "d")] :: MonoidMap Natural String
        , [(1, "a"), (2, "b"), (3, "c"), (4, "d")] :: MonoidMap Natural String
        )
      ]
    )

exampleSpec_gmapT_keys_div_2 :: Spec
exampleSpec_gmapT_keys_div_2 =
    unitTestSpec "gmapT_keys" "div_2"
    (mapNaturals (`div` 2))
    (unitTestData1
      [ ( [(2, "a"), (4, "b"), (6, "c"), (8, "d")] :: MonoidMap Natural String
        , [(1, "a"), (2, "b"), (3, "c"), (4, "d")] :: MonoidMap Natural String
        )
      ]
    )

exampleSpec_gmapT_keys_mod_2 :: Spec
exampleSpec_gmapT_keys_mod_2 =
    unitTestSpec "gmapT_keys" "mod_2"
    (mapNaturals (`mod` 2))
    (unitTestData1
      [ ( [(0, "a"), (2, "b"), (1, "c"), (3, "d")] :: MonoidMap Natural String
        , [(0, "a"    <> "b"), (1, "c"    <> "d")] :: MonoidMap Natural String
        )
      ]
    )

exampleSpec_gmapT_keys_mul_2 :: Spec
exampleSpec_gmapT_keys_mul_2 =
    unitTestSpec "gmapT_keys" "mul_2"
    (mapNaturals (* 2))
    (unitTestData1
      [ ( [(1, "a"), (2, "b"), (3, "c"), (4, "d")] :: MonoidMap Natural String
        , [(2, "a"), (4, "b"), (6, "c"), (8, "d")] :: MonoidMap Natural String
        )
      ]
    )

exampleSpec_gmapT_values_id :: Spec
exampleSpec_gmapT_values_id =
    unitTestSpec "gmapT_values" "id"
    (mapNaturals id)
    (unitTestData1
      [ ( [(A, 1), (B, 2), (C, 3), (D, 4)] :: MonoidMap LatinChar (Sum Natural)
        , [(A, 1), (B, 2), (C, 3), (D, 4)] :: MonoidMap LatinChar (Sum Natural)
        )
      ]
    )

exampleSpec_gmapT_values_div_2 :: Spec
exampleSpec_gmapT_values_div_2 =
    unitTestSpec "gmapT_values" "div_2"
    (mapNaturals (`div` 2))
    (unitTestData1
      [ ( [(A, 2), (B, 4), (C, 6), (D, 8)] :: MonoidMap LatinChar (Sum Natural)
        , [(A, 1), (B, 2), (C, 3), (D, 4)] :: MonoidMap LatinChar (Sum Natural)
        )
      ]
    )

exampleSpec_gmapT_values_mod_2 :: Spec
exampleSpec_gmapT_values_mod_2 =
    unitTestSpec "gmapT_values" "mod_2"
    (mapNaturals (`mod` 2))
    (unitTestData1
      [ ( [(A, 1), (B, 2), (C, 3), (D, 4)] :: MonoidMap LatinChar (Sum Natural)
        , [(A, 1),         (C, 1)        ] :: MonoidMap LatinChar (Sum Natural)
        )
      ]
    )

exampleSpec_gmapT_values_mul_2 :: Spec
exampleSpec_gmapT_values_mul_2 =
    unitTestSpec "gmapT_values" "mul_2"
    (mapNaturals (* 2))
    (unitTestData1
      [ ( [(A, 1), (B, 2), (C, 3), (D, 4)] :: MonoidMap LatinChar (Sum Natural)
        , [(A, 2), (B, 4), (C, 6), (D, 8)] :: MonoidMap LatinChar (Sum Natural)
        )
      ]
    )

mapNaturals :: Data a => (Natural -> Natural) -> a -> a
mapNaturals f =
    everywhereT $ \x ->
        case (cast x) of
            Just (n :: Natural) ->
                fromMaybe
                    (error "mapNaturals")
                    (cast (f n))
            Nothing -> x

everywhereT :: Data a => (forall b. Data b => b -> b) -> a -> a
everywhereT f x = f (gmapT (everywhereT f) x)

--------------------------------------------------------------------------------
-- Intersection
--------------------------------------------------------------------------------

exampleSpec_intersectionWith_min_Sum_Natural :: Spec
exampleSpec_intersectionWith_min_Sum_Natural = unitTestSpec
    "MonoidMap.intersectionWith (Sum Natural)"
    "MonoidMap.intersectionWith"
    (MonoidMap.intersectionWith min)
    (exampleData_intersectionWith_min_Sum_Natural)

exampleData_intersectionWith_min_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
exampleData_intersectionWith_min_Sum_Natural = unitTestData2
    [ ( m [0, 1, 2, 3, 4, 5, 6, 7]
      , m [7, 6, 5, 4, 3, 2, 1, 0]
      , m [0, 1, 2, 3, 3, 2, 1, 0]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

--------------------------------------------------------------------------------
-- Union
--------------------------------------------------------------------------------

exampleSpec_unionWith_max_Sum_Natural :: Spec
exampleSpec_unionWith_max_Sum_Natural = unitTestSpec
    "MonoidMap.unionWith (Sum Natural)"
    "MonoidMap.unionWith"
    (MonoidMap.unionWith max)
    (exampleData_unionWith_max_Sum_Natural)

exampleData_unionWith_max_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
exampleData_unionWith_max_Sum_Natural = unitTestData2
    [ ( m [0, 1, 2, 3, 4, 5, 6, 7]
      , m [7, 6, 5, 4, 3, 2, 1, 0]
      , m [7, 6, 5, 4, 4, 5, 6, 7]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

--------------------------------------------------------------------------------
-- Semigroup
--------------------------------------------------------------------------------

exampleSpec_Semigroup_mappend_String :: Spec
exampleSpec_Semigroup_mappend_String = unitTestSpec
    "Semigroup.mappend (String)"
    "mappend"
    (mappend)
    (exampleData_Semigroup_concat_String)

exampleData_Semigroup_concat_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
exampleData_Semigroup_concat_String = unitTestData2
    [ ( m ["abc", "ij" , "p"  , ""   ]
      , m [   "",   "k",  "qr", "xyz"]
      , m ["abc", "ijk", "pqr", "xyz"]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_Semigroup_mappend_Sum_Natural :: Spec
exampleSpec_Semigroup_mappend_Sum_Natural = unitTestSpec
    "Semigroup.mappend (Sum Natural)"
    "mappend"
    (mappend)
    (exampleData_Semigroup_concat_Sum_Natural)

exampleData_Semigroup_concat_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
exampleData_Semigroup_concat_Sum_Natural = unitTestData2
    [ ( m [4, 2, 1, 0]
      , m [0, 1, 2, 4]
      , m [4, 3, 3, 4]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

--------------------------------------------------------------------------------
-- Group
--------------------------------------------------------------------------------

exampleSpec_Group_invert_Product_Rational :: Spec
exampleSpec_Group_invert_Product_Rational = unitTestSpec
    "Group.invert (Product Rational)"
    "invert"
    (invert)
    (exampleData_Group_invert_Product_Rational)

exampleData_Group_invert_Product_Rational :: UnitTestData1
    (MonoidMap LatinChar (Product Rational))
    (MonoidMap LatinChar (Product Rational))
exampleData_Group_invert_Product_Rational = unitTestData1
    [ ( m [  2,   4,   8,   16]
      , m [1%2, 1%4, 1%8, 1%16]
      )
    , ( m [1%2, 1%4, 1%8, 1%16]
      , m [  2,   4,   8,   16]
      )
    , ( m [  2, 1%4,   8,   16]
      , m [1%2,   4, 1%8, 1%16]
      )
    , ( m [1%2,   4, 1%8, 1%16]
      , m [  2, 1%4,   8,   16]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Product

exampleSpec_Group_invert_Sum_Integer :: Spec
exampleSpec_Group_invert_Sum_Integer = unitTestSpec
    "Group.invert (Sum Integer)"
    "invert"
    (invert)
    (exampleData_Group_invert_Sum_Integer)

exampleData_Group_invert_Sum_Integer :: UnitTestData1
    (MonoidMap LatinChar (Sum Integer))
    (MonoidMap LatinChar (Sum Integer))
exampleData_Group_invert_Sum_Integer = unitTestData1
    [ ( m [ 1,  2,  3,  4]
      , m [-1, -2, -3, -4]
      )
    , ( m [-1, -2, -3, -4]
      , m [ 1,  2,  3,  4]
      )
    , ( m [ 1, -2,  3, -4]
      , m [-1,  2, -3,  4]
      )
    , ( m [-1,  2, -3,  4]
      , m [ 1, -2,  3, -4]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Sum

exampleSpec_Group_pow_Product_Rational :: Spec
exampleSpec_Group_pow_Product_Rational = unitTestSpec
    "Group.pow (Product Rational)"
    "pow"
    (pow)
    (exampleData_Group_pow_Product_Rational)

exampleData_Group_pow_Product_Rational :: UnitTestData2
    (MonoidMap LatinChar (Product Rational))
    (Integer)
    (MonoidMap LatinChar (Product Rational))
exampleData_Group_pow_Product_Rational = unitTestData2
    [ ( m [  2,   -4,   8,   -16], (-1)
      , m [1%2, -1%4, 1%8, -1%16]
      )
    , ( m [  2,   -4,   8,   -16], 0
      , m [  1,    1,   1,     1]
      )
    , ( m [  2,   -4,   8,   -16], 1
      , m [  2,   -4,   8,   -16]
      )
    , ( m [  2,   -4,   8,   -16], 2
      , m [  4,   16,  64,   256]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Product

exampleSpec_Group_pow_Sum_Integer :: Spec
exampleSpec_Group_pow_Sum_Integer = unitTestSpec
    "Group.pow (Sum Integer)"
    "pow"
    (pow)
    (exampleData_Group_pow_Sum_Integer)

exampleData_Group_pow_Sum_Integer :: UnitTestData2
    (MonoidMap LatinChar (Sum Integer))
    (Integer)
    (MonoidMap LatinChar (Sum Integer))
exampleData_Group_pow_Sum_Integer = unitTestData2
    [ ( m [ 1, -2,  3, -4], (-1)
      , m [-1,  2, -3,  4]
      )
    , ( m [ 1, -2,  3, -4], 0
      , m [ 0,  0,  0,  0]
      )
    , ( m [ 1, -2,  3, -4], 1
      , m [ 1, -2,  3, -4]
      )
    , ( m [ 1, -2,  3, -4], 2
      , m [ 2, -4,  6, -8]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Sum

exampleSpec_Group_subtract_Product_Rational :: Spec
exampleSpec_Group_subtract_Product_Rational = unitTestSpec
    "Group.(~~) (Product Rational)"
    "(~~)"
    (~~)
    (exampleData_Group_subtract_Product_Rational)

exampleData_Group_subtract_Product_Rational :: UnitTestData2
    (MonoidMap LatinChar (Product Rational))
    (MonoidMap LatinChar (Product Rational))
    (MonoidMap LatinChar (Product Rational))
exampleData_Group_subtract_Product_Rational = unitTestData2
    [ ( m [ 1,    1,    1,    1]
      , m [ 1,    2,    4,    8]
      , m [ 1,  1%2,  1%4,  1%8]
      )
    , ( m [-1,   -1,   -1,   -1]
      , m [ 1,    2,    4,    8]
      , m [-1, -1%2, -1%4, -1%8]
      )
    , ( m [ 1,    1,    1,    1]
      , m [-1,   -2,   -4,   -8]
      , m [-1, -1%2, -1%4, -1%8]
      )
    , ( m [-1,   -1,   -1,   -1]
      , m [-1,   -2,   -4,   -8]
      , m [ 1,  1%2,  1%4,  1%8]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Product

exampleSpec_Group_subtract_Sum_Integer :: Spec
exampleSpec_Group_subtract_Sum_Integer = unitTestSpec
    "Group.(~~) (Sum Integer)"
    "(~~)"
    (~~)
    (exampleData_Group_subtract_Sum_Integer)

exampleData_Group_subtract_Sum_Integer :: UnitTestData2
    (MonoidMap LatinChar (Sum Integer))
    (MonoidMap LatinChar (Sum Integer))
    (MonoidMap LatinChar (Sum Integer))
exampleData_Group_subtract_Sum_Integer = unitTestData2
    [ ( m [ 1,  2,  3,  4]
      , m [ 1,  2,  3,  4]
      , m [ 0,  0,  0,  0]
      )
    , ( m [ 0,  0,  0,  0]
      , m [ 1,  2,  3,  4]
      , m [-1, -2, -3, -4]
      )
    , ( m [ 1,  2,  3,  4]
      , m [-1, -2, -3, -4]
      , m [ 2,  4,  6,  8]
      )
    , ( m [-1, -2, -3, -4]
      , m [-1, -2, -3, -4]
      , m [ 0,  0,  0,  0]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Sum

--------------------------------------------------------------------------------
-- Reductive
--------------------------------------------------------------------------------

exampleSpec_Reductive_isPrefixOf_String :: Spec
exampleSpec_Reductive_isPrefixOf_String = unitTestSpec
    "Reductive.isPrefixOf (String)"
    "isPrefixOf"
    (isPrefixOf)
    (exampleData_Reductive_isPrefixOf_String)

exampleData_Reductive_isPrefixOf_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (Bool)
exampleData_Reductive_isPrefixOf_String = unitTestData2
    [ ( m ["A"   , "B"   , "C"   ]
      , m ["A123", "B123", "C123"]
      , True
      )
    , ( m ["A123", "B123", "C123"]
      , m ["A"   , "B"   , "C"   ]
      , False
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_Reductive_isSuffixOf_String :: Spec
exampleSpec_Reductive_isSuffixOf_String = unitTestSpec
    "Reductive.isSuffixOf (String)"
    "isSuffixOf"
    (isSuffixOf)
    (exampleData_Reductive_isSuffixOf_String)

exampleData_Reductive_isSuffixOf_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (Bool)
exampleData_Reductive_isSuffixOf_String = unitTestData2
    [ ( m [   "A",    "B",    "C"]
      , m ["123A", "123B", "123C"]
      , True
      )
    , ( m ["123A", "123B", "123C"]
      , m [   "A",    "B",    "C"]
      , False
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_Reductive_isPrefixOf_Sum_Natural :: Spec
exampleSpec_Reductive_isPrefixOf_Sum_Natural = unitTestSpec
    "Reductive.isPrefixOf (Sum Natural)"
    "isPrefixOf"
    (isPrefixOf)
    (exampleData_Reductive_Sum_Natural)

exampleSpec_Reductive_isSuffixOf_Sum_Natural :: Spec
exampleSpec_Reductive_isSuffixOf_Sum_Natural = unitTestSpec
    "Reductive.isSuffixOf (Sum Natural)"
    "isSuffixOf"
    (isSuffixOf)
    (exampleData_Reductive_Sum_Natural)

exampleData_Reductive_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (Bool)
exampleData_Reductive_Sum_Natural = unitTestData2
    [ ( m [1, 1], m [1, 1], True )
    , ( m [1, 1], m [1, 2], True )
    , ( m [1, 1], m [2, 1], True )
    , ( m [1, 1], m [2, 2], True )
    , ( m [1, 2], m [1, 1], False)
    , ( m [1, 2], m [1, 2], True )
    , ( m [1, 2], m [2, 1], False)
    , ( m [1, 2], m [2, 2], True )
    , ( m [2, 1], m [1, 1], False)
    , ( m [2, 1], m [1, 2], False)
    , ( m [2, 1], m [2, 1], True )
    , ( m [2, 1], m [2, 2], True )
    , ( m [2, 2], m [1, 1], False)
    , ( m [2, 2], m [1, 2], False)
    , ( m [2, 2], m [2, 1], False)
    , ( m [2, 2], m [2, 2], True )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Sum

exampleSpec_Reductive_stripPrefix_String :: Spec
exampleSpec_Reductive_stripPrefix_String = unitTestSpec
    "Reductive.stripPrefix (String)"
    "stripPrefix"
    (stripPrefix)
    (exampleData_Reductive_stripPrefix_String)

exampleData_Reductive_stripPrefix_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (Maybe (MonoidMap LatinChar String))
exampleData_Reductive_stripPrefix_String = unitTestData2
    [ ( m [""   , ""   , ""   ]
      , m ["abc", "pqr", "xyz"]
      , m ["abc", "pqr", "xyz"] & Just
      )
    , ( m ["a"  , "p"  , "x"  ]
      , m ["abc", "pqr", "xyz"]
      , m [ "bc",  "qr",  "yz"] & Just
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["abc", "pqr", "xyz"]
      , m [   "",    "",    ""] & Just
      )
    , ( m ["?"  , "p"  , "x"  ]
      , m ["abc", "pqr", "xyz"]
      , Nothing
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_Reductive_stripPrefix_Sum_Natural :: Spec
exampleSpec_Reductive_stripPrefix_Sum_Natural = unitTestSpec
    "Reductive.stripPrefix (Sum Natural)"
    "stripPrefix"
    (stripPrefix)
    (exampleData_Reductive_stripPrefix_Sum_Natural)

exampleData_Reductive_stripPrefix_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (Maybe (MonoidMap LatinChar (Sum Natural)))
exampleData_Reductive_stripPrefix_Sum_Natural = unitTestData2
    [ ( m [0, 0, 0]
      , m [2, 4, 8]
      , m [2, 4, 8] & Just
      )
    , ( m [1, 2, 4]
      , m [2, 4, 8]
      , m [1, 2, 4] & Just
      )
    , ( m [2, 4, 8]
      , m [2, 4, 8]
      , m [0, 0, 0] & Just
      )
    , ( m [3, 4, 8]
      , m [2, 4, 8]
      , Nothing
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_Reductive_stripSuffix_String :: Spec
exampleSpec_Reductive_stripSuffix_String = unitTestSpec
    "Reductive.stripSuffix (String)"
    "stripSuffix"
    (stripSuffix)
    (exampleData_Reductive_stripSuffix_String)

exampleData_Reductive_stripSuffix_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (Maybe (MonoidMap LatinChar String))
exampleData_Reductive_stripSuffix_String = unitTestData2
    [ ( m [   "",    "",    ""]
      , m ["abc", "pqr", "xyz"]
      , m ["abc", "pqr", "xyz"] & Just
      )
    , ( m [  "c",   "r",   "z"]
      , m ["abc", "pqr", "xyz"]
      , m ["ab" , "pq" , "xy" ] & Just
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["abc", "pqr", "xyz"]
      , m [""   , ""   , ""   ] & Just
      )
    , ( m [  "?",   "r",   "z"]
      , m ["abc", "pqr", "xyz"]
      , Nothing
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_Reductive_stripSuffix_Sum_Natural :: Spec
exampleSpec_Reductive_stripSuffix_Sum_Natural = unitTestSpec
    "Reductive.stripSuffix (Sum Natural)"
    "stripSuffix"
    (stripSuffix)
    (exampleData_Reductive_stripSuffix_Sum_Natural)

exampleData_Reductive_stripSuffix_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (Maybe (MonoidMap LatinChar (Sum Natural)))
exampleData_Reductive_stripSuffix_Sum_Natural = unitTestData2
    [ ( m [0, 0, 0]
      , m [2, 4, 8]
      , m [2, 4, 8] & Just
      )
    , ( m [1, 2, 4]
      , m [2, 4, 8]
      , m [1, 2, 4] & Just
      )
    , ( m [2, 4, 8]
      , m [2, 4, 8]
      , m [0, 0, 0] & Just
      )
    , ( m [3, 4, 8]
      , m [2, 4, 8]
      , Nothing
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

--------------------------------------------------------------------------------
-- LeftGCDMonoid
--------------------------------------------------------------------------------

exampleSpec_LeftGCDMonoid_commonPrefix_String :: Spec
exampleSpec_LeftGCDMonoid_commonPrefix_String = unitTestSpec
    "LeftGCDMonoid.commonPrefix (String)"
    "commonPrefix"
    (commonPrefix)
    (exampleData_LeftGCDMonoid_commonPrefix_String)

exampleData_LeftGCDMonoid_commonPrefix_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
exampleData_LeftGCDMonoid_commonPrefix_String = unitTestData2
    [ ( m ["---", "---", "---"]
      , m ["abc", "pqr", "xyz"]
      , m [""   , ""   , ""   ]
      )
    , ( m ["a--", "p--", "x--"]
      , m ["abc", "pqr", "xyz"]
      , m ["a"  , "p"  , "x"  ]
      )
    , ( m ["ab-", "pq-", "xy-"]
      , m ["abc", "pqr", "xyz"]
      , m ["ab" , "pq" , "xy" ]
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["abc", "pqr", "xyz"]
      , m ["abc", "pqr", "xyz"]
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["ab-", "pq-", "xy-"]
      , m ["ab" , "pq" , "xy" ]
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["a--", "p--", "x--"]
      , m ["a"  , "p"  , "x"  ]
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["---", "---", "---"]
      , m [""   , ""   , ""   ]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_LeftGCDMonoid_commonPrefix_Sum_Natural :: Spec
exampleSpec_LeftGCDMonoid_commonPrefix_Sum_Natural = unitTestSpec
    "LeftGCDMonoid.commonPrefix (Sum Natural)"
    "commonPrefix"
    (commonPrefix)
    (exampleData_LeftGCDMonoid_commonPrefix_Sum_Natural)

exampleData_LeftGCDMonoid_commonPrefix_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
exampleData_LeftGCDMonoid_commonPrefix_Sum_Natural = unitTestData2
    [ ( m [0, 0, 0]
      , m [1, 2, 3]
      , m [0, 0, 0]
      )
    , ( m [1, 1, 1]
      , m [1, 2, 3]
      , m [1, 1, 1]
      )
    , ( m [2, 2, 2]
      , m [1, 2, 3]
      , m [1, 2, 2]
      )
    , ( m [3, 3, 3]
      , m [1, 2, 3]
      , m [1, 2, 3]
      )
    , ( m [4, 4, 4]
      , m [1, 2, 3]
      , m [1, 2, 3]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_LeftGCDMonoid_stripCommonPrefix_String :: Spec
exampleSpec_LeftGCDMonoid_stripCommonPrefix_String = unitTestSpec
    "LeftGCDMonoid.stripCommonPrefix (String)"
    "stripCommonPrefix"
    (stripCommonPrefix)
    (exampleData_LeftGCDMonoid_stripCommonPrefix_String)

exampleData_LeftGCDMonoid_stripCommonPrefix_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    ( MonoidMap LatinChar String
    , MonoidMap LatinChar String
    , MonoidMap LatinChar String
    )
exampleData_LeftGCDMonoid_stripCommonPrefix_String = unitTestData2
    [ (   m ["---", "---", "---"]
      ,   m ["abc", "pqr", "xyz"]

      , ( m [""   , ""   , ""   ]
        , m ["---", "---", "---"]
        , m ["abc", "pqr", "xyz"]
        )
      )
    , (   m ["a--", "p--", "x--"]
      ,   m ["abc", "pqr", "xyz"]

      , ( m ["a"  , "p"  , "x"  ]
        , m [ "--",  "--",  "--"]
        , m [ "bc",  "qr",  "yz"]
        )
      )
    , (   m ["ab-", "pq-", "xy-"]
      ,   m ["abc", "pqr", "xyz"]

      , ( m ["ab" , "pq" , "xy" ]
        , m [  "-",   "-",   "-"]
        , m [  "c",   "r",   "z"]
        )
      )
    , (   m ["abc", "pqr", "xyz"]
      ,   m ["abc", "pqr", "xyz"]

      , ( m ["abc", "pqr", "xyz"]
        , m [   "",    "",    ""]
        , m [   "",    "",    ""]
        )
      )
    , (   m ["abc", "pqr", "xyz"]
      ,   m ["ab-", "pq-", "xy-"]

      , ( m ["ab" , "pq" , "xy" ]
        , m [  "c",   "r",   "z"]
        , m [  "-",   "-",   "-"]
        )
      )
    , (   m ["abc", "pqr", "xyz"]
      ,   m ["a--", "p--", "x--"]
      , ( m ["a"  , "p"  , "x"  ]
        , m [ "bc",  "qr",  "yz"]
        , m [ "--",  "--",  "--"]
        )
      )
    , (   m ["abc", "pqr", "xyz"]
      ,   m ["---", "---", "---"]
      , ( m [""   , ""   , ""   ]
        , m ["abc", "pqr", "xyz"]
        , m ["---", "---", "---"]
        )
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_LeftGCDMonoid_stripCommonPrefix_Sum_Natural :: Spec
exampleSpec_LeftGCDMonoid_stripCommonPrefix_Sum_Natural = unitTestSpec
    "LeftGCDMonoid.stripCommonPrefix (Sum Natural)"
    "stripCommonPrefix"
    (stripCommonPrefix)
    (exampleData_LeftGCDMonoid_stripCommonPrefix_Sum_Natural)

exampleData_LeftGCDMonoid_stripCommonPrefix_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    ( MonoidMap LatinChar (Sum Natural)
    , MonoidMap LatinChar (Sum Natural)
    , MonoidMap LatinChar (Sum Natural)
    )
exampleData_LeftGCDMonoid_stripCommonPrefix_Sum_Natural = unitTestData2
    [ (   m [0, 1, 2, 3, 4]
      ,   m [4, 3, 2, 1, 0]

      , ( m [0, 1, 2, 1, 0]
        , m [0, 0, 0, 2, 4]
        , m [4, 2, 0, 0, 0]
        )
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

--------------------------------------------------------------------------------
-- RightGCDMonoid
--------------------------------------------------------------------------------

exampleSpec_RightGCDMonoid_commonSuffix_String :: Spec
exampleSpec_RightGCDMonoid_commonSuffix_String = unitTestSpec
    "RightGCDMonoid.commonSuffix (String)"
    "commonSuffix"
    (commonSuffix)
    (exampleData_RightGCDMonoid_commonSuffix_String)

exampleData_RightGCDMonoid_commonSuffix_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
exampleData_RightGCDMonoid_commonSuffix_String = unitTestData2
    [ ( m ["---", "---", "---"]
      , m ["abc", "pqr", "xyz"]
      , m [  "" ,    "",    ""]
      )
    , ( m ["--c", "--r", "--z"]
      , m ["abc", "pqr", "xyz"]
      , m [  "c",   "r",   "z"]
      )
    , ( m ["-bc", "-qr", "-yz"]
      , m ["abc", "pqr", "xyz"]
      , m [ "bc",  "qr",  "yz"]
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["abc", "pqr", "xyz"]
      , m ["abc", "pqr", "xyz"]
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["-bc", "-qr", "-yz"]
      , m [ "bc",  "qr",  "yz"]
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["--c", "--r", "--z"]
      , m [  "c",   "r",   "z"]
      )
    , ( m ["abc", "pqr", "xyz"]
      , m ["---", "---", "---"]
      , m [   "",    "",    ""]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_RightGCDMonoid_commonSuffix_Sum_Natural :: Spec
exampleSpec_RightGCDMonoid_commonSuffix_Sum_Natural = unitTestSpec
    "RightGCDMonoid.commonSuffix (Sum Natural)"
    "commonSuffix"
    (commonSuffix)
    (exampleData_RightGCDMonoid_commonSuffix_Sum_Natural)

exampleData_RightGCDMonoid_commonSuffix_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
exampleData_RightGCDMonoid_commonSuffix_Sum_Natural = unitTestData2
    [ ( m [0, 0, 0]
      , m [1, 2, 3]
      , m [0, 0, 0]
      )
    , ( m [1, 1, 1]
      , m [1, 2, 3]
      , m [1, 1, 1]
      )
    , ( m [2, 2, 2]
      , m [1, 2, 3]
      , m [1, 2, 2]
      )
    , ( m [3, 3, 3]
      , m [1, 2, 3]
      , m [1, 2, 3]
      )
    , ( m [4, 4, 4]
      , m [1, 2, 3]
      , m [1, 2, 3]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_RightGCDMonoid_stripCommonSuffix_String :: Spec
exampleSpec_RightGCDMonoid_stripCommonSuffix_String = unitTestSpec
    "RightGCDMonoid.stripCommonSuffix (String)"
    "stripCommonSuffix"
    (stripCommonSuffix)
    (exampleData_RightGCDMonoid_stripCommonSuffix_String)

exampleData_RightGCDMonoid_stripCommonSuffix_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    ( MonoidMap LatinChar String
    , MonoidMap LatinChar String
    , MonoidMap LatinChar String
    )
exampleData_RightGCDMonoid_stripCommonSuffix_String = unitTestData2
    [ (   m ["---", "---", "---"]
      ,   m ["abc", "pqr", "xyz"]

      , ( m ["---", "---", "---"]
        , m ["abc", "pqr", "xyz"]
        , m [   "",    "",    ""]
        )
      )
    , (   m ["--c", "--r", "--z"]
      ,   m ["abc", "pqr", "xyz"]

      , ( m ["--" , "--" , "--" ]
        , m ["ab" , "pq" , "xy" ]
        , m [  "c",   "r",   "z"]
        )
      )
    , (   m ["--c", "--r", "--z"]
      ,   m ["abc", "pqr", "xyz"]

      , ( m ["--" , "--" , "--" ]
        , m ["ab" , "pq" , "xy" ]
        , m [  "c",   "r",   "z"]
        )
      )
    , (   m ["-bc", "-qr", "-yz"]
      ,   m ["abc", "pqr", "xyz"]

      , ( m ["-"  , "-"  , "-"  ]
        , m ["a"  , "p"  , "x"  ]
        , m [ "bc",  "qr",  "yz"]
        )
      )
    , (   m ["abc", "pqr", "xyz"]
      ,   m ["abc", "pqr", "xyz"]

      , ( m [""   , ""   , ""   ]
        , m [""   , ""   , ""   ]
        , m ["abc", "pqr", "xyz"]
        )
      )
    , (   m ["abc", "pqr", "xyz"]
      ,   m ["-bc", "-qr", "-yz"]

      , ( m ["a"  , "p"  , "x"  ]
        , m ["-"  , "-"  , "-"  ]
        , m [ "bc",  "qr",  "yz"]
        )
      )
    , (   m ["abc", "pqr", "xyz"]
      ,   m ["--c", "--r", "--z"]

      , ( m ["ab" , "pq" , "xy" ]
        , m ["--" , "--" , "--" ]
        , m [  "c",   "r",   "z"]
        )
      )
    , (   m ["abc", "pqr", "xyz"]
      ,   m ["---", "---", "---"]

      , ( m ["abc", "pqr", "xyz"]
        , m ["---", "---", "---"]
        , m [   "",    "",    ""]
        )
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_RightGCDMonoid_stripCommonSuffix_Sum_Natural :: Spec
exampleSpec_RightGCDMonoid_stripCommonSuffix_Sum_Natural = unitTestSpec
    "RightGCDMonoid.stripCommonSuffix (Sum Natural)"
    "stripCommonSuffix"
    (stripCommonSuffix)
    (exampleData_RightGCDMonoid_stripCommonSuffix_Sum_Natural)

exampleData_RightGCDMonoid_stripCommonSuffix_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    ( MonoidMap LatinChar (Sum Natural)
    , MonoidMap LatinChar (Sum Natural)
    , MonoidMap LatinChar (Sum Natural)
    )
exampleData_RightGCDMonoid_stripCommonSuffix_Sum_Natural = unitTestData2
    [ (   m [0, 1, 2, 3, 4]
      ,   m [4, 3, 2, 1, 0]

      , ( m [0, 0, 0, 2, 4]
        , m [4, 2, 0, 0, 0]
        , m [0, 1, 2, 1, 0]
        )
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

--------------------------------------------------------------------------------
-- OverlappingGCDMonoid
--------------------------------------------------------------------------------

exampleSpec_OverlappingGCDMonoid_overlap_String :: Spec
exampleSpec_OverlappingGCDMonoid_overlap_String = unitTestSpec
    "OverlappingGCDMonoid.overlap (String)"
    "overlap"
    (overlap)
    (exampleData_OverlappingGCDMonoid_overlap_String)

exampleData_OverlappingGCDMonoid_overlap_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
exampleData_OverlappingGCDMonoid_overlap_String = unitTestData2
    [ ( m ["abcd"    , "0123"    ]
      , m [    "efgh",     "4567"]
      , m [    ""    ,     ""    ]
      )
    , ( m ["abcde"   , "01234"   ]
      , m [   "defgh",    "34567"]
      , m [   "de"   ,    "34"   ]
      )
    , ( m ["abcdef"  , "012345"  ]
      , m [  "cdefgh",   "234567"]
      , m [  "cdef"  ,   "2345"  ]
      )
    , ( m ["abcdefg" , "0123456" ]
      , m [ "bcdefgh",  "1234567"]
      , m [ "bcdefg" ,  "123456" ]
      )
    , ( m ["abcdefgh", "01234567"]
      , m ["abcdefgh", "01234567"]
      , m ["abcdefgh", "01234567"]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_OverlappingGCDMonoid_overlap_Sum_Natural :: Spec
exampleSpec_OverlappingGCDMonoid_overlap_Sum_Natural = unitTestSpec
    "OverlappingGCDMonoid.overlap (Sum Natural)"
    "overlap"
    (overlap)
    (exampleData_OverlappingGCDMonoid_overlap_Sum_Natural)

exampleData_OverlappingGCDMonoid_overlap_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
exampleData_OverlappingGCDMonoid_overlap_Sum_Natural = unitTestData2
    [ ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
      , m [0, 1, 2, 3, 4, 4, 3, 2, 1, 0]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_OverlappingGCDMonoid_stripPrefixOverlap_String :: Spec
exampleSpec_OverlappingGCDMonoid_stripPrefixOverlap_String = unitTestSpec
    "OverlappingGCDMonoid.stripPrefixOverlap (String)"
    "stripPrefixOverlap"
    (stripPrefixOverlap)
    (exampleData_OverlappingGCDMonoid_stripPrefixOverlap_String)

exampleData_OverlappingGCDMonoid_stripPrefixOverlap_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
exampleData_OverlappingGCDMonoid_stripPrefixOverlap_String = unitTestData2
    [ ( m ["abcd"    , "0123"    ]
      , m [    "efgh",     "4567"]
      , m [    "efgh",     "4567"]
      )
    , ( m ["abcde"   , "01234"   ]
      , m [   "defgh",    "34567"]
      , m [     "fgh",      "567"]
      )
    , ( m ["abcdef"  , "012345"  ]
      , m [  "cdefgh",   "234567"]
      , m [      "gh",       "67"]
      )
    , ( m ["abcdefg" , "0123456" ]
      , m [ "bcdefgh",  "1234567"]
      , m [       "h",        "7"]
      )
    , ( m ["abcdefgh", "01234567"]
      , m ["abcdefgh", "01234567"]
      , m [        "",         ""]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_OverlappingGCDMonoid_stripSuffixOverlap_String :: Spec
exampleSpec_OverlappingGCDMonoid_stripSuffixOverlap_String = unitTestSpec
    "OverlappingGCDMonoid.stripSuffixOverlap (String)"
    "stripSuffixOverlap"
    (stripSuffixOverlap)
    (exampleData_OverlappingGCDMonoid_stripSuffixOverlap_String)

exampleData_OverlappingGCDMonoid_stripSuffixOverlap_String :: UnitTestData2
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
    (MonoidMap LatinChar String)
exampleData_OverlappingGCDMonoid_stripSuffixOverlap_String = unitTestData2
    [ ( m [    "efgh",     "4567"]
      , m ["abcd"    , "0123"    ]
      , m ["abcd"    , "0123"    ]
      )
    , ( m [   "defgh",    "34567"]
      , m ["abcde"   , "01234"   ]
      , m ["abc"     , "012"     ]
      )
    , ( m [  "cdefgh",   "234567"]
      , m ["abcdef"  , "012345"  ]
      , m ["ab"      , "01"      ]
      )
    , ( m [ "bcdefgh",  "1234567"]
      , m ["abcdefg" , "0123456" ]
      , m ["a"       , "0"       ]
      )
    , ( m ["abcdefgh", "01234567"]
      , m ["abcdefgh", "01234567"]
      , m [""        , ""        ]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_OverlappingGCDMonoid_stripPrefixOverlap_Sum_Natural :: Spec
exampleSpec_OverlappingGCDMonoid_stripPrefixOverlap_Sum_Natural = unitTestSpec
    "OverlappingGCDMonoid.stripPrefixOverlap (Sum Natural)"
    "stripPrefixOverlap"
    (stripPrefixOverlap)
    (exampleData_OverlappingGCDMonoid_stripPrefixOverlap_Sum_Natural)

exampleData_OverlappingGCDMonoid_stripPrefixOverlap_Sum_Natural
    :: UnitTestData2
        (MonoidMap LatinChar (Sum Natural))
        (MonoidMap LatinChar (Sum Natural))
        (MonoidMap LatinChar (Sum Natural))
exampleData_OverlappingGCDMonoid_stripPrefixOverlap_Sum_Natural = unitTestData2
    [ ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
      , m [9, 7, 5, 3, 1, 0, 0, 0, 0, 0]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_OverlappingGCDMonoid_stripSuffixOverlap_Sum_Natural :: Spec
exampleSpec_OverlappingGCDMonoid_stripSuffixOverlap_Sum_Natural = unitTestSpec
    "OverlappingGCDMonoid.stripSuffixOverlap (Sum Natural)"
    "stripSuffixOverlap"
    (stripSuffixOverlap)
    (exampleData_OverlappingGCDMonoid_stripSuffixOverlap_Sum_Natural)

exampleData_OverlappingGCDMonoid_stripSuffixOverlap_Sum_Natural
    :: UnitTestData2
        (MonoidMap LatinChar (Sum Natural))
        (MonoidMap LatinChar (Sum Natural))
        (MonoidMap LatinChar (Sum Natural))
exampleData_OverlappingGCDMonoid_stripSuffixOverlap_Sum_Natural = unitTestData2
    [ ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
      , m [9, 7, 5, 3, 1, 0, 0, 0, 0, 0]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

--------------------------------------------------------------------------------
-- GCDMonoid
--------------------------------------------------------------------------------

exampleSpec_GCDMonoid_gcd_Product_Natural :: Spec
exampleSpec_GCDMonoid_gcd_Product_Natural = unitTestSpec
    "GCDMonoid.gcd (Product Natural)"
    "gcd"
    (gcd)
    (exampleData_GCDMonoid_gcd_Product_Natural)

exampleData_GCDMonoid_gcd_Product_Natural :: UnitTestData2
    (MonoidMap LatinChar (Product Natural))
    (MonoidMap LatinChar (Product Natural))
    (MonoidMap LatinChar (Product Natural))
exampleData_GCDMonoid_gcd_Product_Natural = unitTestData2
    [ ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      )
    , ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
      , m [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
      )
    , ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
      , m [2, 1, 2, 1, 2, 1, 2, 1, 2, 1]
      )
    , ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [3, 3, 3, 3, 3, 3, 3, 3, 3, 3]
      , m [3, 1, 1, 3, 1, 1, 3, 1, 1, 3]
      )
    , ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
      , m [4, 1, 2, 1, 4, 1, 2, 1, 4, 1]
      )
    , ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
      , m [5, 1, 1, 1, 1, 5, 1, 1, 1, 1]
      )
    , ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [6, 6, 6, 6, 6, 6, 6, 6, 6, 6]
      , m [6, 1, 2, 3, 2, 1, 6, 1, 2, 3]
      )
    , ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [7, 7, 7, 7, 7, 7, 7, 7, 7, 7]
      , m [7, 1, 1, 1, 1, 1, 1, 7, 1, 1]
      )
    , ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [8, 8, 8, 8, 8, 8, 8, 8, 8, 8]
      , m [8, 1, 2, 1, 4, 1, 2, 1, 8, 1]
      )
    , ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [9, 9, 9, 9, 9, 9, 9, 9, 9, 9]
      , m [9, 1, 1, 3, 1, 1, 3, 1, 1, 9]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_GCDMonoid_gcd_Sum_Natural :: Spec
exampleSpec_GCDMonoid_gcd_Sum_Natural = unitTestSpec
    "GCDMonoid.gcd (Sum Natural)"
    "gcd"
    (gcd)
    (exampleData_GCDMonoid_gcd_Sum_Natural)

exampleData_GCDMonoid_gcd_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
exampleData_GCDMonoid_gcd_Sum_Natural = unitTestData2
    [ ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
      , m [0, 1, 2, 3, 4, 4, 3, 2, 1, 0]
      )
    , ( m [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
      , m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [0, 1, 2, 3, 4, 4, 3, 2, 1, 0]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_GCDMonoid_gcd_Set_Natural :: Spec
exampleSpec_GCDMonoid_gcd_Set_Natural = unitTestSpec
    "GCDMonoid.gcd (Set Natural)"
    "gcd"
    (gcd)
    (exampleData_GCDMonoid_gcd_Set_Natural)

exampleData_GCDMonoid_gcd_Set_Natural :: UnitTestData2
    (MonoidMap LatinChar (Set Natural))
    (MonoidMap LatinChar (Set Natural))
    (MonoidMap LatinChar (Set Natural))
exampleData_GCDMonoid_gcd_Set_Natural = unitTestData2
    [ ( m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    , ( m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[          ], [          ]]
      , m [[          ], [          ]]
      )
    , ( m [[          ], [          ]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[          ], [          ]]
      )
    , ( m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[   1, 2, 3], [   5, 6, 7]]
      , m [[   1, 2, 3], [   5, 6, 7]]
      )
    , ( m [[   1, 2, 3], [   5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[   1, 2, 3], [   5, 6, 7]]
      )
    , ( m [[0, 1, 2   ], [4, 5, 6   ]]
      , m [[   1, 2, 3], [   5, 6, 7]]
      , m [[   1, 2   ], [   5, 6   ]]
      )
    , ( m [[   1, 2, 3], [   5, 6, 7]]
      , m [[0, 1, 2   ], [4, 5, 6   ]]
      , m [[   1, 2   ], [   5, 6   ]]
      )
    , ( m [[0, 1      ], [4, 5      ]]
      , m [[      2, 3], [      6, 7]]
      , m [[          ], [          ]]
      )
    , ( m [[      2, 3], [      6, 7]]
      , m [[0, 1      ], [4, 5      ]]
      , m [[          ], [          ]]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Set.fromList

--------------------------------------------------------------------------------
-- LCMMonoid
--------------------------------------------------------------------------------

exampleSpec_LCMMonoid_lcm_Product_Natural :: Spec
exampleSpec_LCMMonoid_lcm_Product_Natural = unitTestSpec
    "LCMMonoid.lcm (Product Natural)"
    "lcm"
    (lcm)
    (exampleData_LCMMonoid_lcm_Product_Natural)

exampleData_LCMMonoid_lcm_Product_Natural :: UnitTestData2
    (MonoidMap LatinChar (Product Natural))
    (MonoidMap LatinChar (Product Natural))
    (MonoidMap LatinChar (Product Natural))
exampleData_LCMMonoid_lcm_Product_Natural = unitTestData2
    [ ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
      , m [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
      )
    , ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1]
      , m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      )
    , ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 2,  2,  2,  2,  2,  2,  2,  2,  2,  2]
      , m [ 0,  2,  2,  6,  4, 10,  6, 14,  8, 18]
      )
    , ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 3,  3,  3,  3,  3,  3,  3,  3,  3,  3]
      , m [ 0,  3,  6,  3, 12, 15,  6, 21, 24,  9]
      )
    , ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 4,  4,  4,  4,  4,  4,  4,  4,  4,  4]
      , m [ 0,  4,  4, 12,  4, 20, 12, 28,  8, 36]
      )
    , ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 5,  5,  5,  5,  5,  5,  5,  5,  5,  5]
      , m [ 0,  5, 10, 15, 20,  5, 30, 35, 40, 45]
      )
    , ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 6,  6,  6,  6,  6,  6,  6,  6,  6,  6]
      , m [ 0,  6,  6,  6, 12, 30,  6, 42, 24, 18]
      )
    , ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 7,  7,  7,  7,  7,  7,  7,  7,  7,  7]
      , m [ 0,  7, 14, 21, 28, 35, 42,  7, 56, 63]
      )
    , ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 8,  8,  8,  8,  8,  8,  8,  8,  8,  8]
      , m [ 0,  8,  8, 24,  8, 40, 24, 56,  8, 72]
      )
    , ( m [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
      , m [ 9,  9,  9,  9,  9,  9,  9,  9,  9,  9]
      , m [ 0,  9, 18,  9, 36, 45, 18, 63, 72,  9]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_LCMMonoid_lcm_Sum_Natural :: Spec
exampleSpec_LCMMonoid_lcm_Sum_Natural = unitTestSpec
    "LCMMonoid.lcm (Sum Natural)"
    "lcm"
    (lcm)
    (exampleData_LCMMonoid_lcm_Sum_Natural)

exampleData_LCMMonoid_lcm_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
exampleData_LCMMonoid_lcm_Sum_Natural = unitTestData2
    [ ( m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
      , m [9, 8, 7, 6, 5, 5, 6, 7, 8, 9]
      )
    , ( m [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
      , m [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      , m [9, 8, 7, 6, 5, 5, 6, 7, 8, 9]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

exampleSpec_LCMMonoid_lcm_Set_Natural :: Spec
exampleSpec_LCMMonoid_lcm_Set_Natural = unitTestSpec
    "LCMMonoid.lcm (Set Natural)"
    "lcm"
    (lcm)
    (exampleData_LCMMonoid_lcm_Set_Natural)

exampleData_LCMMonoid_lcm_Set_Natural :: UnitTestData2
    (MonoidMap LatinChar (Set Natural))
    (MonoidMap LatinChar (Set Natural))
    (MonoidMap LatinChar (Set Natural))
exampleData_LCMMonoid_lcm_Set_Natural = unitTestData2
    [ ( m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    , ( m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[          ], [          ]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    , ( m [[          ], [          ]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    , ( m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[   1, 2, 3], [   5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    , ( m [[   1, 2, 3], [   5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    , ( m [[0, 1, 2   ], [4, 5, 6   ]]
      , m [[   1, 2, 3], [   5, 6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    , ( m [[   1, 2, 3], [   5, 6, 7]]
      , m [[0, 1, 2   ], [4, 5, 6   ]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    , ( m [[0, 1      ], [4, 5      ]]
      , m [[      2, 3], [      6, 7]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    , ( m [[      2, 3], [      6, 7]]
      , m [[0, 1      ], [4, 5      ]]
      , m [[0, 1, 2, 3], [4, 5, 6, 7]]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Set.fromList

--------------------------------------------------------------------------------
-- Monus
--------------------------------------------------------------------------------

exampleSpec_Monus_monus_Set_Natural :: Spec
exampleSpec_Monus_monus_Set_Natural = unitTestSpec
    "Monus.monus (Set Natural)"
    "<\\>"
    (<\>)
    (exampleData_Monus_monus_Set_Natural)

exampleData_Monus_monus_Set_Natural :: UnitTestData2
    (MonoidMap LatinChar (Set Natural))
    (MonoidMap LatinChar (Set Natural))
    (MonoidMap LatinChar (Set Natural))
exampleData_Monus_monus_Set_Natural = unitTestData2
    [ ( m [[0, 1, 2], [3, 4, 5]]
      , m [[       ], [       ]]
      , m [[0, 1, 2], [3, 4, 5]]
      )
    , ( m [[0, 1, 2], [3, 4, 5]]
      , m [[0      ], [3      ]]
      , m [[   1, 2], [   4, 5]]
      )
    , ( m [[0, 1, 2], [3, 4, 5]]
      , m [[   1   ], [   4   ]]
      , m [[0,    2], [3,    5]]
      )
    , ( m [[0, 1, 2], [3, 4, 5]]
      , m [[      2], [      5]]
      , m [[0, 1   ], [3, 4   ]]
      )
    , ( m [[0, 1, 2], [3, 4, 5]]
      , m [[0, 1, 2], [3, 4, 5]]
      , m [[       ], [       ]]
      )
    , ( m [[0, 1, 2], [3, 4, 5]]
      , m [[3, 4, 5], [0, 1, 2]]
      , m [[0, 1, 2], [3, 4, 5]]
      )
    , ( m [[0, 1, 2], [3, 4, 5]]
      , m [[2, 3, 4], [1, 2, 3]]
      , m [[0, 1   ], [   4, 5]]
      )
    , ( m [[0, 1, 2], [3, 4, 5]]
      , m [[1, 2, 3], [2, 3, 4]]
      , m [[0      ], [      5]]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..] . fmap Set.fromList

exampleSpec_Monus_monus_Sum_Natural :: Spec
exampleSpec_Monus_monus_Sum_Natural = unitTestSpec
    "Monus.monus (Sum Natural)"
    "<\\>"
    (<\>)
    (exampleData_Monus_monus_Sum_Natural)

exampleData_Monus_monus_Sum_Natural :: UnitTestData2
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
    (MonoidMap LatinChar (Sum Natural))
exampleData_Monus_monus_Sum_Natural = unitTestData2
    [ ( m [0, 1, 2, 3]
      , m [0, 0, 0, 0]
      , m [0, 1, 2, 3]
      )
    , ( m [0, 1, 2, 3]
      , m [1, 1, 1, 1]
      , m [0, 0, 1, 2]
      )
    , ( m [0, 1, 2, 3]
      , m [2, 2, 2, 2]
      , m [0, 0, 0, 1]
      )
    , ( m [0, 1, 2, 3]
      , m [3, 3, 3, 3]
      , m [0, 0, 0, 0]
      )
    , ( m [0, 1, 2, 3]
      , m [4, 4, 4, 4]
      , m [0, 0, 0, 0]
      )
    ]
  where
    m = MonoidMap.fromList . zip [A ..]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

data LatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Show, Data)
