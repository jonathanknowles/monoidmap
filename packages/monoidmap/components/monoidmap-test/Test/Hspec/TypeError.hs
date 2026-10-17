{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
-- Provides expectations about ill-typed expressions.
--
module Test.Hspec.TypeError
    ( shouldFailToTypecheckWith
    ) where

import Prelude

import Control.DeepSeq
    ( NFData, force )
import Control.Exception
    ( TypeError (TypeError), evaluate, throwIO, try )
import Control.Monad
    ( forM_, unless )
import Data.List
    ( isInfixOf, isSuffixOf )
import Test.Hspec
    ( Expectation, expectationFailure )

-- | Expects the given expression to be ill-typed, with a type error message
--   that contains all of the given string fragments.
--
-- The module that calls this function must be compiled with the
-- @-fdefer-type-errors@ option, so that type errors are deferred until
-- runtime, at which point they are thrown as 'TypeError' exceptions.
--
-- Deferred type error messages are not stable: their layout, wording and
-- quotation marks vary between GHC versions and locales. Therefore, each
-- fragment should be as small as possible, and should not contain quotation
-- marks or line breaks.
--
-- Notes on the type signature:
--
-- - The 'NFData' constraint allows the expression to be fully evaluated,
--   which is necessary to trigger a deferred type error.
--
-- - The @() ~ ()@ constraint delays the deferred type error until the
--   expression is evaluated by this function. Without it, GHC would
--   throw the deferred type error as soon as the enclosing definition
--   was evaluated, before this function had a chance to catch it.
--
shouldFailToTypecheckWith
    :: NFData a
    => (() ~ () => a)
    -- ^ An expression that should fail to typecheck.
    -> [String]
    -- ^ String fragments that the type error message should contain.
    -> Expectation
shouldFailToTypecheckWith a fragments =
    try (evaluate (force a)) >>= \case
        Right _ ->
            expectationFailure
                "Expected a type error, but the expression was well-typed."
        Left e@(TypeError message)
            | not ("(deferred type error)" `isSuffixOf` message) ->
                throwIO e
            | otherwise ->
                forM_ fragments $ \fragment ->
                    unless (fragment `isInfixOf` message) $
                        expectationFailure $ unlines
                            [ "Type error message did not contain: "
                                <> show fragment
                            , "Type error message:"
                            , message
                            ]
