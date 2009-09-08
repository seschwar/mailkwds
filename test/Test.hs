-- |
-- Module:     Main
-- Copyright:  Copyright (c) 2009 Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = []

