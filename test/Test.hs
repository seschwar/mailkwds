-- |
-- Module:     Main
-- Copyright:  Copyright (c) 2009 Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Main where

import Data.List (all)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Utils
import XLabel

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "header folding"
        [ testProperty "fold1" prop_fold1
        , testProperty "fold2" prop_fold2
        , testProperty "fold3" prop_fold3
        , testProperty "fold4" prop_fold4
        ]
    ]

prop_fold1 x = all (not . null) x ==>
    (unfoldHeaders . unfoldHeaders $ x) == unfoldHeaders x
prop_fold2 x = all (not . null) x ==>
    (foldHeaders . foldHeaders $ x) == foldHeaders x
prop_fold3 x = all (not . null) x ==>
    (unfoldHeaders . foldHeaders . unfoldHeaders $ x) == unfoldHeaders x
prop_fold4 x = all (not . null) x ==>
    (unfoldHeaders . foldHeaders . unfoldHeaders . foldHeaders $ x)
    == (unfoldHeaders . foldHeaders $ x)

