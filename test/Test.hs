-- |
-- Module:     Main
-- Copyright:  Copyright (c) 2009 Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Main where

import Control.Applicative (liftA2)
import Data.Char (isSpace)
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
    [ testGroup "utils"
        [ testProperty "cat1" prop_cat1
        , testProperty "cat2" prop_cat1
        ]
    , testGroup "header folding"
        [ testProperty "fold1" prop_fold1
        , testProperty "fold2" prop_fold2
        , testProperty "fold3" prop_fold3
        , testProperty "fold4" prop_fold4
        , testProperty "fold5" prop_fold5
        , testProperty "fold6" prop_fold6
        ]
    ]

prop_cat1 xs = (not . null) (xs :: [[Int]]) ==>
    mconscat (const $ const True) id id xs == [concat xs]
prop_cat2 xs = mconscat (const $ const False) id id xs == xs

prop_fold1 xs = all (not . null) xs ==>
    (unfoldHeaders . unfoldHeaders $ xs) == unfoldHeaders xs
prop_fold2 xs = all (not . null) xs ==>
    (foldHeaders . foldHeaders $ xs) == foldHeaders xs
prop_fold3 xs = all (not . null) xs ==>
    (unfoldHeaders . foldHeaders . unfoldHeaders $ xs) == unfoldHeaders xs
prop_fold4 xs = all (not . null) xs ==>
    (unfoldHeaders . foldHeaders . unfoldHeaders . foldHeaders $ xs)
    == (unfoldHeaders . foldHeaders $ xs)
prop_fold5 xs = all f $ foldHeaders xs
    where f = liftA2 (||) ((<= 78) . length) (not . any isSpace . stripStart)
prop_fold6 xs = all (not . null) xs ==>
    all (not . isSpace) . map head $ unfoldHeaders xs

