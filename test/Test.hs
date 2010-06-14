{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module:     Main
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    MIT
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Main where

import Args
import ByteString (ByteString)
import Control.Applicative (liftA2)
import Control.Arrow (first, (***))
import Data.Char (isSpace)
import Data.List (nub)
import MailKwds
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Utils
import qualified ByteString as B
import qualified Data.Map as M

import System.IO.Unsafe (unsafePerformIO)
import System.IO (hPutStrLn, stderr)

debug :: Show a => String -> a -> a
debug x y = unsafePerformIO $ do
    hPutStrLn stderr $ x ++ ": " ++ show y
    return y

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "argument parsing"
        [ testProperty "input order"    prop_args1
        , testProperty "output order"   prop_args2
        , testProperty "keyword order"  prop_args3
        ]
    , testGroup "utils"
        [ testProperty "catenate all"  prop_cat1
        , testProperty "catenate none" prop_cat2
        ]
    , testGroup "header folding"
        [ testProperty "unfold twice"       prop_fold1
--      , testProperty "fold twice"         prop_fold2
        , testProperty "ufu"                prop_fold3
        , testProperty "fuf"                prop_fold4
        , testProperty "folded or no space" prop_fold5
        , testProperty "fold6"              prop_fold6
        ]
    ]

instance Arbitrary ByteString where
    arbitrary = fmap B.pack arbitrary
    shrink    = fmap B.pack . shrink . B.unpack

prop_args1 xs = (not . null) xs ==>
    case parseArgs $ (xs >>= \(x, y) -> ["-i", x, y]) ++ ["--", "tidy"] of
         Left  _   -> False
         Right cfg -> input cfg == (M.fromList . map (first B.toLower) . map (B.pack *** B.pack) $ xs)
prop_args2 xs = (not . null) xs ==>
    case parseArgs $ (xs >>= \(x, y) -> ["-o", x, y]) ++ ["--", "tidy"] of
         Left  _   -> False
         Right cfg -> output cfg  == (fmap (B.pack *** B.pack) $ nub xs)
prop_args3 xs = case parseArgs $ ["--", "tidy"] ++ xs of
                     Left  _   -> False
                     Right cfg -> keywords cfg == (fmap B.pack $ nub xs)

prop_cat1 :: [[Int]] -> Property
prop_cat1 xs = (not . null) xs ==>
    mconscat (const $ const True) id id xs == [concat xs]

prop_cat2 :: [[Int]] -> Bool
prop_cat2 xs = mconscat (const $ const False) id id xs == xs

prop_fold1 xs = all (not . B.null) xs ==>
    (unfoldHeaders . unfoldHeaders $ xs) == unfoldHeaders xs
prop_fold2 xs = all (not . B.null) xs ==>
    (foldHeaders . foldHeaders $ xs) == foldHeaders xs
prop_fold3 xs = all (not . B.null) xs ==>
    (unfoldHeaders . foldHeaders . unfoldHeaders $ xs) == unfoldHeaders xs
prop_fold4 xs = all (not . B.null) xs ==>
    (unfoldHeaders . foldHeaders . unfoldHeaders . foldHeaders $ xs)
    == (unfoldHeaders . foldHeaders $ xs)
prop_fold5 xs =  all (not . B.null) xs ==>
    all f $ foldHeaders xs
  where
    f = liftA2 (||) ((<= 78) . B.length) (not . B.any isSpace . B.stripStart)
prop_fold6 xs = all (not . B.null) xs ==>
    all (not . isSpace) . map B.head $ unfoldHeaders xs

