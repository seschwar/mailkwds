{-# LANGUAGE BangPatterns #-}

-- |
-- Module:     Main
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Main where

import Data.List (nub)
import System.Environment (getArgs)
import XLabel.Core (rewriteMsg, toHeaders)
import XLabel.Args (Config(..), parseArgs)

-- | Rewrites the X-Label header fields from a message read from stdin to
-- stdout.
main :: IO ()
main = do
    args <- getArgs
    let !config = case parseArgs args of
                       Left  msg -> error msg
                       Right cfg -> cfg
    let f = toHeaders (output config) . (command config $ labels config) . nub
    interact $ unlines . rewriteMsg (input config) f . lines

