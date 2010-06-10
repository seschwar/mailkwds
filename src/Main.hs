{-# LANGUAGE BangPatterns #-}

-- |
-- Module:     Main
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Main where

import Control.Monad (when)
import Data.List (nub)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Args (Config(..), parseArgs)
import XLabel (rewriteMsg, toHeaders)
import qualified ByteString as B

-- | Rewrites the X-Label header fields from a message read from stdin to
-- stdout.
main :: IO ()
main = do
    args <- getArgs
    let !config = case parseArgs args of
                       Left  msg -> error msg
                       Right cfg -> cfg
    when (help config) (putStr helpMessage >> exitSuccess)
    when (version config) (putStr versionMessage >> exitSuccess)
    let f = toHeaders (output config) . (command config $ labels config) . nub
    B.interact $ B.unlines . rewriteMsg (input config) f (catenate config) . B.lines

helpMessage:: String
helpMessage = unlines $
    [ "Usage: x-label [OPTION]... [--] [COMMAND [LABEL]...]"
    , "Change the set of labels in the specified headers."
    , ""
    , "Options:"
    , "    -c, --catenate  catenate folded headers"
    , "    -h, --help      print usage information"
    , "    -f HEADER SEP, --from HEADER SEP, -i HEADER SEP, --input HEADER SEP"
    , "                    read labels listed in HEADER separated by SEP"
    , "    -o HEADER SEP, --output HEADER SEP, -t HEADER SEP, --to HEADER SEP"
    , "                    print HEADER with a list of labels separated by SEP"
    , "    -v, --version   print version information"
    , "Commands:"
    , "    add             add LABELs"
    , "    clear           remove all labels"
    , "    remove          remove LABELs"
    , "    set             replace with LABELs"
    , "    tidy            remove duplicated labels"
    ]

versionNumber :: String
versionNumber = "0.0"

versionMessage :: String
versionMessage = unlines $
    [ "x-label " ++ versionNumber
    , "Copyright (C) 2009-2010, Sebastian Schwarz"
    , "This is free software licensed under the ISC license."
    , "There is absolutely no warranty."
    ]

