-- |
-- Module:     Main
-- Copyright:  Copyright (c) 2009-2014, Sebastian Schwarz <seschwar@gmail.com>
-- License:    MIT
-- Maintainer: Sebastian Schwarz <seschwar@gmail.com>
--

{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad      (when)
import Data.List          (nub)
import System.Environment (getArgs)
import System.Exit        (exitSuccess)

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Version               (showVersion)

import           Args           (Config (..), parseArgs)
import           MailKwds       (rewriteMsg, toHeaders)
import qualified Paths_mailkwds as P

-- | Rewrites the keyword header fields from a message read from stdin to
-- stdout.
main :: IO ()
main = do
    args <- getArgs
    let !config = case parseArgs args of
                       Left  msg -> error msg
                       Right cfg -> cfg
    when (help config) (putStr helpMessage >> exitSuccess)
    when (version config) (putStr versionMessage >> exitSuccess)
    let f = toHeaders (output config) . (command config $ keywords config) . nub
    B.interact $ B.unlines . rewriteMsg (input config) f (catenate config) . B.lines

helpMessage:: String
helpMessage = unlines $
    [ "Usage: mailkwds [OPTION]... [--] [COMMAND [KEYWORD]...]"
    , "Change the set of keywords in the specified headers."
    , ""
    , "Options:"
    , "    -c, --catenate  catenate folded headers"
    , "    -h, --help      print usage information"
    , "    -f HEADER SEP, --from HEADER SEP, -i HEADER SEP, --input HEADER SEP"
    , "                    read keywords listed in HEADER separated by SEP"
    , "    -o HEADER SEP, --output HEADER SEP, -t HEADER SEP, --to HEADER SEP"
    , "                    print HEADER with a list of keywords separated by SEP"
    , "    -v, --version   print version information"
    , "Commands:"
    , "    add             add KEYWORDs"
    , "    clear           remove all keywords"
    , "    remove          remove KEYWORDs"
    , "    set             replace with KEYWORDs"
    , "    tidy            remove duplicated keywords"
    ]

versionMessage :: String
versionMessage = unlines $
    [ "mailkwds " ++ showVersion P.version
    , "Copyright (C) 2009-2014, Sebastian Schwarz"
    , "This is free software licensed under the MIT/X11 license."
    , "There is absolutely no warranty."
    ]
