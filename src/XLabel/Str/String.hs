{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:     XLabel.Str.String
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module XLabel.Str.String
    ( module System.IO
    , module XLabel.Lst.List
    , Str
    , String
    , pack
    , unpack
    , append
    , uncons
    , toLower
    , toUpper
    , strip
    , stripStart
    , stripEnd
    , splitBy
    , splitOn
    , breakBy
    , breakOn
    ) where

import Data.Bool (Bool)
import Data.Char (Char, String)
import Data.Function (id)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (Maybe(..))
import System.IO (readFile, writeFile, appendFile, hGetContents, hGetLine, hPutStr, hPutStrLn, interact, getContents, getLine, putStr, putStrLn)
import XLabel.Lst.List
import qualified Data.Char as C

type Str = String

pack :: [Char] -> String
pack = id

unpack :: String -> [Char]
unpack = id

append :: String -> String -> String
append = (++)

uncons :: String -> Maybe (Char, String)
uncons []     = Nothing
uncons (x:xs) = Just (x:xs)

toLower :: String -> String
toLower = map C.toLower

toUpper :: String -> String
toUpper = map C.toUpper

strip :: String -> String
strip = dropAround C.isSpace

stripStart :: String -> String
stripStart = dropWhile C.isSpace

stripEnd :: String -> String
stripEnd = dropWhile C.isSpace

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy = splitWhen

breakBy :: (String -> Bool) -> String -> (String, String)
breakBy = break

breakOn :: String -> String -> (String, String)
breakOn = undefined

