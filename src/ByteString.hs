{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:     ByteString
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    MIT
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module ByteString
    ( module Data.ByteString.Lazy.Char8
    , (++)
    , toLower
    , toUpper
    , dropAround
    , dropWhileEnd
    , strip
    , stripStart
    , stripEnd
    , splitBy
    , splitOn
    , breakBy
    , breakOn
    , isSuffixOf
    , isInfixOf
    , hGetLine
    , hPutStr
    , hPutStrLn
    , getLine
    ) where

import Control.Monad ((>>))
import Data.Bool (Bool(..), not, otherwise)
import Data.ByteString.Lazy.Char8 hiding (cons', find, fromChunks, hGetNonBlocking, split, splitWith, toChunks)
import Data.Char (Char)
import Data.Eq ((==))
import Data.Function (($))
import Data.Function ((.))
import Data.Tuple (snd)
import Prelude (Num(..), undefined)
import System.IO (Handle, IO, stdin)
import Data.String (fromString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Char as C

(++) :: ByteString -> ByteString -> ByteString
(++) = append

toLower :: ByteString -> ByteString
toLower = map C.toLower

toUpper :: ByteString -> ByteString
toUpper = map C.toUpper

dropAround :: (Char -> Bool) -> ByteString -> ByteString
dropAround f = dropWhileEnd f . dropWhile f

dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd f = reverse . dropWhile f . reverse

strip :: ByteString -> ByteString
strip = dropAround C.isSpace

stripStart :: ByteString -> ByteString
stripStart = dropWhile C.isSpace

stripEnd :: ByteString -> ByteString
stripEnd = dropWhileEnd C.isSpace

splitBy :: (Char -> Bool) -> ByteString -> [ByteString]
splitBy = B.splitWith

-- | Adapted from the @tokenise@ example of 'Data.ByteString.breakSubstring'
splitOn :: ByteString -> ByteString -> [ByteString]
splitOn x y = let (h, t) = breakOn x y
              in  h : if null t
                         then []
                         else splitOn x (drop (length x) t)

breakBy :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakBy = break

-- | Lazy version of 'Data.ByteString.breakSubstring'
breakOn :: ByteString -> ByteString -> (ByteString, ByteString)
breakOn pat src = search 0 src
    where search n s | null s             = (src, empty)  -- not found
                     | pat `isPrefixOf` s = (take n src, s)
                     | otherwise          = search (n + 1) (tail s)

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y

isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf "" _  = True
isInfixOf _  "" = False
isInfixOf x  y  = not . null . snd . breakOn x $ y

hGetLine :: Handle -> IO ByteString
hGetLine = undefined

hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h s = hPut h s >> hPut h (singleton '\n')

getLine :: IO ByteString
getLine = hGetLine stdin

