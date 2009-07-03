-- |
-- Module:     Main
-- Copyright:  Copyright (c) 2009 Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Main where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (intercalate, stripPrefix, union, (\\))
import System.Environment (getArgs)

-- | A message 'Label' is a 'String'.
type Label = String

-- | Rewrites the X-Label header fields from a message read from stdin to
-- stdout.
main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "No command specified.")
    interact $ unlines . f (operator (head args) (tail args)) . break (== []) . lines
        where f op (h, b) = rewrite op "" h ++ b

-- | Chooses the operator to apply to the 'Label's in the mail and the ones
-- specified as command line arguments.
operator :: Eq a => String -> [a] -> [a] -> [a]
operator "add"    a b = a `union` b
operator "clear"  _ _ = []
operator "remove" a b = b \\ a
operator "set"    a _ = a
operator "tidy"   _ b = b
operator s        _ _ = error $ "Invalid command: " ++ s

-- | Appends 'Strings' beginning with a whitespace character to the
-- previous 'String' in the list.
unfldHdr :: [String] -> [String]
unfldHdr = foldr f []
    where
        f x (x'@(y:_):xs) | isSpace y = (x ++ x'):xs
        f x xs                        = x:xs

-- | Rewrites the given message header by applying the given function to the
-- existing X-Label header fields.  The resulting new header field will be
-- inserted at the end.
rewrite :: ([Label] -> [Label]) -> String -> [String] -> [String]
rewrite f acc []        = case f $ hdr2lst acc of
                               [] -> []
                               ls -> ("X-Label: " ++ lst2hdr ls) : []
rewrite f acc (s:ss)    = case stripPrefix "X-Label:" s of
                               Nothing -> s : rewrite f acc ss
                               Just s' -> rewrite1 f (acc ++ " " ++ s') ss

-- | Unfold the header field body.
rewrite1 :: ([Label] -> [Label]) -> String -> [String] -> [String]
rewrite1 f acc (s@(c:_):ss) | isSpace c = rewrite1 f (acc ++ s) ss
rewrite1 f acc ss           = rewrite f acc ss

-- | Parses the 'String' of a comma separated header field body to a list.
hdr2lst :: String -> [Label]
hdr2lst = words

-- | Formats a list of 'Label's so that they can be included as the body of a
-- header field.
lst2hdr :: [Label] -> String
lst2hdr = intercalate " "

