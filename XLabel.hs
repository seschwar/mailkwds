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
    interact $ unlines . rewrite (operator (head args) (tail args)) "" . lines

-- | Chooses the operator to apply to the 'Label's in the mail and the ones
-- specified as command line arguments.
operator :: Eq a => String -> [a] -> [a] -> [a]
operator "add"    a b = a `union` b
operator "clear"  _ _ = []
operator "remove" a b = b \\ a
operator "set"    a _ = a
operator "tidy"   _ b = b
operator s        _ _ = error $ "Invalid command: " ++ s

-- | Rewrites the given message by applying the given function to the existing
-- X-Label header fields.  The resulting new header field will be inserted at
-- the end of the header section right before the beginning of the message body.
rewrite :: ([Label] -> [Label]) -> String -> [String] -> [String]
rewrite f acc []        = rewrite2 f acc []
rewrite f acc ss@("":_) = rewrite2 f acc ss
rewrite f acc (s:ss)    = case stripPrefix "X-Label:" s of
                               Nothing -> s : rewrite f acc ss
                               Just s' -> rewrite1 f (acc ++ ", " ++ s') ss

-- | Unfold the header field body.
rewrite1 :: ([Label] -> [Label]) -> String -> [String] -> [String]
rewrite1 f acc (s@(c:_):ss) | isSpace c = rewrite1 f (acc ++ s) ss
rewrite1 f acc ss           = rewrite f acc ss

-- | Return X-Label header if it is not empty and the rest of the 'String's.
rewrite2 :: ([Label] -> [Label]) -> String -> [String] -> [String]
rewrite2 f acc ss = case f $ hdr2lst acc of
                         [] -> []
                         ls -> ("X-Label: " ++ lst2hdr ls) : ss

-- | Parses the 'String' of a comma separated header field body to a list.
hdr2lst :: String -> [Label]
hdr2lst = filter (not . null) . map strip . split (== ',')

-- | Formats a list of 'Label's so that they can be included as the body of a
-- header field.
lst2hdr :: [Label] -> String
lst2hdr = intercalate ", "

-- | Splits a list at every position where the given predicate is true dropping
-- the delimiting element.
split :: (a -> Bool) -> [a] -> [[a]]
split prd xs = case dropWhile prd xs of
                    []  -> []
                    xs' -> x : split prd xs''
                        where (x, xs'') = break prd xs'

-- | Strips all whitespace from both the left and the right hand side of the
-- given 'String'.
strip :: String -> String
strip = rstrip . lstrip

-- | Strips all whitespace from the left side of the given 'String'.
lstrip :: String -> String
lstrip = dropWhile isSpace

-- | Strips all whitespace from the right side of the given 'String'.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

