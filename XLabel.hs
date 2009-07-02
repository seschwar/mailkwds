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

-- | Splits a list at every position where @prd@ is 'True' dropping that
-- element.
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn prd xs = case dropWhile prd xs of
                    []  -> []
                    xs' -> x : splitOn prd xs''
                        where (x, xs'') = break prd xs'

-- | Splits a 'String' on all whitespace, like 'words'
split :: String -> [String]
split = splitOn isSpace

-- | Strips all elements for which @prd@ is 'True' from the beginning and the
-- end of a list.
stripWhile :: (a -> Bool) -> [a] -> [a]
stripWhile prd = rstripWhile prd . lstripWhile prd

-- | Strips all elements for which @prd@ is 'True' from the beginning of a list.
lstripWhile :: (a -> Bool) -> [a] -> [a]
lstripWhile = dropWhile

-- | Strips all elements for which @prd@ is 'True' from the end of a list.
rstripWhile :: (a -> Bool) -> [a] -> [a]
rstripWhile prd = reverse . lstripWhile prd . reverse

-- | Strips all whitespace from both the left and the right hand side of the
-- given 'String'.
strip :: String -> String
strip = stripWhile isSpace

-- | Strips all whitespace from the left side of the given 'String'.
lstrip :: String -> String
lstrip = lstripWhile isSpace

-- | Strips all whitespace from the right side of the given 'String'.
rstrip :: String -> String
rstrip = rstripWhile isSpace

