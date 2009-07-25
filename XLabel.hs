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
import Data.List.Split (keepDelimsL, split, whenElt)
import System.Environment (getArgs)
import Utils (appendWhile)

-- | A message 'Label' is a 'String'.
type Label = String

-- | Rewrites the X-Label header fields from a message read from stdin to
-- stdout.
main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "No command specified.")
    interact $ unlines . rewriteMsg (operator (head args) (tail args)) . lines

-- | Chooses the operator to apply to the 'Label's in the mail and the ones
-- specified as command line arguments.
operator :: Eq a => String -> [a] -> [a] -> [a]
operator "add"    a b = a `union` b
operator "clear"  _ _ = []
operator "remove" a b = b \\ a
operator "set"    a _ = a
operator "tidy"   _ b = b
operator s        _ _ = error $ "Invalid command: " ++ s

-- | Rewrites an email message consisting of a tuple heades and body.
rewriteMsg :: ([Label] -> [Label]) -> [String] -> [String]
rewriteMsg f msg = let (h, b) = break (== []) msg
                   in (fldHdr . rewriteHdr f "" . unfldHdr) h ++ b

-- | Appends 'String's beginning with a whitespace character to the
-- previous 'String' in the list.
unfldHdr :: [String] -> [String]
unfldHdr = appendWhile $ const $ isSpace . head

-- | Rewrites the given message header by applying the given function to the
-- existing X-Label header fields.  The resulting new header field will be
-- inserted at the end.
rewriteHdr :: ([Label] -> [Label]) -> String -> [String] -> [String]
rewriteHdr f acc []     = case f $ toLabels " " acc of
                               [] -> []
                               ls -> ["X-Label: " ++ toHeader " " ls]
rewriteHdr f acc (s:ss) = case stripPrefix "X-Label:" s of
                               Nothing -> s : rewriteHdr f acc ss
                               Just s' -> rewriteHdr f (acc ++ " " ++ s') ss

-- | Parses the 'String' of a comma separated header field body to a list.
toLabels :: String -> String -> [Label]
toLabels = split . dropBlanks . dropDelims . onSublist

-- | Formats a list of 'Label's so that they can be included as the body of a
-- header field.
toHeader :: String -> [Label] -> String
toHeader = intercalate

-- | Folds headers longer than 78 character in multiple lines.
fldHdr :: [String] -> [String]
fldHdr = concatMap $ appendWhile f . (split . keepDelimsL . whenElt $ isSpace)
    where f x y = length x + length y <= 78

