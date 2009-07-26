-- |
-- Module:     XLabel
-- Copyright:  Copyright (c) 2009 Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module XLabel where

import Control.Monad (mapM)
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (dropBlanks, dropDelims, keepDelimsL, onSublist, split,
                        whenElt)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), catMaybes)
import Utils (catWhile)

-- | A message 'Label' is a 'String'.
type Label = String

-- | Rewrites an email message consisting of a tuple heades and body.
rewriteMsg :: Map String String -> ([Label] -> [Label]) -> [String] -> [String]
rewriteMsg m f msg = let (h, b) = break (== []) msg
                     in (fldHdr . rewriteHdrs m f . unfldHdr) h ++ b

-- | Appends 'String's beginning with a whitespace character to the
-- previous 'String' in the list.
unfldHdr :: [String] -> [String]
unfldHdr = catWhile $ const $ isSpace . head

-- | Rewrite the headers of a message by appending the extraced 'Label's if they
-- are not empty.
rewriteHdrs :: Map String String -> ([Label] -> [Label]) -> [String] -> [String]
rewriteHdrs m f hs = let (hs', ls) = runWriter $ mapM (extractLabels m) hs
                     in case f ls of
                             []  -> catMaybes hs'
                             ls' -> catMaybes hs'
                                    ++ ["X-Label: " ++ toHeader " " ls']

-- | Extracts the 'Label's of a single header by 'tell'ing them to a 'Writer'
-- 'Monad' and dropping them from the message by replacing them with 'Nothing'.
extractLabels :: Map String String -> String -> Writer [Label] (Maybe String)
extractLabels m h = case break (== ':') h of
                         (n, ':':b) -> case Data.Map.lookup n m of
                                            Nothing  -> return $ Just h
                                            Just sep -> tell (toLabels sep b)
                                                        >> return Nothing
                         _          -> return $ Just h

-- | Parses the 'String' of a comma separated header field body to a list.
toLabels :: String -> String -> [Label]
toLabels = split . dropBlanks . dropDelims . onSublist

-- | Formats a list of 'Label's so that they can be included as the body of a
-- header field.
toHeader :: String -> [Label] -> String
toHeader = intercalate

-- | Folds headers longer than 78 character in multiple lines.
fldHdr :: [String] -> [String]
fldHdr = concatMap $ catWhile f . (split . keepDelimsL . whenElt $ isSpace)
    where f x y = length x + length y <= 78

