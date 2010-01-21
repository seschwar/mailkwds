-- |
-- Module:     XLabel.Core
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module XLabel.Core where

import Control.Applicative (liftA2)
import Control.Monad (mapM)
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import Data.List.Split (dropBlanks, dropDelims, keepDelimsL, onSublist, split,
                        whenElt)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), catMaybes)
import XLabel.Utils (mconscat, strip, stripStart, stripEnd)

-- | A message 'Label' is just a 'String'.
type Label = String

-- | Rewrites an email message by using 'rewriteHdrs' on its unfolded header.
rewriteMsg :: Map String String -> ([Label] -> Maybe String) -> [String]
              -> [String]
rewriteMsg m f msg = let (h, b) = break null msg
                     in  (foldHeaders . rewriteHdrs m f . unfoldHeaders) h ++ b

-- | Appends a 'String' beginning with a whitespace character to the previous
-- 'String' in the list removing any superfluous intermediate whitespace
-- characters.
unfoldHeaders :: [String] -> [String]
unfoldHeaders = mconscat (const $ liftA2 (&&) (not . null) (isSpace . head))
                stripEnd (pad . stripStart)
    where pad "" = ""
          pad x  = ' ':x

-- | Folds headers longer than 78 character in multiple lines.
foldHeaders :: [String] -> [String]
foldHeaders = concatMap $ mconscat f id id
              . (split . keepDelimsL . whenElt $ isSpace)
    where f x y = length x + length y <= 78

-- | Rewrite the headers of a message by appending the extraced and modified
-- 'Label's if they are not empty.
rewriteHdrs :: Map String String -> ([Label] -> Maybe String) -> [String]
               -> [String]
rewriteHdrs m f hs = let (hs', ls) = runWriter $ mapM (extractLabels m) hs
                     in  catMaybes $ hs' ++ [f ls]

-- | Extracts the 'Label's of a single header by 'tell'ing them to a 'Writer'
-- 'Monad' and dropping them from the message by replacing them with 'Nothing'.
extractLabels :: Map String String -> String -> Writer [Label] (Maybe String)
extractLabels m h = case break (== ':') h of
                         (n, ':':b) -> case Data.Map.lookup (map toLower n) m of
                                            Nothing  -> return $ Just h
                                            Just sep -> tell (toLabels sep b)
                                                        >> return Nothing
                         _          -> return $ Just h

-- | Splits the body of the given header field on the given substring into
-- 'Label's.
toLabels :: String -> String -> [Label]
toLabels x = filter (not . null) . map strip
             . (split . dropBlanks . dropDelims . onSublist $ x)

-- | Produces an email header field of the given name and the 'Label's.
toHeader :: String -> String -> [Label] -> Maybe String
toHeader _   _   [] = Nothing
toHeader hdr sep ls = Just $ hdr ++ ": " ++ intercalate sep ls

