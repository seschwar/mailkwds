-- |
-- Module:     MailKwds
-- Copyright:  Copyright (c) 2009-2014, Sebastian Schwarz <seschwar@gmail.com>
-- License:    MIT
-- Maintainer: Sebastian Schwarz <seschwar@gmail.com>
--

{-# LANGUAGE OverloadedStrings #-}

module MailKwds where

import Control.Applicative (liftA2)
import Data.Char           (isSpace, toLower)
import Data.Maybe          (catMaybes)
import Prelude             hiding (lookup)

import           Control.Monad.Trans.Writer.Lazy (Writer, runWriter, tell)
import           Data.ByteString.Lazy.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8      as B
import           Data.ByteString.Lazy.Search     (split, strictify)
import           Data.Map                        (Map, lookup)

import Utils (applyEach, mconscat)

stripStart :: ByteString -> ByteString
stripStart = B.dropWhile isSpace

stripEnd :: ByteString -> ByteString
stripEnd = B.reverse . B.dropWhile isSpace . B.reverse

-- | Rewrites an email message by using 'rewriteHdrs' on its unfolded header.
rewriteMsg :: Map ByteString ByteString
              -> ([ByteString] -> [Maybe ByteString])
              -> ([ByteString] -> [ByteString])
              -> [ByteString] -> [ByteString]
rewriteMsg m f g msg = let (h, b) = break B.null msg
                       in  (g . rewriteHdr m f . unfoldHeaders) h ++ b

-- | Appends a 'String' beginning with a whitespace character to the previous
-- 'String' in the list removing any superfluous intermediate whitespace
-- characters.
unfoldHeaders :: [ByteString] -> [ByteString]
unfoldHeaders = mconscat (const $ liftA2 (&&) (not . B.null) (isSpace . B.head))
                stripEnd (pad . stripStart)
    where pad "" = ""
          pad x  = B.cons ' ' x

-- | Folds header fields longer than 78 character in multiple lines.
foldHeaders :: [ByteString] -> [ByteString]
foldHeaders = concatMap $ mconscat f id id . pad . B.words
    where pad []     = []
          pad (x:xs) = x : map (B.cons ' ') xs
          f x y = B.length x + B.length y <= 78

-- | Rewrite the header of a message by appending the extraced and modified
-- keywords if they are not empty.
rewriteHdr :: Map ByteString ByteString
              -> ([ByteString] -> [Maybe ByteString])
              -> [ByteString] -> [ByteString]
rewriteHdr m f hs = let (hs', ls) = runWriter $ mapM (extractKeywords m) hs
                    in  catMaybes $ hs' ++ f ls

-- | Extracts the keywords of a single header field by 'tell'ing them to a
-- 'Writer' 'Monad' and dropping them from the message by replacing them with
-- 'Nothing'.
extractKeywords :: Map ByteString ByteString
                   -> ByteString
                   -> Writer [ByteString] (Maybe ByteString)
extractKeywords m h = let (n, b) = B.break (== ':') h
                      in  if ":" `B.isPrefixOf` b
                             then case lookup (B.map toLower n) m of
                                       Nothing  -> return $ Just h
                                       Just sep -> tell (toKeywords sep $ B.tail b)
                                                   >> return Nothing
                             else return $ Just h

-- | Splits the body of the given header field on the given substring into
-- keywords.
toKeywords :: ByteString -> ByteString -> [ByteString]
toKeywords x = filter (not . B.null)
               . map (stripEnd . stripStart)
               . split (strictify x)

-- | Produces an email header field for all specified fields.
toHeaders :: [(ByteString, ByteString)] -> [ByteString] -> [Maybe ByteString]
toHeaders = applyEach . map (uncurry toHeader)

-- | Produces an email header field of the given name and the 'Keyword's.
toHeader :: ByteString -> ByteString -> [ByteString] -> Maybe ByteString
toHeader _   _   [] = Nothing
toHeader hdr sep ls = Just $ hdr `B.append` ": " `B.append` B.intercalate sep ls
