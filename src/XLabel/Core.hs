-- |
-- Module:     XLabel.Core
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module XLabel.Core where

import Control.Applicative (liftA2)
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.Char (isSpace)
import Data.Map (Map, lookup)
import Data.Maybe (catMaybes)
import Prelude hiding (lookup)
import XLabel.ByteString (ByteString)
import XLabel.Utils (applyEach, mconscat)
import qualified XLabel.ByteString as B

-- | Rewrites an email message by using 'rewriteHdrs' on its unfolded header.
rewriteMsg :: Map ByteString ByteString -> ([ByteString] -> [Maybe ByteString]) -> ([ByteString] -> [ByteString])
              -> [ByteString] -> [ByteString]
rewriteMsg m f g msg = let (h, b) = break B.null msg
                       in  (g . rewriteHdrs m f . unfoldHeaders) h ++ b

-- | Appends a 'String' beginning with a whitespace character to the previous
-- 'String' in the list removing any superfluous intermediate whitespace
-- characters.
unfoldHeaders :: [ByteString] -> [ByteString]
unfoldHeaders = mconscat (const $ liftA2 (&&) (not . B.null) (isSpace . B.head))
                B.stripEnd (pad . B.stripStart)
    where pad "" = ""
          pad x  = B.cons ' ' x

-- | Folds headers longer than 78 character in multiple lines.
foldHeaders :: [ByteString] -> [ByteString]
foldHeaders = concatMap $ mconscat f id id . pad . B.words
    where pad []     = []
          pad (x:xs) = x : map (B.cons ' ') xs
          f x y = B.length x + B.length y <= 78

-- | Rewrite the headers of a message by appending the extraced and modified
-- 'Label's if they are not empty.
rewriteHdrs :: Map ByteString ByteString -> ([ByteString] -> [Maybe ByteString]) -> [ByteString] -> [ByteString]
rewriteHdrs m f hs = let (hs', ls) = runWriter $ mapM (extractLabels m) hs
                     in  catMaybes $ hs' ++ f ls

-- | Extracts the 'Label's of a single header by 'tell'ing them to a 'Writer'
-- 'Monad' and dropping them from the message by replacing them with 'Nothing'.
extractLabels :: Map ByteString ByteString -> ByteString -> Writer [ByteString] (Maybe ByteString)
extractLabels m h = let (n, b) = B.break (== ':') h
                    in  if ":" `B.isPrefixOf` b
                           then case lookup (B.toLower n) m of
                                     Nothing  -> return $ Just h
                                     Just sep -> tell (toLabels sep $ B.tail b)
                                                 >> return Nothing
                           else return $ Just h

-- | Splits the body of the given header field on the given substring into
-- 'Label's.
toLabels :: ByteString -> ByteString -> [ByteString]
toLabels x = filter (not . B.null) . map B.strip . B.splitOn x

-- | Produces an email header field for all specified fields.
toHeaders :: [(ByteString, ByteString)] -> [ByteString] -> [Maybe ByteString]
toHeaders xs = applyEach $ map (uncurry toHeader) xs

-- | Produces an email header field of the given name and the 'Label's.
toHeader :: ByteString -> ByteString -> [ByteString] -> Maybe ByteString
toHeader _   _   [] = Nothing
toHeader hdr sep ls = Just $ hdr `B.append` ": " `B.append` B.intercalate sep ls

