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
import qualified XLabel.ByteString as S

-- | Rewrites an email message by using 'rewriteHdrs' on its unfolded header.
rewriteMsg :: Map ByteString ByteString -> ([ByteString] -> [Maybe ByteString]) -> ([ByteString] -> [ByteString])
              -> [ByteString] -> [ByteString]
rewriteMsg m f g msg = let (h, b) = break S.null msg
                       in  (g . rewriteHdrs m f . unfoldHeaders) h ++ b

-- | Appends a 'String' beginning with a whitespace character to the previous
-- 'String' in the list removing any superfluous intermediate whitespace
-- characters.
unfoldHeaders :: [ByteString] -> [ByteString]
unfoldHeaders = mconscat (const $ liftA2 (&&) (not . S.null) (isSpace . S.head))
                S.stripEnd (pad . S.stripStart)
    where pad "" = ""
          pad x  = S.cons ' ' x

-- | Folds headers longer than 78 character in multiple lines.
foldHeaders :: [ByteString] -> [ByteString]
foldHeaders = concatMap $ mconscat f id id . pad . S.words
    where pad []     = []
          pad (x:xs) = x : map (S.cons ' ') xs
          f x y = S.length x + S.length y <= 78

-- | Rewrite the headers of a message by appending the extraced and modified
-- 'Label's if they are not empty.
rewriteHdrs :: Map ByteString ByteString -> ([ByteString] -> [Maybe ByteString]) -> [ByteString] -> [ByteString]
rewriteHdrs m f hs = let (hs', ls) = runWriter $ mapM (extractLabels m) hs
                     in  catMaybes $ hs' ++ f ls

-- | Extracts the 'Label's of a single header by 'tell'ing them to a 'Writer'
-- 'Monad' and dropping them from the message by replacing them with 'Nothing'.
extractLabels :: Map ByteString ByteString -> ByteString -> Writer [ByteString] (Maybe ByteString)
extractLabels m h = let (n, b) = S.break (== ':') h
                    in  if ":" `S.isPrefixOf` b
                           then case lookup (S.toLower n) m of
                                     Nothing  -> return $ Just h
                                     Just sep -> tell (toLabels sep $ S.tail b)
                                                 >> return Nothing
                           else return $ Just h

-- | Splits the body of the given header field on the given substring into
-- 'Label's.
toLabels :: ByteString -> ByteString -> [ByteString]
toLabels x = filter (not . S.null) . map S.strip . S.splitOn x

-- | Produces an email header field for all specified fields.
toHeaders :: [(ByteString, ByteString)] -> [ByteString] -> [Maybe ByteString]
toHeaders xs = applyEach $ map (uncurry toHeader) xs

-- | Produces an email header field of the given name and the 'Label's.
toHeader :: ByteString -> ByteString -> [ByteString] -> Maybe ByteString
toHeader _   _   [] = Nothing
toHeader hdr sep ls = Just $ hdr `S.append` ": " `S.append` S.intercalate sep ls

