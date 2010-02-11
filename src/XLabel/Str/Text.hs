{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:     XLabel.Str.Text
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--


module XLabel.Str.Text
    ( module Data.Text.Lazy
    , module Data.Text.Lazy.IO
    , Str
    , (++)
    , break
    , breakOn
    , splitOn
    ) where

import Data.Bool (Bool)
import Data.Char (Char)
import Data.Text.Lazy hiding (break, center, justifyLeft, justifyRight, split)
import Data.Text.Lazy.IO
import qualified Data.Text.Lazy as T

type Str = Text

(++) :: Text -> Text -> Text
(++) = append

break :: (Char -> Bool) -> Text -> (Text, Text)
break = breakBy

breakOn :: Text -> Text -> (Text, Text)
breakOn = T.break

splitOn :: Text -> Text -> [Text]
splitOn = T.split

