-- |
-- Module:     Utils
-- Copyright:  Copyright (c) 2009 Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Utils where

import Data.Char (isSpace)

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

