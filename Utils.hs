-- |
-- Module:     Utils
-- Copyright:  Copyright (c) 2009 Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Utils where

import Data.Char (isSpace)

-- | Appends an element in a 'List' to the previous one if @prd@ is 'True'.
appendWhile :: ([a] -> [a] -> Bool) -> [[a]] -> [[a]]
appendWhile _   []                   = []
appendWhile _   [x]                  = [x]
appendWhile prd (x:x':xs) | prd x x' = appendWhile prd $ (x ++ x') : xs
appendWhile prd (x:x':xs)            = x : appendWhile prd (x':xs)

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

